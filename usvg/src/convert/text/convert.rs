// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cmp;
use std::rc::Rc;

use crate::{fontdb, tree, utils};
use crate::convert::{prelude::*, style, units};
use super::TextNode;


/// A read-only text index in bytes.
///
/// Guarantee to be on a char boundary and in text bounds.
#[derive(Clone, Copy, PartialEq)]
pub struct ByteIndex(usize);

impl ByteIndex {
    pub fn new(i: usize) -> Self {
        ByteIndex(i)
    }

    pub fn value(&self) -> usize {
        self.0
    }

    /// Converts byte position into a code point position.
    pub fn code_point_at(&self, text: &str) -> usize {
        text.char_indices().take_while(|(i, _)| *i != self.0).count()
    }

    /// Converts byte position into a character.
    pub fn char_from(&self, text: &str) -> char {
        text[self.0..].chars().next().unwrap()
    }
}


#[derive(Clone, Copy, PartialEq)]
pub enum TextAnchor {
    Start,
    Middle,
    End,
}


pub struct TextPath {
    /// A text offset in SVG coordinates.
    ///
    /// Percentage values already resolved.
    pub start_offset: f64,

    pub segments: Vec<tree::PathSegment>,
}


#[derive(Clone)]
pub enum TextFlow {
    Horizontal,
    Path(Rc<TextPath>),
}


/// A text chunk.
///
/// Text alignment and BIDI reordering can be done only inside a text chunk.
pub struct TextChunk {
    pub x: Option<f64>,
    pub y: Option<f64>,
    pub anchor: TextAnchor,
    pub spans: Vec<TextSpan>,
    pub text_flow: TextFlow,
    pub text: String,
}

impl TextChunk {
    pub fn span_at(&self, byte_offset: ByteIndex) -> Option<&TextSpan> {
        for span in &self.spans {
            if span.contains(byte_offset) {
                return Some(span);
            }
        }

        None
    }
}


/// Spans do not overlap.
#[derive(Clone)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
    pub fill: Option<tree::Fill>,
    pub stroke: Option<tree::Stroke>,
    pub font: fontdb::Font,
    pub font_size: f64,
    pub decoration: TextDecoration,
    pub baseline_shift: f64,
    pub visibility: tree::Visibility,
    pub letter_spacing: f64,
    pub word_spacing: f64,
}

impl TextSpan {
    pub fn contains(&self, byte_offset: ByteIndex) -> bool {
        byte_offset.value() >= self.start && byte_offset.value() < self.end
    }
}


#[derive(Clone, Copy, PartialEq)]
pub enum WritingMode {
    LeftToRight,
    TopToBottom,
}


struct IterState {
    chars_count: usize,
    chunk_bytes_count: usize,
    split_chunk: bool,
    text_flow: TextFlow,
    chunks: Vec<TextChunk>,
}

pub fn collect_text_chunks(
    text_node: &TextNode,
    pos_list: &[CharacterPosition],
    state: &State,
    tree: &mut tree::Tree,
) -> Vec<TextChunk> {
    let mut iter_state = IterState {
        chars_count: 0,
        chunk_bytes_count: 0,
        split_chunk: false,
        text_flow: TextFlow::Horizontal,
        chunks: Vec::new(),
    };

    collect_text_chunks_impl(text_node, text_node, pos_list, state, tree, &mut iter_state);

    iter_state.chunks
}

fn collect_text_chunks_impl(
    text_node: &TextNode,
    parent: &svgdom::Node,
    pos_list: &[CharacterPosition],
    state: &State,
    tree: &mut tree::Tree,
    iter_state: &mut IterState,
) {
    for child in parent.children() {
        if child.is_element() {
            if child.is_tag_name(EId::TextPath) {
                if !parent.is_tag_name(EId::Text) {
                    // `textPath` can be set only as a direct `text` element child.
                    iter_state.chars_count += count_chars(&child);
                    continue;
                }

                match resolve_text_flow(child.clone(), state) {
                    Some(v) => {
                        iter_state.text_flow = v;
                    }
                    None => {
                        // Skip an invalid text path and all it's children.
                        // We have to update the chars count,
                        // because `pos_list` was calculated including this text path.
                        iter_state.chars_count += count_chars(&child);
                        continue;
                    }
                }

                iter_state.split_chunk = true;
            }

            collect_text_chunks_impl(text_node, &child, pos_list, state, tree, iter_state);

            iter_state.text_flow = TextFlow::Horizontal;

            // Next char after `textPath` should be split too.
            if child.is_tag_name(EId::TextPath) {
                iter_state.split_chunk = true;
            }

            continue;
        }

        if !style::is_visible_element(parent, state.opt) {
            iter_state.chars_count += child.text().chars().count();
            continue;
        }

        let anchor = conv_text_anchor(parent);

        // TODO: what to do when <= 0? UB?
        let font_size = units::resolve_font_size(parent, state);
        if !(font_size > 0.0) {
            // Skip this span.
            iter_state.chars_count += child.text().chars().count();
            continue;
        }

        let font = match resolve_font(parent, state) {
            Some(v) => v,
            None => {
                // Skip this span.
                iter_state.chars_count += child.text().chars().count();
                continue;
            }
        };

        let letter_spacing = parent.resolve_length(AId::LetterSpacing, state, 0.0);
        let word_spacing = parent.resolve_length(AId::WordSpacing, state, 0.0);

        let span = TextSpan {
            start: 0,
            end: 0,
            fill: style::resolve_fill(parent, true, state, tree),
            stroke: style::resolve_stroke(parent, true, state, tree),
            font,
            font_size,
            decoration: resolve_decoration(text_node, parent, state, tree),
            visibility: parent.find_enum(AId::Visibility),
            baseline_shift: resolve_baseline_shift(parent, state),
            letter_spacing,
            word_spacing,
        };

        let mut is_new_span = true;
        for c in child.text().chars() {
            let char_len = c.len_utf8();

            // Create a new chunk if:
            // - this is the first span (yes, position can be None)
            // - text character has an absolute coordinate assigned to it (via x/y attribute)
            // - `c` is the first char of the `textPath`
            // - `c` is the first char after `textPath`
            let is_new_chunk =
                   pos_list[iter_state.chars_count].x.is_some()
                || pos_list[iter_state.chars_count].y.is_some()
                || iter_state.split_chunk
                || iter_state.chunks.is_empty();

            iter_state.split_chunk = false;

            if is_new_chunk {
                iter_state.chunk_bytes_count = 0;

                let mut span2 = span.clone();
                span2.start = 0;
                span2.end = char_len;

                iter_state.chunks.push(TextChunk {
                    x: pos_list[iter_state.chars_count].x,
                    y: pos_list[iter_state.chars_count].y,
                    anchor,
                    spans: vec![span2],
                    text_flow: iter_state.text_flow.clone(),
                    text: c.to_string(),
                });
            } else if is_new_span {
                // Add this span to the last text chunk.
                let mut span2 = span.clone();
                span2.start = iter_state.chunk_bytes_count;
                span2.end = iter_state.chunk_bytes_count + char_len;

                if let Some(chunk) = iter_state.chunks.last_mut() {
                    chunk.text.push(c);
                    chunk.spans.push(span2);
                }
            } else {
                // Extend the last span.
                if let Some(chunk) = iter_state.chunks.last_mut() {
                    chunk.text.push(c);
                    if let Some(span) = chunk.spans.last_mut() {
                        debug_assert_ne!(span.end, 0);
                        span.end += char_len;
                    }
                }
            }

            is_new_span = false;
            iter_state.chars_count += 1;
            iter_state.chunk_bytes_count += char_len;
        }
    }
}

fn resolve_text_flow(
    node: svgdom::Node,
    state: &State,
) -> Option<TextFlow> {
    let av = node.attributes().get_value(AId::Href).cloned();
    let path_node = match av {
        Some(AValue::Link(n)) => n.clone(),
        _ => return None,
    };

    if !path_node.is_tag_name(EId::Path) {
        return None;
    }

    let segments = match path_node.attributes().get_value(AId::D).cloned() {
        Some(AValue::Path(path)) => crate::convert::path::convert(path),
        _ => return None,
    };

    if segments.len() < 2 {
        return None;
    }

    let start_offset = node.attributes().get_length_or(AId::StartOffset, Length::zero());
    let start_offset = if start_offset.unit == Unit::Percent {
        // 'If a percentage is given, then the `startOffset` represents
        // a percentage distance along the entire path.'
        let path_len = utils::path_length(&segments);
        path_len * (start_offset.num / 100.0)
    } else {
        node.resolve_length(AId::StartOffset, state, 0.0)
    };

    Some(TextFlow::Path(Rc::new(TextPath {
        start_offset,
        segments,
    })))
}

pub fn resolve_rendering_mode(
    text_node: &TextNode,
    state: &State,
) -> tree::ShapeRendering {
    let mode: tree::TextRendering = text_node
        .try_find_enum(AId::TextRendering)
        .unwrap_or(state.opt.text_rendering);

    match mode {
        tree::TextRendering::OptimizeSpeed => tree::ShapeRendering::CrispEdges,
        tree::TextRendering::OptimizeLegibility => tree::ShapeRendering::GeometricPrecision,
        tree::TextRendering::GeometricPrecision =>  tree::ShapeRendering::GeometricPrecision,
    }
}

fn resolve_font(
    node: &svgdom::Node,
    state: &State,
) -> Option<fontdb::Font> {
    let style = conv_font_style(node);
    let stretch = conv_font_stretch(node);
    let weight = resolve_font_weight(node);
    let properties = fontdb::Properties { style, weight, stretch };

    let font_family = if let Some(n) = node.find_node_with_attribute(AId::FontFamily) {
        n.attributes().get_str_or(AId::FontFamily, &state.opt.font_family).to_owned()
    } else {
        state.opt.font_family.to_owned()
    };

    let mut name_list = Vec::new();
    for family in font_family.split(',') {
        // TODO: to a proper parser
        let family = family.replace('\'', "");
        let family = family.trim();
        name_list.push(family.to_string());
    }

    // Use the default font as fallback.
    name_list.push(state.opt.font_family.clone());

    let name_list: Vec<_> = name_list.iter().map(|s| s.as_str()).collect();

    let mut db = state.db.borrow_mut();
    let id = match db.select_best_match(&name_list, properties) {
        Some(id) => id,
        None => {
            warn!("No match for '{}' font-family.", font_family);
            return None;
        }
    };

    db.load_font(id)
}

fn conv_font_style(
    node: &svgdom::Node,
) -> fontdb::Style {
    if let Some(n) = node.find_node_with_attribute(AId::FontStyle) {
        match n.attributes().get_str_or(AId::FontStyle, "") {
            "italic"  => fontdb::Style::Italic,
            "oblique" => fontdb::Style::Oblique,
            _         => fontdb::Style::Normal,
        }
    } else {
        fontdb::Style::Normal
    }
}

fn conv_font_stretch(
    node: &svgdom::Node,
) -> fontdb::Stretch {
    if let Some(n) = node.find_node_with_attribute(AId::FontStretch) {
        match n.attributes().get_str_or(AId::FontStretch, "") {
            "narrower" | "condensed" => fontdb::Stretch::Condensed,
            "ultra-condensed"        => fontdb::Stretch::UltraCondensed,
            "extra-condensed"        => fontdb::Stretch::ExtraCondensed,
            "semi-condensed"         => fontdb::Stretch::SemiCondensed,
            "semi-expanded"          => fontdb::Stretch::SemiExpanded,
            "wider" | "expanded"     => fontdb::Stretch::Expanded,
            "extra-expanded"         => fontdb::Stretch::ExtraExpanded,
            "ultra-expanded"         => fontdb::Stretch::UltraExpanded,
            _                        => fontdb::Stretch::Normal,
        }
    } else {
        fontdb::Stretch::Normal
    }
}

fn conv_text_anchor(
    node: &svgdom::Node,
) -> TextAnchor {
    if let Some(n) = node.find_node_with_attribute(AId::TextAnchor) {
        match n.attributes().get_str_or(AId::TextAnchor, "") {
            "middle" => TextAnchor::Middle,
            "end"    => TextAnchor::End,
            _        => TextAnchor::Start,
        }
    } else {
        TextAnchor::Start
    }
}

#[derive(Clone, Copy)]
pub struct CharacterPosition {
    pub x: Option<f64>,
    pub y: Option<f64>,
    pub dx: Option<f64>,
    pub dy: Option<f64>,
}

/// Resolves text's character positions.
///
/// This includes: x, y, dx, dy.
///
/// # The character
///
/// The first problem with this task is that the *character* itself
/// is basically undefined in the SVG spec. Sometimes it's an *XML character*,
/// sometimes a *glyph*, and sometimes just a *character*.
///
/// There is an ongoing [discussion](https://github.com/w3c/svgwg/issues/537)
/// on the SVG working group that addresses this by stating that a character
/// is a Unicode code point. But it's not final.
///
/// Also, according to the SVG 2 spec, *character* is *a Unicode code point*.
///
/// Anyway, we treat a character as a Unicode code point.
///
/// # Algorithm
///
/// To resolve positions, we have to iterate over descendant nodes and
/// if the current node is a `tspan` and has x/y/dx/dy attribute,
/// than the positions from this attribute should be assigned to the characters
/// of this `tspan` and it's descendants.
///
/// Positions list can have more values than characters in the `tspan`,
/// so we have to clamp it, because values should not overlap, e.g.:
///
/// (we ignore whitespaces for example purposes,
/// so the `text` content is `Text` and not `T ex t`)
///
/// ```text
/// <text>
///   a
///   <tspan x="10 20 30">
///     bc
///   </tspan>
///   d
/// </text>
/// ```
///
/// In this example, the `d` position should not be set to `30`.
/// And the result should be: `[None, 10, 20, None]`
///
/// Another example:
///
/// ```text
/// <text>
///   <tspan x="100 110 120 130">
///     a
///     <tspan x="50">
///       bc
///     </tspan>
///   </tspan>
///   d
/// </text>
/// ```
///
/// The result should be: `[100, 50, 120, None]`
pub fn resolve_positions_list(
    text_node: &TextNode,
    state: &State,
) -> Vec<CharacterPosition> {
    // Allocate a list that has all characters positions set to `None`.
    let total_chars = count_chars(text_node);
    let mut list = vec![CharacterPosition {
        x: None,
        y: None,
        dx: None,
        dy: None,
    }; total_chars];

    let mut offset = 0;
    for child in text_node.descendants() {
        if child.is_element() {
            let child_chars = count_chars(&child);
            macro_rules! push_list {
                ($aid:expr, $field:ident) => {
                    if let Some(num_list) = units::convert_list(&child, $aid, state) {
                        // Note that we are using not the total count,
                        // but the amount of characters in the current `tspan` (with children).
                        let len = cmp::min(num_list.len(), child_chars);
                        for i in 0..len {
                            list[offset + i].$field = Some(num_list[i]);
                        }
                    }
                };
            }

            push_list!(AId::X, x);
            push_list!(AId::Y, y);
            push_list!(AId::Dx, dx);
            push_list!(AId::Dy, dy);
        } else if child.is_text() {
            // Advance the offset.
            offset += child.text().chars().count();
        }
    }

    list
}

/// Resolves characters rotation.
///
/// The algorithm is well explained
/// [in the SVG spec](https://www.w3.org/TR/SVG11/text.html#TSpanElement) (scroll down a bit).
///
/// ![](https://www.w3.org/TR/SVG11/images/text/tspan05-diagram.png)
///
/// Note: this algorithm differs from the position resolving one.
pub fn resolve_rotate_list(
    text_node: &TextNode,
) -> Vec<f64> {
    // Allocate a list that has all characters angles set to `0.0`.
    let mut list = vec![0.0; count_chars(text_node)];
    let mut last = 0.0;
    let mut offset = 0;
    for child in text_node.descendants() {
        if child.is_element() {
            if let Some(num_list) = child.attributes().get_number_list(AId::Rotate) {
                for i in 0..count_chars(&child) {
                    if let Some(a) = num_list.get(i).cloned() {
                        list[offset + i] = a;
                        last = a;
                    } else {
                        // If the rotate list doesn't specify the rotation for
                        // this character - use the last one.
                        list[offset + i] = last;
                    }
                }
            }
        } else if child.is_text() {
            // Advance the offset.
            offset += child.text().chars().count();
        }
    }

    list
}

#[derive(Clone)]
pub struct TextDecorationStyle {
    pub fill: Option<tree::Fill>,
    pub stroke: Option<tree::Stroke>,
}

#[derive(Clone)]
pub struct TextDecoration {
    pub underline: Option<TextDecorationStyle>,
    pub overline: Option<TextDecorationStyle>,
    pub line_through: Option<TextDecorationStyle>,
}

/// Resolves node's `text-decoration` property.
///
/// `text` and `tspan` can point to the same node.
fn resolve_decoration(
    text_node: &TextNode,
    tspan: &svgdom::Node,
    state: &State,
    tree: &mut tree::Tree,
) -> TextDecoration {
    // TODO: explain the algorithm

    let text_dec = conv_text_decoration(text_node);
    let tspan_dec = conv_text_decoration2(tspan);

    let mut gen_style = |in_tspan: bool, in_text: bool| {
        let n = if in_tspan {
            tspan.clone()
        } else if in_text {
            (*text_node).clone()
        } else {
            return None;
        };

        Some(TextDecorationStyle {
            fill: style::resolve_fill(&n, true, state, tree),
            stroke: style::resolve_stroke(&n, true, state, tree),
        })
    };

    TextDecoration {
        underline:    gen_style(tspan_dec.has_underline,    text_dec.has_underline),
        overline:     gen_style(tspan_dec.has_overline,     text_dec.has_overline),
        line_through: gen_style(tspan_dec.has_line_through, text_dec.has_line_through),
    }
}

struct TextDecorationTypes {
    has_underline: bool,
    has_overline: bool,
    has_line_through: bool,
}

/// Resolves the `text` node's `text-decoration` property.
fn conv_text_decoration(
    text_node: &TextNode,
) -> TextDecorationTypes {
    fn find_decoration(node: &svgdom::Node, value: &str) -> bool {
        node.ancestors().any(|n| n.attributes().get_str(AId::TextDecoration) == Some(value))
    }

    TextDecorationTypes {
        has_underline: find_decoration(text_node, "underline"),
        has_overline: find_decoration(text_node, "overline"),
        has_line_through: find_decoration(text_node, "line-through"),
    }
}

/// Resolves the default `text-decoration` property.
fn conv_text_decoration2(
    tspan: &svgdom::Node,
) -> TextDecorationTypes {
    let attrs = tspan.attributes();
    TextDecorationTypes {
        has_underline:    attrs.get_str(AId::TextDecoration) == Some("underline"),
        has_overline:     attrs.get_str(AId::TextDecoration) == Some("overline"),
        has_line_through: attrs.get_str(AId::TextDecoration) == Some("line-through"),
    }
}

fn resolve_baseline_shift(
    node: &svgdom::Node,
    state: &State,
) -> f64 {
    let mut shift = 0.0;
    let nodes: Vec<_> = node.ancestors().take_while(|n| !n.is_tag_name(EId::Text)).collect();
    for n in nodes.iter().rev() {
        match n.attributes().get_value(AId::BaselineShift) {
            Some(AValue::String(ref s)) => {
                match s.as_str() {
                    "baseline" => {}
                    "sub" => {
                        let font_size = units::resolve_font_size(&n, state);
                        if let Some(font) = resolve_font(n, state) {
                            shift -= font.subscript_offset(font_size);
                        }
                    }
                    "super" => {
                        let font_size = units::resolve_font_size(&n, state);
                        if let Some(font) = resolve_font(n, state) {
                            shift += font.superscript_offset(font_size);
                        }
                    }
                    _ => {}
                }
            }
            Some(AValue::Length(len)) => {
                if len.unit == Unit::Percent {
                    shift += units::resolve_font_size(&n, state) * (len.num / 100.0);
                } else {
                    shift += units::convert_length(
                        *len, &n, AId::BaselineShift, tree::Units::ObjectBoundingBox, state,
                    );
                }
            }
            _ => {}
        }
    }

    shift
}

fn resolve_font_weight(
    node: &svgdom::Node,
) -> fontdb::Weight {
    fn bound(min: usize, val: usize, max: usize) -> usize {
        cmp::max(min, cmp::min(max, val))
    }

    let nodes: Vec<_> = node.ancestors().collect();
    let mut weight = 400;
    for n in nodes.iter().rev().skip(1) { // skip Root
        weight = match n.attributes().get_str_or(AId::FontWeight, "") {
            "normal" => 400,
            "bold" => 700,
            "100" => 100,
            "200" => 200,
            "300" => 300,
            "400" => 400,
            "500" => 500,
            "600" => 600,
            "700" => 700,
            "800" => 800,
            "900" => 900,
            "bolder" => {
                // By the CSS2 spec the default value should be 400
                // so `bolder` will result in 500.
                // But Chrome and Inkscape will give us 700.
                // Have no idea is it a bug or something, but
                // we will follow such behavior for now.
                let step = if weight == 400 { 300 } else { 100 };

                bound(100, weight + step, 900)
            }
            "lighter" => {
                // By the CSS2 spec the default value should be 400
                // so `lighter` will result in 300.
                // But Chrome and Inkscape will give us 200.
                // Have no idea is it a bug or something, but
                // we will follow such behavior for now.
                let step = if weight == 400 { 200 } else { 100 };

                bound(100, weight - step, 900)
            }
            _ => weight,
        };
    }

    let weight = fontdb::Weight::from(weight as u16);
    match weight {
        fontdb::Weight::Other(_) => fontdb::Weight::Normal,
        _ => weight,
    }
}

fn count_chars(
    node: &svgdom::Node,
) -> usize {
    node.descendants()
        .filter(|n| n.is_text())
        .fold(0, |w, n| w + n.text().chars().count())
}

/// Converts the writing mode.
///
/// According to the [SVG 2.0] spec, there are only two writing modes:
/// horizontal left-to-right and vertical right-to-left.
/// E.g:
///
/// - `lr`, `lr-tb`, `rl`, `rl-tb` => `horizontal-tb`
/// - `tb`, `tb-rl` => `vertical-rl`
///
/// Also, looks like no one really supports the `rl` and `rl-tb`, except `Batik`.
/// And I'm not sure if it's behaviour is correct.
///
/// So we will ignore it as well, mainly because I have no idea how exactly
/// it should affect the rendering.
///
/// [SVG 2.0]: https://www.w3.org/TR/SVG2/text.html#WritingModeProperty
pub fn convert_writing_mode(
    text_node: &TextNode,
) -> WritingMode {
    if let Some(n) = text_node.find_node_with_attribute(AId::WritingMode) {
        match n.attributes().get_str_or(AId::WritingMode, "lr-tb") {
            "tb" | "tb-rl" => WritingMode::TopToBottom,
            _ => WritingMode::LeftToRight,
        }
    } else {
        WritingMode::LeftToRight
    }
}