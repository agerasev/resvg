// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![allow(non_camel_case_types)]

use std::ffi::CStr;
use std::mem::drop;
use std::os::raw::c_char;
use std::slice;
use std::str;

use usvg::NodeExt;

#[repr(i32)]
pub enum ErrorId {
    Ok = 0,
    NotAnUtf8Str,
    FileOpenFailed,
    MalformedGZip,
    ElementsLimitReached,
    InvalidSize,
    ParsingFailed,
    PointerIsNull,
    InvalidFitValue,
    InvalidEnumValue,
    RenderError,
    BBoxError,
    EmptyNodeId,
    NodeNotFound,
    NotImplemented,
    PanicCaught,
}

macro_rules! unwrap_or_return {
    ($e:expr, $r:expr) => {
        match $e {
            Some(value) => value,
            None => return $r,
        }
    }
}

macro_rules! unwrap_or_return_into {
    ($e:expr) => {
        match $e {
            Ok(value) => value,
            Err(e) => return Into::<ErrorId>::into(e),
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct resvg_path_bbox {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct resvg_rect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct resvg_size {
    pub width: f64,
    pub height: f64,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct resvg_transform {
    pub a: f64,
    pub b: f64,
    pub c: f64,
    pub d: f64,
    pub e: f64,
    pub f: f64,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub enum resvg_fit_to_type {
    RESVG_FIT_TO_ORIGINAL,
    RESVG_FIT_TO_WIDTH,
    RESVG_FIT_TO_HEIGHT,
    RESVG_FIT_TO_ZOOM,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct resvg_fit_to {
    kind: resvg_fit_to_type,
    value: f32,
}

impl resvg_fit_to {
    #[inline]
    fn to_usvg(&self) -> Option<usvg::FitTo> {
        match self.kind {
            resvg_fit_to_type::RESVG_FIT_TO_ORIGINAL => {
                Some(usvg::FitTo::Original)
            }
            resvg_fit_to_type::RESVG_FIT_TO_WIDTH => {
                if self.value >= 1.0 {
                    Some(usvg::FitTo::Width(self.value as u32))
                } else {
                    None
                }
            }
            resvg_fit_to_type::RESVG_FIT_TO_HEIGHT => {
                if self.value >= 1.0 {
                    Some(usvg::FitTo::Height(self.value as u32))
                } else {
                    None
                }
            }
            resvg_fit_to_type::RESVG_FIT_TO_ZOOM => {
                Some(usvg::FitTo::Zoom(self.value))
            }
        }
    }
}


#[no_mangle]
pub extern "C" fn resvg_init_log() {
    if let Ok(()) = log::set_logger(&LOGGER) {
        log::set_max_level(log::LevelFilter::Warn);
    }
}


#[repr(C)]
pub struct resvg_options(usvg::Options);

#[no_mangle]
pub extern "C" fn resvg_options_create() -> *mut resvg_options {
    Box::into_raw(Box::new(resvg_options(usvg::Options::default())))
}

#[inline]
fn cast_opt(opt: *const resvg_options) -> Option<&'static usvg::Options> {
    if opt.is_null() {
        None
    } else {
        Some(unsafe { &(*opt).0 })
    }
}

#[inline]
fn cast_opt_mut(opt: *mut resvg_options) -> Option<&'static mut usvg::Options> {
    if opt.is_null() {
        None
    } else {
        Some(unsafe { &mut (*opt).0 })
    }
}

#[no_mangle]
pub extern "C" fn resvg_options_set_resources_dir(opt: *mut resvg_options, path: *const c_char) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    if path.is_null() {
        opt.resources_dir = None;
    } else {
        opt.resources_dir = Some(unwrap_or_return_into!(cstr_to_str(path)).into());
    }
    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_options_set_dpi(opt: *mut resvg_options, dpi: f64) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    opt.dpi = dpi;
    ErrorId::Ok    
}

#[no_mangle]
pub extern "C" fn resvg_options_set_font_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    let family = unwrap_or_return_into!(cstr_to_str(family));
    opt.font_family = family.to_string();
    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_options_set_font_size(opt: *mut resvg_options, font_size: f64) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    opt.font_size = font_size;
    ErrorId::Ok
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_set_serif_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
        let family = unwrap_or_return_into!(cstr_to_str(family));
        opt.fontdb.set_serif_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_set_sans_serif_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
        let family = unwrap_or_return_into!(cstr_to_str(family));
        opt.fontdb.set_sans_serif_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_set_cursive_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
        let family = unwrap_or_return_into!(cstr_to_str(family));
        opt.fontdb.set_cursive_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_set_fantasy_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
        let family = unwrap_or_return_into!(cstr_to_str(family));
        opt.fontdb.set_fantasy_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_set_monospace_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
        let family = unwrap_or_return_into!(cstr_to_str(family));
        opt.fontdb.set_monospace_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
pub extern "C" fn resvg_options_set_languages(opt: *mut resvg_options, languages: *const c_char) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);

    if languages.is_null() {
        opt.languages = Vec::new();
        return ErrorId::Ok;
    }

    let languages_str = unwrap_or_return_into!(cstr_to_str(languages));

    let mut languages = Vec::new();
    for lang in languages_str.split(',') {
        languages.push(lang.trim().to_string());
    }

    opt.languages = languages;
    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_options_set_shape_rendering_mode(opt: *mut resvg_options, mode: i32) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    opt.shape_rendering = match mode {
        0 => usvg::ShapeRendering::OptimizeSpeed,
        1 => usvg::ShapeRendering::CrispEdges,
        2 => usvg::ShapeRendering::GeometricPrecision,
        _ => return ErrorId::InvalidEnumValue,
    };
    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_options_set_text_rendering_mode(opt: *mut resvg_options, mode: i32) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    opt.text_rendering = match mode {
        0 => usvg::TextRendering::OptimizeSpeed,
        1 => usvg::TextRendering::OptimizeLegibility,
        2 => usvg::TextRendering::GeometricPrecision,
        _ => return ErrorId::InvalidEnumValue,
    };
    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_options_set_image_rendering_mode(opt: *mut resvg_options, mode: i32) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    opt.image_rendering = match mode {
        0 => usvg::ImageRendering::OptimizeQuality,
        1 => usvg::ImageRendering::OptimizeSpeed,
        _ => return ErrorId::InvalidEnumValue,
    };
    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_options_set_keep_named_groups(opt: *mut resvg_options, keep: bool) -> ErrorId {
    let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
    opt.keep_named_groups = keep;
    ErrorId::Ok
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_load_system_fonts(opt: *mut resvg_options) -> ErrorId {
    #[cfg(feature = "text")] {
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
        opt.fontdb.load_system_fonts();
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_load_font_file(
    opt: *mut resvg_options,
    file_path: *const c_char,
) -> ErrorId {
    #[cfg(feature = "text")] {
        let file_path = unwrap_or_return_into!(cstr_to_str(file_path));
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::PointerIsNull);
        match opt.fontdb.load_font_file(file_path) {
            Ok(()) => ErrorId::Ok,
            Err(e) => {
                log::warn!("Failed to load font file '{}': {}", file_path, e);
                ErrorId::FileOpenFailed
            }
        }
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn resvg_options_load_font_data(
    opt: *mut resvg_options,
    data: *const c_char,
    len: usize,
) -> ErrorId {
    #[cfg(feature = "text")] {
        if data.is_null() {
            return ErrorId::PointerIsNull;
        }
        let data = unsafe { slice::from_raw_parts(data as *const u8, len) };
        let opt = unwrap_or_return!(cast_opt_mut(opt), ErrorId::NotAnUtf8Str);
        opt.fontdb.load_font_data(data.to_vec());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[no_mangle]
pub extern "C" fn resvg_options_destroy(opt: *mut resvg_options) -> ErrorId {
    if opt.is_null() {
        return ErrorId::PointerIsNull;
    }
    drop(unsafe { Box::from_raw(opt) });
    ErrorId::Ok
}


#[repr(C)]
pub struct resvg_render_tree(pub usvg::Tree);

#[no_mangle]
pub extern "C" fn resvg_parse_tree_from_file(
    file_path: *const c_char,
    opt: *const resvg_options,
    raw_tree: *mut *mut resvg_render_tree,
) -> ErrorId {
    let file_path = unwrap_or_return_into!(cstr_to_str(file_path));

    let opt = unwrap_or_return!(cast_opt(opt), ErrorId::PointerIsNull);

    let file_data = match std::fs::read(file_path) {
        Ok(tree) => tree,
        Err(e) => {
            log::warn!("Failed read file '{}': {}", file_path, e);
            return ErrorId::FileOpenFailed;
        }
    };

    let tree = match usvg::Tree::from_data(&file_data, &opt.to_ref()) {
        Ok(tree) => tree,
        Err(e) => {
            log::warn!("Failed to parse SVG data: {}", e);
            return convert_error(e);
        },
    };

    let tree_box = Box::new(resvg_render_tree(tree));
    unsafe { *raw_tree = Box::into_raw(tree_box); }

    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_parse_tree_from_data(
    data: *const c_char,
    len: usize,
    opt: *const resvg_options,
    raw_tree: *mut *mut resvg_render_tree,
) -> ErrorId {
    if data.is_null() {
        return ErrorId::PointerIsNull;
    }
    let data = unsafe { slice::from_raw_parts(data as *const u8, len) };

    let opt = unwrap_or_return!(cast_opt(opt), ErrorId::PointerIsNull);

    let tree = match usvg::Tree::from_data(data, &opt.to_ref()) {
        Ok(tree) => tree,
        Err(e) => {
            log::warn!("Failed to parse SVG data: {}", e);
            return convert_error(e);
        }
    };

    let tree_box = Box::new(resvg_render_tree(tree));
    unsafe { *raw_tree = Box::into_raw(tree_box); }

    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_tree_destroy(tree: *mut resvg_render_tree) -> ErrorId {
    if tree.is_null() {
        return ErrorId::PointerIsNull;
    }
    drop(unsafe { Box::from_raw(tree) });
    ErrorId::Ok
}

#[no_mangle]
pub extern "C" fn resvg_is_image_empty(tree: *const resvg_render_tree) -> bool {
    if tree.is_null() {
        return false;
    }
    let tree = unsafe { &(*tree).0 };

    // The root/svg node should have at least two children.
    // The first child is `defs` and it always present.
    tree.root().children().count() > 1
}

#[no_mangle]
pub extern "C" fn resvg_get_image_size(tree: *const resvg_render_tree) -> resvg_size {
    if tree.is_null() {
        return resvg_size {
            width: 0.0,
            height: 0.0,
        };
    }
    let tree = unsafe { &(*tree).0 };

    let size = tree.svg_node().size;

    resvg_size {
        width: size.width(),
        height: size.height(),
    }
}

#[no_mangle]
pub extern "C" fn resvg_get_image_viewbox(tree: *const resvg_render_tree) -> resvg_rect {
    if tree.is_null() {
        return resvg_rect {
            x: 0.0,
            y: 0.0,
            width: 0.0,
            height: 0.0,
        };
    }
    let tree = unsafe { &(*tree).0 };

    let r = tree.svg_node().view_box.rect;

    resvg_rect {
        x: r.x(),
        y: r.y(),
        width: r.width(),
        height: r.height(),
    }
}


#[no_mangle]
pub extern "C" fn resvg_get_image_bbox(
    tree: *const resvg_render_tree,
    bbox: *mut resvg_rect,
) -> ErrorId {
    if tree.is_null() {
        return ErrorId::PointerIsNull;
    }
    let tree = unsafe { &(*tree).0 };

    if let Some(r) = tree.root().calculate_bbox().and_then(|r| r.to_rect()) {
        unsafe {
            *bbox = resvg_rect {
                x: r.x(),
                y: r.y(),
                width: r.width(),
                height: r.height(),
            }
        }

        ErrorId::Ok
    } else {
        ErrorId::BBoxError
    }
}

#[no_mangle]
pub extern "C" fn resvg_get_node_bbox(
    tree: *const resvg_render_tree,
    id: *const c_char,
    bbox: *mut resvg_path_bbox,
) -> ErrorId {
    let id = match cstr_to_str(id) {
        Ok(v) => v,
        Err(CStrToStrError::PointerIsNull) => return ErrorId::PointerIsNull,
        Err(CStrToStrError::NotAnUtf8Str(e)) => {
            log::warn!("Provided ID is no an UTF-8 string: {}", e);
            return ErrorId::NotAnUtf8Str;
        }
    };

    if id.is_empty() {
        log::warn!("Node ID must not be empty.");
        return ErrorId::EmptyNodeId;
    }

    if tree.is_null() {
        return ErrorId::PointerIsNull;
    }
    let tree = unsafe { &(*tree).0 };

    match tree.node_by_id(id) {
        Some(node) => {
            if let Some(r) = node.calculate_bbox() {
                unsafe {
                    *bbox = resvg_path_bbox {
                        x: r.x(),
                        y: r.y(),
                        width: r.width(),
                        height: r.height(),
                    }
                }

                ErrorId::Ok
            } else {
                ErrorId::BBoxError
            }
        }
        None => {
            log::warn!("No node with '{}' ID is in the tree.", id);
            ErrorId::NodeNotFound
        }
    }
}

#[no_mangle]
pub extern "C" fn resvg_node_exists(
    tree: *const resvg_render_tree,
    id: *const c_char,
) -> bool {
    let id = match cstr_to_str(id) {
        Ok(v) => v,
        Err(CStrToStrError::PointerIsNull) => return false,
        Err(CStrToStrError::NotAnUtf8Str(e)) => {
            log::warn!("Provided ID is no an UTF-8 string: {}", e);
            return false;
        }
    };

    if tree.is_null() {
        return false;
    }
    let tree = unsafe {
        &(*tree).0
    };

    tree.node_by_id(id).is_some()
}

#[no_mangle]
pub extern "C" fn resvg_get_node_transform(
    tree: *const resvg_render_tree,
    id: *const c_char,
    ts: *mut resvg_transform,
) -> ErrorId {
    let id = match cstr_to_str(id) {
        Ok(v) => v,
        Err(CStrToStrError::PointerIsNull) => return ErrorId::PointerIsNull,
        Err(CStrToStrError::NotAnUtf8Str(e)) => {
            log::warn!("Provided ID is no an UTF-8 string: {}", e);
            return ErrorId::NotAnUtf8Str;
        }
    };

    if tree.is_null() {
        return ErrorId::PointerIsNull;
    }
    let tree = unsafe { &(*tree).0 };

    if let Some(node) = tree.node_by_id(id) {
        let abs_ts = node.abs_transform();

        unsafe {
            *ts = resvg_transform {
                a: abs_ts.a,
                b: abs_ts.b,
                c: abs_ts.c,
                d: abs_ts.d,
                e: abs_ts.e,
                f: abs_ts.f,
            }
        }

        ErrorId::Ok
    } else {
        ErrorId::NodeNotFound
    }
}

enum CStrToStrError {
    PointerIsNull,
    NotAnUtf8Str(str::Utf8Error),
}

impl Into<ErrorId> for CStrToStrError {
    fn into(self) -> ErrorId {
        match self {
            CStrToStrError::PointerIsNull => ErrorId::PointerIsNull,
            CStrToStrError::NotAnUtf8Str(_) => ErrorId::NotAnUtf8Str,
        }
    }
}

fn cstr_to_str(text: *const c_char) -> Result<&'static str, CStrToStrError> {
    if text.is_null() {
        return Err(CStrToStrError::PointerIsNull);
    }
    let text = unsafe {
        CStr::from_ptr(text)
    };

    text.to_str().map_err(|e| CStrToStrError::NotAnUtf8Str(e))
}

fn convert_error(e: usvg::Error) -> ErrorId {
    match e {
        usvg::Error::NotAnUtf8Str => ErrorId::NotAnUtf8Str,
        usvg::Error::MalformedGZip => ErrorId::MalformedGZip,
        usvg::Error::ElementsLimitReached => ErrorId::ElementsLimitReached,
        usvg::Error::InvalidSize => ErrorId::InvalidSize,
        usvg::Error::ParsingFailed(_) => ErrorId::ParsingFailed,
    }
}


#[no_mangle]
pub extern "C" fn resvg_render(
    tree: *const resvg_render_tree,
    fit_to: resvg_fit_to,
    width: u32,
    height: u32,
    pixmap: *const c_char,
) -> ErrorId {
    if tree.is_null() {
        return ErrorId::PointerIsNull;
    }
    let tree = unsafe { &*tree };

    let pixmap_len = width as usize * height as usize * tiny_skia::BYTES_PER_PIXEL;
    let pixmap: &mut [u8] = unsafe { std::slice::from_raw_parts_mut(pixmap as *mut u8, pixmap_len) };
    let pixmap = tiny_skia::PixmapMut::from_bytes(pixmap, width, height).unwrap();

    let fit_to = match fit_to.to_usvg() {
        Some(fit_to) => fit_to,
        None => return ErrorId::InvalidFitValue,
    };
    match resvg::render(&tree.0, fit_to, pixmap) {
        Some(()) => ErrorId::Ok,
        None => ErrorId::RenderError,
    }
}

#[no_mangle]
pub extern "C" fn resvg_render_node(
    tree: *const resvg_render_tree,
    id: *const c_char,
    fit_to: resvg_fit_to,
    width: u32,
    height: u32,
    pixmap: *const c_char,
) -> ErrorId {
    if tree.is_null() {
        return ErrorId::PointerIsNull;
    }
    let tree = unsafe { &*tree };

    let id = unwrap_or_return_into!(cstr_to_str(id));

    if id.is_empty() {
        log::warn!("Node with an empty ID cannot be rendered.");
        return ErrorId::EmptyNodeId;
    }

    if let Some(node) = tree.0.node_by_id(id) {
        let pixmap_len = width as usize * height as usize * tiny_skia::BYTES_PER_PIXEL;
        let pixmap: &mut [u8] = unsafe { std::slice::from_raw_parts_mut(pixmap as *mut u8, pixmap_len) };
        let pixmap = tiny_skia::PixmapMut::from_bytes(pixmap, width, height).unwrap();

        let fit_to = match fit_to.to_usvg() {
            Some(fit_to) => fit_to,
            None => return ErrorId::InvalidFitValue,
        };
        match resvg::render_node(&tree.0, &node, fit_to, pixmap) {
            Some(()) => ErrorId::Ok,
            None => ErrorId::RenderError,
        }
    } else {
        log::warn!("A node with '{}' ID wasn't found.", id);
        ErrorId::NodeNotFound
    }
}


/// A simple stderr logger.
static LOGGER: SimpleLogger = SimpleLogger;
struct SimpleLogger;
impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::LevelFilter::Warn
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            let target = if record.target().len() > 0 {
                record.target()
            } else {
                record.module_path().unwrap_or_default()
            };

            let line = record.line().unwrap_or(0);

            match record.level() {
                log::Level::Error => eprintln!("Error (in {}:{}): {}", target, line, record.args()),
                log::Level::Warn  => eprintln!("Warning (in {}:{}): {}", target, line, record.args()),
                log::Level::Info  => eprintln!("Info (in {}:{}): {}", target, line, record.args()),
                log::Level::Debug => eprintln!("Debug (in {}:{}): {}", target, line, record.args()),
                log::Level::Trace => eprintln!("Trace (in {}:{}): {}", target, line, record.args()),
            }
        }
    }

    fn flush(&self) {}
}
