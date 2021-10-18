// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![allow(non_camel_case_types)]

use std::ffi::CStr;
use std::mem::drop;
use std::os::raw::c_char;
use std::ptr;
use std::slice;
use std::str;
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard, TryLockError};

use usvg::NodeExt;

use macro_rules_attribute::apply;

macro_rules! c_api {
    (
        #[return_on_panic($ret_on_panic:expr)]
        $(#[$field_meta:meta])*
        $fn_vis:vis fn $fn_name:ident($( $arg_name:ident : $arg_type:ty ),* $(,)?) $( -> $ret_type:ty )?
        $fn_body:block
    ) => (
        $(#[$field_meta])*
        #[no_mangle]
        #[no_panic::no_panic]
        $fn_vis extern "C" fn $fn_name($( $arg_name : $arg_type ),*) $( -> $ret_type )? {
            std::panic::catch_unwind(move || $fn_body).unwrap_or($ret_on_panic)
        }
    )
}

#[repr(C)]
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
    RenderFailed,
    BBoxCalcFailed,
    EmptyNodeId,
    NodeNotFound,
    ZeroPixmapSize,
    PixmapCreationFailed,
    NotImplemented,
    ConcurrentAccess,
    PanicCaught,
    Poisoned,
}

macro_rules! some_or_return {
    ($opt:expr, $ret_val:expr $(,)?) => {
        match $opt {
            Some(val) => val,
            None => return $ret_val,
        }
    };
}

macro_rules! ok_or_return {
    ($res:expr, $map_ret_val:expr $(,)?) => {
        match $res {
            Ok(val) => val,
            Err(err) => return $map_ret_val(err),
        }
    };
    ($res:expr) => {
        ok_or_return!($res, Into::<ErrorId>::into)
    };
}

fn store_to_ptr<T>(dst: *mut T, src: T) -> bool {
    if dst.is_null() {
        false
    } else {
        unsafe { ptr::write(dst, src); }
        true
    }
}

macro_rules! store_to_ptr_or_return {
    ($dst:expr, $src:expr, $ret:expr $(,)?) => {
        if !store_to_ptr($dst, $src) {
            return $ret;
        }
    };
    ($dst:expr, $src:expr $(,)?) => {
        store_to_ptr_or_return!($dst, $src, ErrorId::PointerIsNull);
    };
}

enum CastError {
    PointerIsNull,
    ConcurrentAccess,
    Poisoned,
}

impl Into<ErrorId> for CastError {
    fn into(self) -> ErrorId {
        match self {
            CastError::PointerIsNull => ErrorId::PointerIsNull,
            CastError::ConcurrentAccess => ErrorId::ConcurrentAccess,
            CastError::Poisoned => ErrorId::Poisoned,
        }
    }
}

fn cast_ptr<'a, T, W>(ptr: *const W) -> Result<RwLockReadGuard<'a, T>, CastError>
where
    W: AsRef<RwLock<T>> + 'a,
{
    let wrapper = match unsafe { ptr.as_ref() } {
        Some(v) => v,
        None => return Err(CastError::PointerIsNull),
    };
    let lock = match wrapper.as_ref().try_read() {
        Ok(v) => v,
        Err(TryLockError::WouldBlock) => return Err(CastError::ConcurrentAccess),
        Err(TryLockError::Poisoned(_)) => return Err(CastError::Poisoned),
    };
    Ok(lock)
}

fn cast_mut_ptr<'a, T, W>(ptr: *mut W) -> Result<RwLockWriteGuard<'a, T>, CastError>
where
    W: AsMut<RwLock<T>> + 'a,
{
    let wrapper = match unsafe { ptr.as_mut() } {
        Some(v) => v,
        None => return Err(CastError::PointerIsNull),
    };
    let lock = match wrapper.as_mut().try_write() {
        Ok(v) => v,
        Err(TryLockError::WouldBlock) => return Err(CastError::ConcurrentAccess),
        Err(TryLockError::Poisoned(_)) => return Err(CastError::Poisoned),
    };
    Ok(lock)
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


#[apply(c_api!)]
#[return_on_panic(())]
pub fn resvg_init_log() {
    if let Ok(()) = log::set_logger(&LOGGER) {
        log::set_max_level(log::LevelFilter::Warn);
    }
}


#[repr(C)]
pub struct resvg_options(RwLock<usvg::Options>);

impl AsRef<RwLock<usvg::Options>> for resvg_options {
    fn as_ref(&self) -> &RwLock<usvg::Options> {
        &self.0
    }
}

impl AsMut<RwLock<usvg::Options>> for resvg_options {
    fn as_mut(&mut self) -> &mut RwLock<usvg::Options> {
        &mut self.0
    }
}

#[apply(c_api!)]
#[return_on_panic(std::ptr::null_mut())]
pub fn resvg_options_create() -> *mut resvg_options {
    Box::into_raw(Box::new(resvg_options(RwLock::new(
        usvg::Options::default(),
    ))))
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_resources_dir(opt: *mut resvg_options, path: *const c_char) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    if path.is_null() {
        opt.resources_dir = None;
    } else {
        opt.resources_dir = Some(ok_or_return!(cstr_to_str(path)).into());
    }
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_dpi(opt: *mut resvg_options, dpi: f64) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    opt.dpi = dpi;
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_font_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    let family = ok_or_return!(cstr_to_str(family));
    opt.font_family = family.to_string();
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_font_size(opt: *mut resvg_options, font_size: f64) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    opt.font_size = font_size;
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_set_serif_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        let family = ok_or_return!(cstr_to_str(family));
        opt.fontdb.set_serif_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_set_sans_serif_family(
    opt: *mut resvg_options,
    family: *const c_char,
) -> ErrorId {
    #[cfg(feature = "text")] {
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        let family = ok_or_return!(cstr_to_str(family));
        opt.fontdb.set_sans_serif_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_set_cursive_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        let family = ok_or_return!(cstr_to_str(family));
        opt.fontdb.set_cursive_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_set_fantasy_family(opt: *mut resvg_options, family: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        let family = ok_or_return!(cstr_to_str(family));
        opt.fontdb.set_fantasy_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_set_monospace_family(
    opt: *mut resvg_options,
    family: *const c_char,
) -> ErrorId {
    #[cfg(feature = "text")] {
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        let family = ok_or_return!(cstr_to_str(family));
        opt.fontdb.set_monospace_family(family.to_string());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_languages(opt: *mut resvg_options, languages: *const c_char) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));

    if languages.is_null() {
        opt.languages = Vec::new();
        return ErrorId::Ok;
    }

    let languages_str = ok_or_return!(cstr_to_str(languages));

    let mut languages = Vec::new();
    for lang in languages_str.split(',') {
        languages.push(lang.trim().to_string());
    }

    opt.languages = languages;
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_shape_rendering_mode(opt: *mut resvg_options, mode: i32) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    opt.shape_rendering = match mode {
        0 => usvg::ShapeRendering::OptimizeSpeed,
        1 => usvg::ShapeRendering::CrispEdges,
        2 => usvg::ShapeRendering::GeometricPrecision,
        _ => return ErrorId::InvalidEnumValue,
    };
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_text_rendering_mode(opt: *mut resvg_options, mode: i32) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    opt.text_rendering = match mode {
        0 => usvg::TextRendering::OptimizeSpeed,
        1 => usvg::TextRendering::OptimizeLegibility,
        2 => usvg::TextRendering::GeometricPrecision,
        _ => return ErrorId::InvalidEnumValue,
    };
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_image_rendering_mode(opt: *mut resvg_options, mode: i32) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    opt.image_rendering = match mode {
        0 => usvg::ImageRendering::OptimizeQuality,
        1 => usvg::ImageRendering::OptimizeSpeed,
        _ => return ErrorId::InvalidEnumValue,
    };
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_set_keep_named_groups(opt: *mut resvg_options, keep: bool) -> ErrorId {
    let mut opt = ok_or_return!(cast_mut_ptr(opt));
    opt.keep_named_groups = keep;
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_load_system_fonts(opt: *mut resvg_options) -> ErrorId {
    #[cfg(feature = "text")] {
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        opt.fontdb.load_system_fonts();
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_load_font_file(opt: *mut resvg_options, file_path: *const c_char) -> ErrorId {
    #[cfg(feature = "text")] {
        let file_path = ok_or_return!(cstr_to_str(file_path));
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        match opt.fontdb.load_font_file(file_path) {
            Ok(()) => ErrorId::Ok,
            Err(e) => ErrorId::FileOpenFailed,
        }
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
#[allow(unused_variables)]
pub fn resvg_options_load_font_data(
    opt: *mut resvg_options,
    data: *const c_char,
    len: usize,
) -> ErrorId {
    #[cfg(feature = "text")] {
        if data.is_null() {
            return ErrorId::PointerIsNull;
        }
        let data = unsafe { slice::from_raw_parts(data as *const u8, len) };
        let mut opt = ok_or_return!(cast_mut_ptr(opt));
        opt.fontdb.load_font_data(data.to_vec());
        ErrorId::Ok
    }

    #[cfg(not(feature = "text"))] {
        ErrorId::NotImplemented
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_options_destroy(opt: *mut resvg_options) -> ErrorId {
    if opt.is_null() {
        return ErrorId::PointerIsNull;
    }
    drop(unsafe { Box::from_raw(opt) });
    ErrorId::Ok
}


#[repr(C)]
pub struct resvg_render_tree(pub RwLock<usvg::Tree>);

impl AsRef<RwLock<usvg::Tree>> for resvg_render_tree {
    fn as_ref(&self) -> &RwLock<usvg::Tree> {
        &self.0
    }
}

impl AsMut<RwLock<usvg::Tree>> for resvg_render_tree {
    fn as_mut(&mut self) -> &mut RwLock<usvg::Tree> {
        &mut self.0
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_parse_tree_from_file(
    file_path: *const c_char,
    opt: *const resvg_options,
    raw_tree: *mut *mut resvg_render_tree,
) -> ErrorId {
    let file_path = ok_or_return!(cstr_to_str(file_path));

    let opt = ok_or_return!(cast_ptr(opt));

    let file_data = match std::fs::read(file_path) {
        Ok(tree) => tree,
        Err(_) => return ErrorId::FileOpenFailed,
    };

    let tree = match usvg::Tree::from_data(&file_data, &opt.to_ref()) {
        Ok(tree) => tree,
        Err(e) => return convert_error(e),
    };

    let tree_box = Box::new(resvg_render_tree(RwLock::new(tree)));
    unsafe { *raw_tree = Box::into_raw(tree_box); }

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_parse_tree_from_data(
    data: *const c_char,
    len: usize,
    opt: *const resvg_options,
    raw_tree: *mut *mut resvg_render_tree,
) -> ErrorId {
    if data.is_null() {
        return ErrorId::PointerIsNull;
    }
    let data = unsafe { slice::from_raw_parts(data as *const u8, len) };

    let opt = ok_or_return!(cast_ptr(opt));

    let tree = match usvg::Tree::from_data(data, &opt.to_ref()) {
        Ok(tree) => tree,
        Err(e) => return convert_error(e),
    };

    let tree_box = Box::new(resvg_render_tree(RwLock::new(tree)));
    unsafe { *raw_tree = Box::into_raw(tree_box); }

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_tree_destroy(tree: *mut resvg_render_tree) -> ErrorId {
    if tree.is_null() {
        return ErrorId::PointerIsNull;
    }
    drop(unsafe { Box::from_raw(tree) });
    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_is_image_empty(tree: *const resvg_render_tree, is_empty: *mut bool) -> ErrorId {
    let tree = ok_or_return!(cast_ptr(tree));

    // The root/svg node should have at least two children.
    // The first child is `defs` and it always present.
    store_to_ptr_or_return!(is_empty, tree.root().children().count() <= 1);

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_get_image_size(tree: *const resvg_render_tree, size: *mut resvg_size) -> ErrorId {
    let tree = ok_or_return!(cast_ptr(tree));

    let s = tree.svg_node().size;

    store_to_ptr_or_return!(
        size,
        resvg_size {
            width: s.width(),
            height: s.height(),
        },
    );

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_get_image_viewbox(tree: *const resvg_render_tree, rect: *mut resvg_rect) -> ErrorId {
    let tree = ok_or_return!(cast_ptr(tree));

    let r = tree.svg_node().view_box.rect;

    store_to_ptr_or_return!(
        rect,
        resvg_rect {
            x: r.x(),
            y: r.y(),
            width: r.width(),
            height: r.height(),
        },
    );

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_get_image_bbox(tree: *const resvg_render_tree, bbox: *mut resvg_rect) -> ErrorId {
    let tree = ok_or_return!(cast_ptr(tree));

    let r = some_or_return!(tree.root().calculate_bbox(), ErrorId::BBoxCalcFailed);

    store_to_ptr_or_return!(
        bbox,
        resvg_rect {
            x: r.x(),
            y: r.y(),
            width: r.width(),
            height: r.height(),
        },
    );

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_get_node_bbox(
    tree: *const resvg_render_tree,
    id: *const c_char,
    bbox: *mut resvg_path_bbox,
) -> ErrorId {
    let id = ok_or_return!(cstr_to_str(id));
    if id.is_empty() {
        return ErrorId::EmptyNodeId;
    }

    let tree = ok_or_return!(cast_ptr(tree));

    let node = some_or_return!(tree.node_by_id(id), ErrorId::NodeNotFound);

    let r = some_or_return!(node.calculate_bbox(), ErrorId::BBoxCalcFailed);

    store_to_ptr_or_return!(
        bbox,
        resvg_path_bbox {
            x: r.x(),
            y: r.y(),
            width: r.width(),
            height: r.height(),
        }
    );

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_node_exists(
    tree: *const resvg_render_tree,
    id: *const c_char,
    exists: *mut bool,
) -> ErrorId {
    let id = ok_or_return!(cstr_to_str(id));
    if id.is_empty() {
        return ErrorId::EmptyNodeId;
    }

    let tree = ok_or_return!(cast_ptr(tree));

    store_to_ptr_or_return!(
        exists,
        tree.node_by_id(id).is_some(),
    );

    ErrorId::Ok
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_get_node_transform(
    tree: *const resvg_render_tree,
    id: *const c_char,
    ts: *mut resvg_transform,
) -> ErrorId {
    let id = ok_or_return!(cstr_to_str(id));
    if id.is_empty() {
        return ErrorId::EmptyNodeId;
    }

    let tree = ok_or_return!(cast_ptr(tree));

    let node = some_or_return!(tree.node_by_id(id), ErrorId::NodeNotFound);

    let abs_ts = node.abs_transform();

    store_to_ptr_or_return!(
        ts,
        resvg_transform {
            a: abs_ts.a,
            b: abs_ts.b,
            c: abs_ts.c,
            d: abs_ts.d,
            e: abs_ts.e,
            f: abs_ts.f,
        },
    );

    ErrorId::Ok
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
    let text = unsafe { CStr::from_ptr(text) };

    text.to_str().map_err(CStrToStrError::NotAnUtf8Str)
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

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_render(
    tree: *const resvg_render_tree,
    fit_to: resvg_fit_to,
    width: u32,
    height: u32,
    pixmap: *const c_char,
) -> ErrorId {
    let tree = ok_or_return!(cast_ptr(tree));

    if pixmap.is_null() {
        return ErrorId::PointerIsNull;
    }
    if width == 0 || height == 0 {
        return ErrorId::ZeroPixmapSize;
    }
    let pixmap_len = width as usize * height as usize * tiny_skia::BYTES_PER_PIXEL;
    let pixmap: &mut [u8] = unsafe { std::slice::from_raw_parts_mut(pixmap as *mut u8, pixmap_len) };
    let pixmap = some_or_return!(
        tiny_skia::PixmapMut::from_bytes(pixmap, width, height),
        ErrorId::PixmapCreationFailed,
    );

    let fit_to = some_or_return!(fit_to.to_usvg(), ErrorId::InvalidFitValue);

    match resvg::render(&tree, fit_to, pixmap) {
        Some(()) => ErrorId::Ok,
        None => ErrorId::RenderFailed,
    }
}

#[apply(c_api!)]
#[return_on_panic(ErrorId::PanicCaught)]
pub fn resvg_render_node(
    tree: *const resvg_render_tree,
    id: *const c_char,
    fit_to: resvg_fit_to,
    width: u32,
    height: u32,
    pixmap: *const c_char,
) -> ErrorId {
    let tree = ok_or_return!(cast_ptr(tree));

    let id = ok_or_return!(cstr_to_str(id));
    if id.is_empty() {
        return ErrorId::EmptyNodeId;
    }

    if let Some(node) = tree.node_by_id(id) {
        if pixmap.is_null() {
            return ErrorId::PointerIsNull;
        }
        if width == 0 || height == 0 {
            return ErrorId::ZeroPixmapSize;
        }
        let pixmap_len = width as usize * height as usize * tiny_skia::BYTES_PER_PIXEL;
        let pixmap: &mut [u8] = unsafe { std::slice::from_raw_parts_mut(pixmap as *mut u8, pixmap_len) };
        let pixmap = some_or_return!(
            tiny_skia::PixmapMut::from_bytes(pixmap, width, height),
            ErrorId::PixmapCreationFailed,
        );

        let fit_to = some_or_return!(fit_to.to_usvg(), ErrorId::InvalidFitValue);

        match resvg::render_node(&tree, &node, fit_to, pixmap) {
            Some(()) => ErrorId::Ok,
            None => ErrorId::RenderFailed,
        }
    } else {
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
