use std::os::raw::c_void;

#[derive(Debug, Clone)]
pub enum Userdata {
    Full,
    Light(*const c_void),
}
