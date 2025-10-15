use std::os::raw::c_void;

#[derive(Debug, Clone)]
pub enum Userdata<T> {
    Full(T),
    Light(*const c_void),
}
