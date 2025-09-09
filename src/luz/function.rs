use derive_builder::Builder;

#[derive(Debug, Clone, Builder)]
pub struct FuncParams {
    #[builder(default = vec![])]
    fixed: Vec<String>,
    #[builder(default = false)]
    is_vararg: bool,
}



#[derive(Debug, Clone)]
pub struct LuzFunction {}
