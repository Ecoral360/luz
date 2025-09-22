use std::{cell::RefCell, rc::Rc};

use crate::{
    compiler::ctx::Upvalue,
    load,
    luz::{
        lib::LuzNativeLib,
        obj::{LuzObj, LuzType, TableRef},
    },
    luz_fn, luz_let, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn package_lib(registry: TableRef) -> LuzNativeLib {
    let require_fn = luz_fn!([1, runner, args]() {
        let loaded = runner.registry().borrow().get(&LuzObj::str("package.loaded")).as_table_or_err()?;

        luz_let!(modname =? args.get(0); else "bad argument #1 to 'require' (expected string)");

        {
            let loaded = loaded.borrow();
            let module = loaded.get(modname);
            if !module.is_nil() {
                return Ok(vec![module.clone()]);
            }
        }

        let package = runner.get_val("package").ok_or(LuzRuntimeError::message(
            "Variable named 'package' not found",
        ))?.as_table_or_err()?;

        let searchers = package.borrow().get(&LuzObj::str("searchers")).as_table_or_err()?;

        let len = searchers.borrow().len();
        for searcher_idx in 1..=len {
            let searchers = searchers.borrow();
            let searcher = searchers.get(&LuzObj::int(searcher_idx as i64));
            luz_let!(LuzObj::Function(f) = searcher);
            let result = f.borrow().call(runner, vec![modname.clone()], vec![])?;
            if !result.is_empty() && result[0].type_is(LuzType::Function) {
                luz_let!(LuzObj::Function(ref loader) = result[0]);
                let loader_data = result.get(1).unwrap_or(&LuzObj::Nil);
                let module = loader.borrow().call(runner, vec![modname.clone(), loader_data.clone()], vec![])?;
                if module.is_empty() || module[0].is_nil() {
                    let mut loaded = loaded.borrow_mut();
                    let module_val = loaded.get_or_insert(modname, true.into());
                    return Ok(vec![module_val, loader_data.clone()]);
                } else {
                    let mut loaded = loaded.borrow_mut();
                    loaded.insert(modname.clone(), module[0].clone());
                    return Ok(vec![module[0].clone(), loader_data.clone()]);
                }
            }
        }


        Ok(vec![])
    });

    let loaded = luz_table! {};

    registry
        .borrow_mut()
        .insert(LuzObj::str("package.loaded"), loaded.clone());

    let preload = luz_table! {};

    registry
        .borrow_mut()
        .insert(LuzObj::str("package.preload"), loaded.clone());

    let search_path = luz_fn!([4, _runner, _args](LuzObj::String(name), LuzObj::String(path), sep @ (LuzObj::String(..) | LuzObj::Nil), rep @ (LuzObj::String(..) | LuzObj::Nil)) {
        let templates = path.split(';');
        let sep = if sep.is_nil() { "." } else { &sep.to_string() };
        let rep = if rep.is_nil() { "/" } else { &rep.to_string() };

        let name = name.replace(sep, rep);
        let mut tried = vec![];
        for template in templates {
            let path = template.replace('?', &name);
            tried.push(path.clone());
            if std::fs::exists(&path).is_ok_and(|exists| exists) {
                return Ok(vec![LuzObj::String(path)])
            }
        }

        Ok(vec![LuzObj::str("fail"), LuzObj::str(&tried.join(";"))])
    });

    let searchers = luz_table![
        luz_fn!([1, runner, args](modname) {
            let preload = runner.registry().borrow().get(&LuzObj::str("package.preload")).as_table_or_err()?;

            let result = preload.borrow().get(modname).clone();
            Ok(vec![result, LuzObj::str(":preload:")])
        }),
        luz_fn!([1, runner, args](modname) {
            let package = runner.get_val("package").ok_or(LuzRuntimeError::message(
                "Variable named 'package' not found",
            ))?.as_table_or_err()?;

            let package = package.borrow();

            let path = package.get(&LuzObj::str("path"));

            let search_path = package.get(&LuzObj::str("searchpath"));
            luz_let!(LuzObj::Function(f) = search_path);

            let result = f.borrow().call(runner, vec![modname.clone(), path.clone()], vec![])?;

            if !result.is_empty() && result[0] != LuzObj::str("fail") {
                // Load the file as a module
                luz_let!(LuzObj::String(ref fs_path) = result[0]);

                let input = std::fs::read_to_string(fs_path).unwrap();

                let upvalues = vec![Upvalue::new("_ENV".to_owned(), 0, 0, true)];
                let r = load(&input, input.clone(), runner.env_scope(), upvalues)
                    .map_err(|e| LuzRuntimeError::message(e.to_string()))?;

                // let mod_result = r.call(runner, vec![], vec![])?;

                Ok(vec![LuzObj::Function(Rc::new(RefCell::new(r))), LuzObj::str(fs_path)])
            } else {
                Ok(result)
            }
        }),
    ];

    let path = std::env::var("LUA_PATH_5_4")
        .or_else(|_| std::env::var("LUA_PATH"))
        .unwrap_or(String::from("./?.lua"));

    let package = luz_table! {
        loaded: loaded,
        preload: preload,
        searchers: searchers,
        searchpath: search_path,
        path: LuzObj::String(path),
    };

    LuzNativeLib {
        exports: vec![
            (String::from("package"), package),
            (String::from("require"), require_fn),
        ],
    }
}
