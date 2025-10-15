-- and_or_test_suite.lua
-- Thorough test battery for Lua 'and' and 'or' semantics (Lua 5.4)
-- Use this to validate evaluation order, short-circuit, truthiness, multiple returns, precedence, etc.

local results = {}
local pass_count, fail_count = 0, 0

local function fail(name, msg)
  table.insert(results, {name = name, ok = false, msg = msg})
  fail_count = fail_count + 1
  io.write("F")
end

local function pass(name)
  table.insert(results, {name = name, ok = true})
  pass_count = pass_count + 1
  io.write(".")
end

local function expect_eq(name, actual, expected)
  local ok = (actual == expected) or (actual ~= actual and expected ~= expected) -- NaN handling
  if ok then pass(name) else fail(name, ("expected <%s> got <%s>"):format(tostring(expected), tostring(actual))) end
end

local function expect_is_nil(name, actual)
  if actual == nil then pass(name) else fail(name, ("expected <nil> got <%s>"):format(tostring(actual))) end
end

local function expect_true(name, cond)
  if cond then pass(name) else fail(name, "expected true") end
end

local function expect_false(name, cond)
  if not cond then pass(name) else fail(name, "expected false") end
end

local function expect_error(name, fn)
  local ok, err = pcall(fn)
  if not ok then pass(name) else fail(name, "expected error but no error") end
end

local function expect_no_error(name, fn, expected_result)
  local ok, err = pcall(fn)
  if ok then expect_eq(ok, expected_result) else fail(name, "expected not error but error") end
end

-- utilities for side-effects / tracking order
local function mk_logger()
  local log = {}
  local function append(tag) table.insert(log, tag) end
  local function get() return table.concat(log, ",") end
  return append, get
end

---- TESTS ----

-- 1) Basic truthiness: only nil and false are falsey
expect_true("truthy_number", (not (not 0)))      -- 0 is truthy
expect_true("truthy_empty_string", (not (not "")))
expect_false("false_is_falsey", (not (not false))) -- helper sanity
expect_false("nil_is_falsey", (not (not nil)))

-- Using and/or direct values: and returns first falsy or last value; or returns first truthy or last value
expect_eq("and_returns_last_when_all_truthy", (1 and "a" and true), true)
expect_eq("and_returns_first_falsy", (1 and nil and 3), nil)
expect_eq("and_returns_first_falsy_false", (1 and false and 3), false)

expect_eq("or_returns_first_truthy", (nil or false or 0 or "x"), 0) -- 0 is first truthy
expect_eq("or_returns_last_when_all_falsey", (nil or false), false)  -- returns last (false)

-- 2) Short-circuiting with side effects (and)
do
  local log_append, get_log = mk_logger()
  local function A() log_append("A"); return true end
  local function B() log_append("B"); return false end
  local function C() log_append("C"); return 123 end

  local r = A() and B() and C()
  expect_eq("shortcircuit_and_not_call_later", get_log(), "A,B")
  expect_eq("and_result_when_falsy_found", r, false)
end

-- 3) Short-circuiting with side effects (or)
do
  local log_append, get_log = mk_logger()
  local function A() log_append("A"); return nil end
  local function B() log_append("B"); return "ok" end
  local function C() log_append("C"); return "shouldn't" end

  local r = A() or B() or C()
  expect_eq("shortcircuit_or_stop_at_first_truthy", get_log(), "A,B")
  expect_eq("or_result_is_first_truthy", r, "ok")
end

-- 4) Order of evaluation: left-to-right for operands
do
  local log_append, get_log = mk_logger()
  local function left() log_append("L"); return false end
  local function right() log_append("R"); return true end

  local _ = left() and right()
  expect_eq("order_and_left_to_right", get_log(), "L")
  log_append, get_log = mk_logger()  -- reset
  local _ = left() or right()
  expect_eq("order_or_left_to_right", get_log(), "L,R") -- left false -> evaluate right
end

-- 5) and/or used in if/elseif/while/repeat
do
  local called = {}
  local function f(t) table.insert(called, t) return true end

  local condition = (true and f("A"))  -- should call f("A")
  if condition and false then
    table.insert(called, "never")
  elseif condition or false then
    table.insert(called, "elseif-taken")
  end

  expect_eq("if_elseif_or_and_flow", table.concat(called, ","), "A,elseif-taken")
end

-- while loop uses condition truthiness short-circuited too
do
  local cnt = 0
  local function dec()
    cnt = cnt + 1
    return cnt < 2 and true or false  -- will be true once
  end
  local iterations = 0
  while dec() and false do
    iterations = iterations + 1
  end
  -- dec() evaluated, but while body not run because second operand false
  expect_eq("while_shortcircuit", cnt, 1)
  expect_eq("while_body_not_entered", iterations, 0)
end

-- repeat-until: body executes at least once; condition uses truthiness
do
  local i = 0
  repeat
    i = i + 1
  until (i >= 1) and true
  expect_eq("repeat_until_and_condition", i, 1)
end

-- 6) Precedence: 'and' has higher precedence than 'or'
-- a or b and c  === a or (b and c)
do
  local a, b, c = false, true, false
  local res1 = a or b and c
  local res2 = a or (b and c)
  expect_eq("precedence_and_higher_than_or", res1, res2)
  expect_eq("precedence_example_value", res1, false)
end

-- Precedence with 'not': not has higher precedence than and
do
  local a = false
  local b = true
  expect_eq("not_precedence", (not a) and b, ( (not a) and b ) )
  expect_eq("not_precedence_value", (not a) and b, true)
end
--
-- -- 7) Associativity: left-associative chaining
-- do
--   local function f(s) return s end
--   local chain1 = (nil or false or "x" or "y")
--   local chain2 = (((nil or false) or "x") or "y")
--   expect_eq("or_left_associative", chain1, chain2)
--   expect_eq("or_chain_result", chain1, "x")
-- end
--
-- -- 8) Multiple return values interaction
-- do
--   local function one_nil_two() return nil, 2 end
--   local function three() return 3 end
--
--   local r = one_nil_two() or three()   -- 'or' should use first return only; since first ret is nil -> call three()
--   expect_eq("or_with_multi_return_first_check", r, 3)
--
--   -- when used as last expression in argument list, multiple returns are preserved,
--   -- but when used in an expression with 'or'/'and', only first return is considered.
--   local function recv(...) return ... end
--   local a1, a2 = recv(one_nil_two() or three())  -- or yields 3, recv(3) -> one return
--   expect_eq("or_returns_single_value_in_expr_context", a1, 3)
--   expect_is_nil("or_returns_single_value_in_expr_context_second", a2)
-- end
--
-- -- 9) Using and/or in function args and assignment lists
-- do
--   local function id(...) return ... end
--   -- when and/or used in a list of arguments, it behaves like an expression producing one value
--   local x = (false and error("no") or 10)
--   expect_eq("and_or_in_assignment_expr", x, 10)
--
--   -- In multi-assignment, only the last expression can produce multiple returns.
--   local a,b = (false and error("no") or 1), (function() return 2,3 end)()
--   expect_eq("multiassign_last_can_have_multiple", a, 1)
--   expect_eq("multiassign_last_ret2", b, 2)
-- end
--
-- -- 10) Behavior in table constructors and field initializers
-- do
--   local log_append, get_log = mk_logger()
--   local function E() error("shouldn't evaluate") end
--   local function T() log_append("T"); return "ok" end
--
--   local t = { a = false and E(), b = true or E(), c = nil or T() }
--   expect_eq("table_field_and_shortcircuit", t.a, false)
--   expect_eq("table_field_or_shortcircuit", t.b, true)
--   expect_eq("table_field_or_evaluates_rhs_on_nil", t.c, "ok")
--   expect_eq("table_field_sideeffect", get_log(), "T")
-- end
--
-- -- 11) Indexing and errors: LHS is evaluated before RHS, and errors propagate appropriately
-- do
--   local function idx_t() return nil end
--   local function rhs() return 1 end
--   -- attempting to index nil should cause an error; ensure that error occurs before any possible RHS is evaluated in a chained expression where indexing is on LHS
--   expect_error("index_nil_raises", function()
--     local t = nil
--     -- this will attempt to do (t["a"]).y = rhs()  which errors when evaluating t["a"]
--     local _ = (t["a"]).y and rhs()
--   end)
-- end
--
-- -- 12) Using and/or with metamethods: metamethods are NOT invoked for 'and'/'or' (they only evaluate operands as values)
-- -- (Testing that 'and'/'or' do not call __tostring or similar is hard; test that operands are taken as-is)
-- do
--   local mt = {}
--   local o = setmetatable({}, mt)
--   -- __index or __call aren't invoked by 'and'/'or' unless the expression explicitly calls them.
--   -- Just ensure truthiness check is based on the value itself:
--   expect_eq("metatable_object_truthy", (o and "ok"), "ok")
-- end
--
-- -- 13) Combining with relational operators and mixing types
-- do
--   expect_eq("comparisons_and_and_or", (1 < 2 and "yes" or "no"), "yes")
--   expect_eq("comparisons_with_false", (1 > 2 and "yes" or "no"), "no")
-- end
--
-- -- 14) Short-circuit stops evaluation of function that errors
-- do
--   local function good() return true end
--   local function bad() error("bad called") end
--
--   -- and short-circuits if left is false
--   expect_no_error("and_shortcircuit_prevents_call", function() return false and bad() end) -- returns false without calling bad; the expression itself does not error, but our wrapper will return false — so we invert: we expect no error. Adjust:
--   -- Better test: ensure bad is not called by using a pcall wrapper
--   local called = false
--   local function bad_mark() called = true; error("bad") end
--   local ok = pcall(function() return false and bad_mark() end)
--   expect_false("and_does_not_call_rhs_when_left_false", called)
--   expect_true("and_pcall_returns_true_no_error", ok) -- pcall should succeed because expression didn't error
--
--   -- or short-circuits if left is truthy
--   called = false
--   ok = pcall(function() return true or bad_mark() end)
--   expect_false("or_does_not_call_rhs_when_left_truthy", called)
--   expect_true("or_pcall_returns_true_no_error", ok)
-- end
--
-- -- 15) empty-string and zero are truthy (sanity)
-- expect_true("empty_string_is_truthy", not not "")
-- expect_true("zero_is_truthy", not not 0)
--
-- -- 16) chaining with mixed falsey values: first falsy returned
-- expect_eq("and_chain_first_falsy_returned", (1 and "ok" and nil and "later"), nil)
-- expect_eq("or_chain_first_truthy_returned", (nil or false or "first" or "second"), "first")
--
-- -- 17) Logical expressions producing functions / tables, verify returned value identity, not boolean coercion
-- do
--   local t = {}
--   local r = t and {}  -- r is a new table because t truthy
--   expect_true("and_returns_rvalue_not_boolean", type(r) == "table")
-- end
--
-- -- 18) 'not' interaction: not (a or b) vs (not a) and (not b)
-- do
--   local a, b = false, false
--   expect_eq("de_morgan_like", (not (a or b)), ((not a) and (not b)))
--   a, b = false, true
--   expect_eq("de_morgan_like2", (not (a or b)), ((not a) and (not b)))
-- end
--
-- -- 19) Expressions combined with concatenation and arithmetic (precedence checks)
-- do
--   -- check that and/or do not break arithmetic precedence; arithmetic should bind tighter than and/or
--   local res = 1 + 2 and 0 or 5  -- (1+2) and 0 => 0; 0 or 5 => 0
--   expect_eq("arithmetic_before_and_or", res, 0)
-- end
--
-- -- 20) Using boolean expressions as table keys (edge-case)
-- do
--   local t = {}
--   t[true and "k"] = 1
--   expect_eq("boolean_expr_table_key", t["k"], 1)
-- end
--
-- -- 21) 'and' and 'or' with functions returning nil as first result (confirm single-value behavior)
-- do
--   local function ret_nil_then_val() return nil, "second" end
--   local function ret_ok() return "ok" end
--   local v = (ret_nil_then_val() or ret_ok()) -- ret_nil_then_val() yields nil as first -> ret_ok called
--   expect_eq("multi_ret_first_nil_or_calls_rhs", v, "ok")
--   -- show that ret_nil_then_val() as last in arglist would return multiple values, but when used with or it becomes single:
--   local a,b = (ret_nil_then_val() or ret_ok())  -- or yields only "ok"
--   expect_eq("or_single_value_in_assignment_first", a, "ok")
--   expect_is_nil("or_single_value_in_assignment_second", b)
-- end
--
-- -- 22) 'and' with numeric 0 behavior (0 is truthy so should not short-circuit)
-- do
--   local called = false
--   local function mark() called = true; return true end
--   local r = 0 and mark()
--   expect_true("zero_doesnt_shortcircuit", called)
--   expect_eq("zero_and_returns_rhs", r, true)
-- end
--
-- -- 23) long chain stress: many operands to ensure left-to-right evaluation and final value correctness
-- do
--   local expr = nil
--   for i=1,50 do
--     expr = expr or (i % 2 == 0 and false or i)  -- produce first odd number truthy
--   end
--   expect_eq("long_chain_result", expr, 1)
-- end
--
-- -- 24) errors when evaluating LHS indexing before or/and: ensure index error occurs (already tested but add more)
-- do
--   expect_error("index_error_propagates", function()
--     local a = nil
--     local x = a.foo or 1  -- attempts to evaluate a.foo -> error
--   end)
-- end
--
-- -- 25) ensure 'and'/'or' do not coerce to boolean, they return operand values
-- do
--   local v = ("hi" and 0)  -- returns 0, not converted to true/false
--   expect_eq("and_doesnt_coerce_to_boolean", v, 0)
-- end
--
-- -- 26) ensure 'or' returns last operand if all falsey
-- do
--   expect_eq("or_all_falsey_returns_last", (nil or false or nil or false), false)
-- end
--
-- -- 27) Using 'and'/'or' in return statement
-- do
--   local function f1() return nil or "fallback" end
--   local function f2() return "val" and "kept" end
--   expect_eq("return_or", f1(), "fallback")
--   expect_eq("return_and", f2(), "kept")
-- end
--
-- -- 28) short-circuit in chained concatenation (ensure side-effects)
-- do
--   local log_append, get_log = mk_logger()
--   local function A() log_append("A"); return nil end
--   local function B() log_append("B"); return "b" end
--   local res = (A() or B()) .. "X"   -- A() nil -> call B() -> concat
--   expect_eq("concat_with_or_calls_rhs", get_log(), "A,B")
--   expect_eq("concat_result", res, "bX")
-- end
--
---- summary ----
io.write("\n") -- progress line break
print(("Passed: %d, Failed: %d"):format(pass_count, fail_count))
if fail_count > 0 then
  print("Failures detail:")
  for _,r in ipairs(results) do
    if not r.ok then
      print("- ", r.name, ":", r.msg)
    end
  end
else
  print("All tests passed.")
end
--
-- -- return a table so test runner can inspect if run via require
-- return { results = results, pass = pass_count, fail = fail_count }
--
