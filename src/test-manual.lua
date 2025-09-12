local x = 2
print(type(1<x), 1, 2 + x)
print(type(""), 12)

-- assert(type(true) == 'boolean' and type(false) == 'boolean')
-- assert(type(nil) == 'nil'
--    and type(-3) == 'number'
--    and type'x' == 'string'
--    and type{} == 'table'
--    and type(type) == 'function')

-- assert(type(assert) == type(print))
-- local function f (x) return a:x (x) end
-- assert(type(f) == 'function')
-- assert(not pcall(type))


-- testing local-function recursion
-- fact = false
-- do
--   local res = 1
--   local function fact (n)
--     if n==0 then return res
--     else return n*fact(n-1)
--     end
--   end
--   assert(fact(5) == 120)
-- end
-- assert(fact == false)
-- fact = nil
