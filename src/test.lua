

local x = 1
local y = 25
return y > 23.2



-- print("testing functions and calls")

-- get the opportunity to test 'type' too ;)

-- assert(type(1<2) == 'boolean')
-- assert(type(true) == 'boolean' and type(false) == 'boolean')
-- assert(type(nil) == 'nil'
--    and type(-3) == 'number'
--    and type'x' == 'string'
--    and type{} == 'table'
--    and type(type) == 'function')
--
-- assert(type(assert) == type(print))
-- local function f (x) return a:x (x) end
-- assert(type(f) == 'function')
-- assert(not pcall(type))
--
--
-- -- testing local-function recursion
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
