local function test(_ENV)
  assert(a == 12)
  a = 333
  assert(a == 333)
end


a = 12
local print = print
test(setmetatable({}, {__index = _ENV}))
assert(a == 12)
test(_ENV)
assert(a == 333)

print "Ok"

