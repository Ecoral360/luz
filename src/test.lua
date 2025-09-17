-- $Id: testes/literals.lua $
-- See Copyright Notice in file all.lua



fact = false
do
  local res = 1
  local function fact(n)
    if n == 0 then
      return res
    else
      return n * fact(n - 1)
    end
  end

end

