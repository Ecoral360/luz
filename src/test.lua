local function facto(n)
    if n <= 1 then
        return 1
    else
        return n * facto(n-1)
    end
end

print(facto(5))
