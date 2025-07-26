print("Test starting...")
local ok, err = pcall(dofile, "aos.lua")
if not ok then
    print("Error loading aos.lua: " .. tostring(err))
    os.exit(1)
end
print("aos.lua loaded successfully")
print("compute function exists: " .. tostring(type(compute) == "function"))