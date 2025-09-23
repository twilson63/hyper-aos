-- Test that writes output to a file

local output_file = io.open("test_output.txt", "w")
if not output_file then
    error("Cannot open output file")
end

local function log(...)
    local args = {...}
    local str = table.concat(args, " ")
    output_file:write(str .. "\n")
    output_file:flush()
end

log("Starting test...")

-- Set up environment
_G.package = _G.package or {}
_G.package.loaded = _G.package.loaded or {}

-- Load aos.lua
local ok, err = pcall(dofile, "aos.lua")
if not ok then
    log("Error loading aos.lua:", err)
    output_file:close()
    os.exit(1)
end

log("aos.lua loaded successfully")
log("compute exists:", type(compute))

-- Test compute
local state = {}
local assignment = {
    body = {
        type = "process",
        commitments = {
            key1 = {
                type = "RSA-PSS-512",
                committer = "TestOwner123456789012345678901234567890123"
            }
        }
    }
}

local status, result = compute(state, assignment)
log("Compute returned status:", status)

-- Check _G
log("_G.owner:", _G.owner or "nil")
log("_G.id:", _G.id or "nil")

-- Check result
if result then
    log("Result has owner:", result.owner or "nil")
    log("Result has id:", result.id or "nil")
    log("Result has meta:", result.meta and "yes" or "no")
end

log("Test completed")
output_file:close()
print("Test output written to test_output.txt")