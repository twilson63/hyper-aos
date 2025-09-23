-- Simple test to verify state storage in _G

-- Simulate what hyperbeam would do

-- First, set up the global environment
_G.package = _G.package or {}
_G.package.loaded = _G.package.loaded or {}

-- Load aos.lua
local ok, err = pcall(dofile, "aos.lua")
if not ok then
    print("Error loading aos.lua:", err)
    os.exit(1)
end

print("aos.lua loaded successfully")

-- Test that compute function exists
if type(compute) ~= "function" then
    print("ERROR: compute function not found")
    os.exit(1)
end

print("compute function found")

-- Create initial state and assignment
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

-- Call compute
local status, result = compute(state, assignment)

print("Compute returned:")
print("  Status:", status)
print("  Has results:", result and result.results and "yes" or "no")

-- Check if state was stored in _G
print("\nChecking _G after compute:")
print("  _G.owner:", _G.owner or "not set")
print("  _G.id:", _G.id or "not set")
print("  _G.meta exists:", _G.meta and "yes" or "no")
print("  _G.colors exists:", _G.colors and "yes" or "no")
print("  _G.Inbox exists:", _G.Inbox and "yes" or "no")

-- Check returned state
print("\nChecking returned state:")
print("  result.owner:", result and result.owner or "not set")
print("  result.id:", result and result.id or "not set")
print("  result.meta exists:", result and result.meta and "yes" or "no")
print("  result.colors exists:", result and result.colors and "yes" or "no")
print("  result.Inbox exists:", result and result.Inbox and "yes" or "no")

print("\nTest completed successfully!")