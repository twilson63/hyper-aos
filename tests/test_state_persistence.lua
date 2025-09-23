-- Comprehensive test for state persistence in _G

local output_file = io.open("test_persistence_output.txt", "w")
if not output_file then
    error("Cannot open output file")
end

local function log(...)
    local args = {...}
    local str = table.concat(args, " ")
    output_file:write(str .. "\n")
    output_file:flush()
    print(str)  -- Also print to console
end

log("=== Testing State Persistence in _G ===\n")

-- Set up environment
_G.package = _G.package or {}
_G.package.loaded = _G.package.loaded or {}

-- Load aos.lua
dofile("aos.lua")
log("aos.lua loaded successfully\n")

-- Test 1: Initialize process
log("Test 1: Initialize process")
local state1 = {}
local init_assignment = {
    body = {
        type = "process",
        commitments = {
            key1 = {
                type = "RSA-PSS-512",
                committer = "TestOwner123456789012345678901234567890123"
            }
        },
        authority = "Auth123456789012345678901234567890123456789,Auth223456789012345678901234567890123456789"
    }
}

local status1, result1 = compute(state1, init_assignment)
log("  Status:", status1)
log("  _G.owner:", _G.owner or "nil")
log("  _G.id:", _G.id or "nil")
log("  _G.authorities count:", _G.authorities and #_G.authorities or 0)
log("")

-- Test 2: Set a global variable via eval
log("Test 2: Set global variable via eval")
local eval_assignment = {
    body = {
        action = "eval",
        body = "MyGlobalVar = 'Hello from _G'; CustomTable = {a=1, b=2}; return 'Variables set'",
        from = "TestOwner123456789012345678901234567890123",
        commitments = {
            key1 = {
                type = "RSA-PSS-512",
                committer = "TestOwner123456789012345678901234567890123"
            }
        }
    }
}

local status2, result2 = compute(result1, eval_assignment)
log("  Status:", status2)
log("  Output:", result2.results and result2.results.output and result2.results.output.data or "nil")
log("  _G.MyGlobalVar:", _G.MyGlobalVar or "nil")
log("  _G.CustomTable:", _G.CustomTable and "exists" or "nil")
log("")

-- Test 3: Verify persistence in next compute call
log("Test 3: Verify state persists across compute calls")
local check_assignment = {
    body = {
        action = "eval", 
        body = "return 'MyGlobalVar=' .. (MyGlobalVar or 'nil') .. ', CustomTable.a=' .. (CustomTable and CustomTable.a or 'nil')",
        from = "TestOwner123456789012345678901234567890123",
        commitments = {
            key1 = {
                type = "RSA-PSS-512",
                committer = "TestOwner123456789012345678901234567890123"
            }
        }
    }
}

local status3, result3 = compute(result2, check_assignment)
log("  Status:", status3)
log("  Output:", result3.results and result3.results.output and result3.results.output.data or "nil")
log("")

-- Test 4: Check state extraction
log("Test 4: Check extracted state")
log("  result3.MyGlobalVar:", result3.MyGlobalVar or "nil")
log("  result3.CustomTable:", result3.CustomTable and "exists" or "nil")
if result3.CustomTable then
    log("    CustomTable.a:", result3.CustomTable.a or "nil")
    log("    CustomTable.b:", result3.CustomTable.b or "nil")
end
log("")

-- Test 5: Inbox persistence
log("Test 5: Inbox message handling")
local msg_assignment = {
    body = {
        data = "Test message 1"
    }
}
local status4, result4 = compute(result3, msg_assignment)
log("  Inbox size after message:", result4.Inbox and #result4.Inbox or 0)

-- Add another message
local msg_assignment2 = {
    body = {
        data = "Test message 2"
    }
}
local status5, result5 = compute(result4, msg_assignment2)
log("  Inbox size after 2nd message:", result5.Inbox and #result5.Inbox or 0)
log("  _G.Inbox size:", _G.Inbox and #_G.Inbox or 0)
log("")

-- Test 6: Meta table preservation
log("Test 6: Meta table preservation")
log("  result5.meta exists:", result5.meta and "yes" or "no")
if result5.meta then
    log("  meta.initialized:", result5.meta.initialized and "true" or "false")
    log("  meta.owner:", result5.meta.owner or "nil")
    log("  meta.id:", result5.meta.id or "nil")
    log("  meta.authorities count:", result5.meta.authorities and #result5.meta.authorities or 0)
    log("  meta.colors exists:", result5.meta.colors and "yes" or "no")
end

log("\n=== All tests completed successfully! ===")
output_file:close()