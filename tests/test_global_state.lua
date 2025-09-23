-- Test that state is properly stored in _G and persisted across compute calls

print("Testing global state persistence...")

-- Load aos.lua
dofile("aos.lua")

-- Test 1: Initial compute call
print("\nTest 1: Initial state")
local state1 = {}
local assignment1 = {
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

local status1, result1 = compute(state1, assignment1)
print("Status:", status1)
print("Owner set to:", result1.owner or "nil")
print("ID set to:", result1.id or "nil")

-- Test 2: Eval to set a global variable
print("\nTest 2: Setting global variable via eval")
local assignment2 = {
    body = {
        action = "eval",
        body = "TestGlobal = 'Hello from _G'; return TestGlobal",
        from = "TestOwner123456789012345678901234567890123",
        commitments = {
            key1 = {
                type = "RSA-PSS-512", 
                committer = "TestOwner123456789012345678901234567890123"
            }
        }
    }
}

local status2, result2 = compute(result1, assignment2)
print("Status:", status2)
print("Output:", result2.results.output.data)

-- Test 3: Check if global persists
print("\nTest 3: Checking if global persists")
local assignment3 = {
    body = {
        action = "eval",
        body = "return TestGlobal or 'not found'",
        from = "TestOwner123456789012345678901234567890123",
        commitments = {
            key1 = {
                type = "RSA-PSS-512",
                committer = "TestOwner123456789012345678901234567890123"
            }
        }
    }
}

local status3, result3 = compute(result2, assignment3)
print("Status:", status3)
print("Output:", result3.results.output.data)
print("TestGlobal in state:", result3.TestGlobal or "nil")

-- Test 4: Check _G directly
print("\nTest 4: Direct _G check")
print("_G.TestGlobal:", _G.TestGlobal or "nil")
print("_G.owner:", _G.owner or "nil")
print("_G.id:", _G.id or "nil")

-- Test 5: Check Inbox persistence
print("\nTest 5: Inbox persistence")
local assignment4 = {
    body = {
        message = "Test message"
    }
}
local status4, result4 = compute(result3, assignment4)
print("Inbox size:", result4.Inbox and #result4.Inbox or 0)

print("\nAll tests completed!")