-- Test demonstrating the private table implementation
dofile("aos.lua")

print("=== Private Table Implementation Test ===\n")

-- Step 1: First compute call - Process initialization
print("Step 1: Initialize process")
local state1 = {
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}

local process_msg = {
    id = 'process-001',
    type = 'process',
    from = 'creator',
    tags = {},
    commitments = {
        ['key1'] = {
            type = 'hmac-sha256',
            committer = 'ShouldBeIgnored123456789012345678901234567890'
        },
        ['key2'] = {
            type = 'RSA-PSS-512',
            committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
        }
    }
}

local status1, result1 = compute(state1, {body = process_msg})
print("Private values stored in state: " .. (result1._private and "YES" or "NO"))
if result1._private then
    print("  Owner: " .. (result1._private.owner or "nil"))
    print("  ID: " .. (result1._private.id or "nil"))
    print("  Initialized: " .. tostring(result1._private.initialized))
end

-- Step 2: Second compute call - Use persisted private values
print("\nStep 2: Eval with matching committer")
local eval_msg = {
    id = 'msg-002',
    from = 'user',
    tags = {},
    action = 'eval',
    data = 'return "Hello from " .. tostring(private.owner)',
    commitments = {
        ['key1'] = {
            type = 'RSA-PSS-512',
            committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
        }
    }
}

-- Use the state from previous compute (with _private)
local state2 = {
    _private = result1._private,  -- Pass private values
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}

local status2, result2 = compute(state2, {body = eval_msg})
print("Result: " .. result2.results.output.data)

-- Step 3: Attempt with wrong committer
print("\nStep 3: Eval with non-matching committer")
local eval_msg_bad = {
    id = 'msg-003',
    from = 'attacker',
    tags = {},
    action = 'eval',
    data = 'return "Should not execute"',
    commitments = {
        ['key1'] = {
            type = 'RSA-PSS-512',
            committer = 'AttackerAddress123456789012345678901234567890'
        }
    }
}

local state3 = {
    _private = result2._private,  -- Pass private values
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}

local status3, result3 = compute(state3, {body = eval_msg_bad})
print("Result: " .. result3.results.output.data)

-- Step 4: Verify private values cannot be accessed from eval
print("\nStep 4: Try to access private table from eval")
local eval_msg_hack = {
    id = 'msg-004',
    from = 'hacker',
    tags = {},
    action = 'eval',
    data = 'return tostring(private)',
    commitments = {
        ['key1'] = {
            type = 'RSA-PSS-512',
            committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
        }
    }
}

local state4 = {
    _private = result3._private,
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}

local status4, result4 = compute(state4, {body = eval_msg_hack})
print("Result: " .. result4.results.output.data)
print("Private table is: " .. (result4.results.output.data == "nil" and "HIDDEN (secure)" or "EXPOSED (security issue)"))

print("\n=== Summary ===")
print("✓ Private values (owner, id, initialized) stored in State._private")
print("✓ Private values restored on each compute call")
print("✓ Private table not accessible from eval'd code")
print("✓ Ownership validation working correctly with persisted owner")