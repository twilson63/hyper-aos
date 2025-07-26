-- Test script to verify committer-based ownership validation

print("Loading aos.lua...")
dofile("../aos.lua")
print("aos.lua loaded successfully")

-- Test 1: No owner set - should allow eval
print("Test 1: No owner set")
local state1 = {
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}
local assignment1 = {
    body = {
        id = 'test',
        from = 'test',
        owner = 'test',
        tags = {},
        action = 'eval',
        data = 'return "allowed"'
    }
}
local status1, result1 = compute(state1, assignment1)
print("Result: " .. result1.results.output.data)
assert(result1.results.output.data == "allowed", "Test 1 failed")

-- Test 2: Owner set, matching committer - should allow
print("\nTest 2: Owner set, matching committer")
local state2 = {
    owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}
local assignment2 = {
    body = {
        id = 'test',
        from = 'test',
        owner = 'test',
        tags = {},
        action = 'eval',
        data = 'return "authorized"',
        commitments = {
            ['key1'] = {
                type = 'RSA-PSS-512',
                committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
            }
        }
    }
}
local status2, result2 = compute(state2, assignment2)
print("Result: " .. result2.results.output.data)
assert(result2.results.output.data == "authorized", "Test 2 failed")

-- Test 3: Owner set, different committer - should fail
print("\nTest 3: Owner set, different committer")
local state3 = {
    owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}
local assignment3 = {
    body = {
        id = 'test',
        from = 'test',
        owner = 'test',
        tags = {},
        action = 'eval',
        data = 'return "should fail"',
        commitments = {
            ['key1'] = {
                type = 'RSA-PSS-512',
                committer = 'DifferentUser123456789012345678901234567890'
            }
        }
    }
}
local status3, result3 = compute(state3, assignment3)
print("Result: " .. result3.results.output.data)
assert(string.find(result3.results.output.data, "Unauthorized") ~= nil, "Test 3 failed")

-- Test 4: Owner set, hmac-sha256 commitment - should fail
print("\nTest 4: Owner set, hmac-sha256 commitment")
local state4 = {
    owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}
local assignment4 = {
    body = {
        id = 'test',
        from = 'test',
        owner = 'test',
        tags = {},
        action = 'eval',
        data = 'return "should fail"',
        commitments = {
            ['key1'] = {
                type = 'hmac-sha256',
                committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
            }
        }
    }
}
local status4, result4 = compute(state4, assignment4)
print("Result: " .. result4.results.output.data)
assert(string.find(result4.results.output.data, "Unauthorized") ~= nil, "Test 4 failed")

-- Test 5: Owner set, RSA-PSS-256 commitment - should fail
print("\nTest 5: Owner set, RSA-PSS-256 commitment")
local state5 = {
    owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}
local assignment5 = {
    body = {
        id = 'test',
        from = 'test',
        owner = 'test',
        tags = {},
        action = 'eval',
        data = 'return "should fail"',
        commitments = {
            ['key1'] = {
                type = 'RSA-PSS-256',
                committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
            }
        }
    }
}
local status5, result5 = compute(state5, assignment5)
print("Result: " .. result5.results.output.data)
assert(string.find(result5.results.output.data, "Unauthorized") ~= nil, "Test 5 failed")

-- Test 6: Multiple commitments, one matching RSA-PSS-512 - should allow
print("\nTest 6: Multiple commitments, one matching RSA-PSS-512")
local state6 = {
    owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
    results = {
        outbox = {},
        output = {data = '', prompt = ''}
    }
}
local assignment6 = {
    body = {
        id = 'test',
        from = 'test',
        owner = 'test',
        tags = {},
        action = 'eval',
        data = 'return "authorized"',
        commitments = {
            ['key1'] = {
                type = 'hmac-sha256',
                committer = 'SomeOtherUser123456789012345678901234567890'
            },
            ['key2'] = {
                type = 'RSA-PSS-512',
                committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
            },
            ['key3'] = {
                type = 'RSA-PSS-512',
                committer = 'DifferentUser123456789012345678901234567890'
            }
        }
    }
}
local status6, result6 = compute(state6, assignment6)
print("Result: " .. result6.results.output.data)
assert(result6.results.output.data == "authorized", "Test 6 failed")

print("\nAll tests passed!")