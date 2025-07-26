-- Direct test of is_owner functionality
-- Run this from the hyper-aos-demo directory

dofile("aos.lua")

print("Testing committer-based ownership validation...")

-- Test helper function
local function test_eval(state, msg, expected)
    local assignment = {
        body = msg
    }
    local status, result = compute(state, assignment)
    local output = result.results.output.data
    if expected == "authorized" then
        return output == expected
    else
        return string.find(output, "Unauthorized") ~= nil
    end
end

-- Test 1: No owner set
local pass1 = test_eval(
    {results = {outbox = {}, output = {data = '', prompt = ''}}},
    {action = 'eval', data = 'return "authorized"'},
    "authorized"
)
print("Test 1 (no owner): " .. (pass1 and "PASS" or "FAIL"))

-- Test 2: Matching committer
local pass2 = test_eval(
    {
        owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
        results = {outbox = {}, output = {data = '', prompt = ''}}
    },
    {
        action = 'eval',
        data = 'return "authorized"',
        commitments = {
            key1 = {
                type = 'RSA-PSS-512',
                committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
            }
        }
    },
    "authorized"
)
print("Test 2 (matching committer): " .. (pass2 and "PASS" or "FAIL"))

-- Test 3: Different committer
local pass3 = test_eval(
    {
        owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
        results = {outbox = {}, output = {data = '', prompt = ''}}
    },
    {
        action = 'eval',
        data = 'return "fail"',
        commitments = {
            key1 = {
                type = 'RSA-PSS-512',
                committer = 'DifferentUser123456789012345678901234567890'
            }
        }
    },
    "unauthorized"
)
print("Test 3 (different committer): " .. (pass3 and "PASS" or "FAIL"))

-- Test 4: HMAC commitment (should be rejected)
local pass4 = test_eval(
    {
        owner = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0',
        results = {outbox = {}, output = {data = '', prompt = ''}}
    },
    {
        action = 'eval',
        data = 'return "fail"',
        commitments = {
            key1 = {
                type = 'hmac-sha256',
                committer = 'AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'
            }
        }
    },
    "unauthorized"
)
print("Test 4 (hmac commitment): " .. (pass4 and "PASS" or "FAIL"))

local all_pass = pass1 and pass2 and pass3 and pass4
print("\nOverall: " .. (all_pass and "ALL TESTS PASSED" or "SOME TESTS FAILED"))