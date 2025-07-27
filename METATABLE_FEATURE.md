# Process ID Metatable Feature

## Overview

The aos.lua module now includes metatable functionality that allows intuitive access to process data using familiar dot notation:

```lua
-- Instead of manually constructing paths:
ao.resolve("processId/now/supply")

-- You can now use:
_G['processId'].now.supply
```

## How It Works

1. **Process ID Detection**: When you access `_G['someId']`, the metatable checks if `someId` is a valid 43-character base64url process ID.

2. **Proxy Creation**: Valid process IDs return a proxy object that intercepts property access.

3. **Path Building**: Each property access builds a path: `processId.property.subproperty` becomes `processId/property/subproperty`.

4. **Terminal Properties**: Certain properties (like `supply`, `balance`, `owner`) automatically trigger `ao.resolve()` with the built path.

## Terminal Properties

The following properties trigger automatic resolution:
- Token properties: `supply`, `balance`, `owner`, `holders`, `votes`, `canVote`
- Time properties: `now`, `latest`, `timestamp`, `height`, `block`
- Transaction properties: `mint`, `minted`, `burned`, `transferred`, `staked`, `delegated`
- Data properties: `info`, `state`, `stats`, `metadata`, `profile`, `config`

## Examples

```lua
-- Access token balance
local balance = _G['AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0'].balance
-- Calls: ao.resolve("AR8wJBpKbBS7kZbHtUmB9V7VfQYxKCh7kyTkrLGS4N0/balance")

-- Access nested properties
local supply = _G['_T4Ip2y-7hh1iCAK9ilPMrs-nI5MRvt6s-eCoSz24-w'].now.supply
-- Calls: ao.resolve("_T4Ip2y-7hh1iCAK9ilPMrs-nI5MRvt6s-eCoSz24-w/now/supply")

-- Non-process-ID access works normally
_G['myVariable'] = 42  -- Regular variable assignment
```

## Implementation Details

- The metatable is set up automatically on first `compute()` call
- Uses `meta.isProcessId()` to validate 43-character base64url strings
- Creates lightweight proxy objects with `__index` metamethod
- Integrates with existing `ao.resolve()` function
- Preserves existing `_G` behavior for non-process-ID keys

## Testing

The feature includes comprehensive EUnit tests in `aos_metatable_test.erl` that verify:
- Process ID detection and validation
- Terminal property resolution
- Nested property chain building
- Error handling for invalid operations
- Integration with the compute function

## Notes

- In development/testing environments without proper ao.resolve, a placeholder implementation is provided
- The feature is transparent and doesn't affect existing aos functionality
- All process state continues to be stored in `_G` as before