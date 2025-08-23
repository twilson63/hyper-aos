local dump = { _version = "1.0.0" }

-- Reserved words that need to be quoted as keys
local RESERVED_WORDS = {
    ['and'] = true, ['break'] = true, ['do'] = true, ['else'] = true,
    ['elseif'] = true, ['end'] = true, ['false'] = true, ['for'] = true,
    ['function'] = true, ['if'] = true, ['in'] = true, ['local'] = true,
    ['nil'] = true, ['not'] = true, ['or'] = true, ['repeat'] = true,
    ['return'] = true, ['then'] = true, ['true'] = true, ['until'] = true, ['while'] = true
}

-- Check if a number is a valid unsigned integer
local function is_uint(v)
    return type(v) == 'number' and v >= 0 and math.floor(v) == v and v < math.huge
end

-- Default filter function
local function default_filter(val)
    return val
end

-- Main dump implementation
local function dump_value_impl(val, depth, indent_size, padding_size, filter, udata, seen)
    if depth == nil then
        depth = 0
    end
    if indent_size == nil then
        indent_size = 4
    end
    if padding_size == nil then
        padding_size = 0
    end
    if filter == nil then
        filter = default_filter
    end
    if seen == nil then
        seen = {}
    end
    
    -- Prevent infinite recursion by limiting depth
    if depth > 10 then
        return '"<max depth>"'
    end
    
    local val_type = type(val)
    
    if val_type == 'nil' then
        return '"nil"'
    elseif val_type == 'boolean' then
        return tostring(val)
    elseif val_type == 'number' then
        return tostring(val)
    elseif val_type == 'string' then
        return string.format('%q', val)
    elseif val_type == 'table' then
        -- Check for circular references
        local obj_id = tostring(val)
        if seen[obj_id] then
            return '"<Circular ' .. obj_id .. '>"'
        end
        seen[obj_id] = true
        
        local parts = {}
        local current_indent = string.rep(' ', padding_size)
        local field_indent = current_indent .. string.rep(' ', indent_size)
        
        -- Apply filter to each key-value pair
        for k, v in pairs(val) do
            local filtered_key, key_nodump = filter(k, depth, type(k), 'key', nil, udata)
            
            if filtered_key ~= nil then
                local filtered_val, val_nodump = filter(v, depth, type(v), 'val', filtered_key, udata)
                
                if filtered_val ~= nil then
                    local key_str
                    local k_type = type(filtered_key)
                    
                    -- Handle different key types
                    if k_type == 'string' and string.match(filtered_key, '^[a-zA-Z_][a-zA-Z0-9_]*$') and not RESERVED_WORDS[filtered_key] then
                        key_str = filtered_key
                    else
                        if k_type == 'string' then
                            key_str = '[' .. string.format('%q', filtered_key) .. ']'
                        else
                            key_str = '[' .. tostring(filtered_key) .. ']'
                        end
                    end
                    
                    local val_str
                    if type(filtered_val) == 'table' and not val_nodump then
                        val_str = dump._dump_internal(filtered_val, depth + 1, indent_size, padding_size + indent_size, filter, udata, seen)
                    else
                        val_str = dump._dump_internal(filtered_val, depth + 1, indent_size, padding_size, filter, udata, seen)
                    end
                    
                    parts[#parts + 1] = field_indent .. key_str .. ' = ' .. val_str
                end
            end
        end
        
        seen[obj_id] = nil  -- Clean up circular reference tracking
        
        if #parts == 0 then
            return '{}'
        end
        
        -- Determine line endings based on indentation
        if indent_size == 0 then
            return '{' .. ' ' .. table.concat(parts, ', ') .. ' ' .. '}'
        else
            return '{\n' .. table.concat(parts, ',\n') .. '\n' .. current_indent .. '}'
        end
    else
        return string.format('%q', tostring(val))
    end
end

-- Store the internal function in the module
dump._dump_internal = dump_value_impl

-- Main dump interface
local function dump_interface(val, indent, padding, filter, udata)
    -- Validate parameters
    if indent ~= nil and not is_uint(indent) then
        error('indent must be unsigned integer', 2)
    end
    if padding ~= nil and not is_uint(padding) then
        error('padding must be unsigned integer', 2)
    end
    if filter ~= nil and type(filter) ~= 'function' then
        error('filter must be function', 2)
    end
    
    -- Set defaults
    local indent_size = indent or 4
    local padding_size = padding or 0
    local filter_func = filter or default_filter
    
    -- For non-table values, apply filter and format appropriately
    if type(val) ~= 'table' then
        local filtered_val, nodump = filter_func(val, 0, type(val), 'val', nil, udata)
        if nodump == true then
            return tostring(filtered_val)
        end
        return string.format('%q', tostring(filtered_val))
    end
    
    return dump._dump_internal(val, 0, indent_size, padding_size, filter_func, udata, {})
end

dump.dump = dump_interface

return dump