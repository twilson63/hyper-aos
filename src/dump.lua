local dump = { _version = "1.0.0" }

local type = type
local floor = math.floor
local tostring = tostring
local tblsort = table.sort
local tblconcat = table.concat
local strmatch = string.match
local strformat = string.format

local INFINITE_POS = math.huge
local LUA_FIELDNAME_PAT = '^[a-zA-Z_][a-zA-Z0-9_]*$'
local FOR_KEY = 'key'
local FOR_VAL = 'val'
local FOR_CIRCULAR = 'circular'

local RESERVED_WORD = {
    ['nil'] = true,
    ['true'] = true,
    ['false'] = true,
    ['local'] = true,
    ['function'] = true,
    ['and'] = true,
    ['or'] = true,
    ['not'] = true,
    ['if'] = true,
    ['elseif'] = true,
    ['else'] = true,
    ['for'] = true,
    ['in'] = true,
    ['while'] = true,
    ['until'] = true,
    ['repeat'] = true,
    ['break'] = true,
    ['goto'] = true,
    ['return'] = true,
    ['then'] = true,
    ['do'] = true,
    ['end'] = true,
}

local DEFAULT_INDENT = 4

local function DEFAULT_FILTER(val)
    return val
end

local function sort_index(a, b)
    if a.typ == b.typ then
        if a.typ == 'boolean' then
            return b.key
        end
        return a.key < b.key
    end
    return a.typ == 'number'
end

local function dumptbl(tbl, depth, indent, nestIndent, ctx)
    local ref = tostring(tbl)

    if ctx.circular[ref] then
        local val, nodump = ctx.filter(tbl, depth, type(tbl), FOR_CIRCULAR, tbl, ctx.udata)

        if val ~= nil and val ~= tbl then
            local t = type(val)

            if t == 'table' then
                if not nodump then
                    return dumptbl(val, depth + 1, indent, nestIndent, ctx)
                end
                return tostring(val)
            elseif t == 'string' then
                return strformat('%q', val)
            elseif t == 'number' or t == 'boolean' then
                return tostring(val)
            end

            return strformat('%q', tostring(val))
        end

        return '"<Circular ' .. ref .. '>"'
    end

    local res = {}
    local arr = {}
    local narr = 0
    local fieldIndent = indent .. nestIndent

    ctx.circular[ref] = true

    for k, v in pairs(tbl) do
        local key, nokdump = ctx.filter(k, depth, type(k), FOR_KEY, nil, ctx.udata)

        if key ~= nil then
            local val, novdump = ctx.filter(v, depth, type(v), FOR_VAL, key, ctx.udata)
            local kv

            if val ~= nil then
                local kt = type(key)
                local vt = type(val)

                if kt == 'number' or kt == 'boolean' then
                    k = key
                    key = '[' .. tostring(key) .. ']'
                elseif kt == 'table' and not nokdump then
                    key = '[' .. dumptbl(key, depth + 1, fieldIndent, nestIndent, ctx) .. ']'
                    k = key
                    kt = 'string'
                elseif kt ~= 'string' or RESERVED_WORD[key] or not strmatch(key, LUA_FIELDNAME_PAT) then
                    key = strformat("[%q]", tostring(key))
                    k = key
                    kt = 'string'
                end

                if vt == 'number' or vt == 'boolean' then
                    kv = strformat('%s%s = %s', fieldIndent, key, tostring(val))
                elseif vt == 'string' then
                    if not novdump then
                        kv = strformat('%s%s = %q', fieldIndent, key, val)
                    else
                        kv = strformat('%s%s = %s', fieldIndent, key, val)
                    end
                elseif vt == 'table' and not novdump then
                    kv = strformat('%s%s = %s', fieldIndent, key,
                                   dumptbl(val, depth + 1, fieldIndent, nestIndent, ctx))
                else
                    kv = strformat('%s%s = %q', fieldIndent, key, tostring(val))
                end

                narr = narr + 1
                arr[narr] = {
                    typ = kt,
                    key = k,
                    val = kv,
                }
            end
        end
    end

    ctx.circular[ref] = nil
    
    if narr > 0 then
        tblsort(arr, sort_index)

        for i = 1, narr do
            res[i] = arr[i].val
        end
        res[1] = '{' .. ctx.LF .. res[1]
        res = tblconcat(res, ',' .. ctx.LF) .. ctx.LF .. indent .. '}'
    else
        res = '{}'
    end

    return res
end

local function is_uint(v)
    return type(v) == 'number' and v < INFINITE_POS and v >= 0 and floor(v) == v
end

local function dump_value(val, indent, padding, filter, udata)
    local t = type(val)

    if indent == nil then
        indent = DEFAULT_INDENT
    elseif not is_uint(indent) then
        error('indent must be unsigned integer', 2)
    end

    if padding == nil then
        padding = 0
    elseif not is_uint(padding) then
        error('padding must be unsigned integer', 2)
    end

    if filter == nil then
        filter = DEFAULT_FILTER
    elseif type(filter) ~= 'function' then
        error('filter must be function', 2)
    end

    if t == 'table' then
        local ispace = ''
        local pspace = ''

        if indent > 0 then
            ispace = strformat('%' .. tostring(indent) .. 's', '')
        end

        if padding > 0 then
            pspace = strformat('%' .. tostring(padding) .. 's', '')
        end

        return dumptbl(val, 1, pspace, ispace, {
            LF = ispace == '' and ' ' or '\n',
            circular = {},
            filter = filter,
            udata = udata,
        })
    end

    local v, nodump = filter(val, 0, t, FOR_VAL, nil, udata)
    if nodump == true then
        return tostring(v)
    end
    return strformat('%q', tostring(v))
end

dump.dump = dump_value

return dump