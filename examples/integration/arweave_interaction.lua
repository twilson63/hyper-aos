--- Arweave Interaction Example for Hyper-AOS
-- Demonstrates advanced patterns for interacting with Arweave data and other
-- AO processes. Shows how to query data, interact with external processes,
-- and implement decentralized application patterns.
--
-- This example implements a decentralized content management system that
-- can store references to Arweave data, interact with other AO processes,
-- and coordinate distributed workflows.
--
-- @module arweave_interaction  
-- @version 1.0.0

-- Initialize Arweave interaction state in _G namespace
_G.arweave_refs = _G.arweave_refs or {}  -- Stored Arweave transaction references
_G.process_network = _G.process_network or {}  -- Known AO processes
_G.pending_queries = _G.pending_queries or {}  -- Pending external queries
_G.content_cache = _G.content_cache or {}  -- Cached content from Arweave
_G.interaction_stats = _G.interaction_stats or {
  refs_stored = 0,
  queries_sent = 0,
  responses_received = 0,
  cache_hits = 0,
  network_calls = 0
}

--- Helper function to validate Arweave transaction ID format
-- @param tx_id string The transaction ID to validate
-- @return boolean True if valid 43-character Arweave transaction ID
local function is_valid_arweave_tx(tx_id)
  return type(tx_id) == "string" and #tx_id == 43 and string.match(tx_id, "^[a-zA-Z0-9_-]+$")
end

--- Helper function to validate AO process ID format  
-- @param process_id string The process ID to validate
-- @return boolean True if valid 43-character AO process ID
local function is_valid_ao_process(process_id)
  return type(process_id) == "string" and #process_id == 43 and string.match(process_id, "^[a-zA-Z0-9_-]+$")
end

--- Helper function to generate query ID for tracking
-- @return string Unique query identifier
local function generate_query_id()
  return "qry_" .. os.time() .. "_" .. math.random(1000, 9999)
end

--- Store a reference to Arweave content
-- Action: "StoreArweaveRef" with TxId, ContentType, and optional Tags
if Msg.Action == "StoreArweaveRef" then
  local tx_id = Msg.TxId
  local content_type = Msg.ContentType or "unknown"
  local title = Msg.Title or "Untitled"
  local description = Msg.Description or ""
  local tags_str = Msg.Tags or ""
  
  -- Validate transaction ID
  if not tx_id or not is_valid_arweave_tx(tx_id) then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Invalid or missing Arweave transaction ID (must be 43 characters)",
      ["Error-Type"] = "InvalidTxId"
    })
    return
  end
  
  -- Check if reference already exists
  if _G.arweave_refs[tx_id] then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Reference to transaction " .. tx_id .. " already exists",
      ["Error-Type"] = "ReferenceExists"
    })
    return
  end
  
  -- Parse tags
  local tags = {}
  if #tags_str > 0 then
    for tag in string.gmatch(tags_str, "[^,]+") do
      tag = string.match(tag, "^%s*(.-)%s*$")  -- Trim whitespace
      if #tag > 0 then
        table.insert(tags, tag)
      end
    end
  end
  
  -- Store the reference
  _G.arweave_refs[tx_id] = {
    tx_id = tx_id,
    content_type = content_type,
    title = title,
    description = description,
    tags = tags,
    stored_by = Msg.From,
    stored_at = os.time(),
    access_count = 0,
    last_accessed = nil
  }
  
  -- Update statistics
  _G.interaction_stats.refs_stored = _G.interaction_stats.refs_stored + 1
  
  Send({
    Target = Msg.From,
    Action = "StoreArweaveRefResponse",
    Data = "Arweave reference stored successfully",
    ["Tx-Id"] = tx_id,
    ["Content-Type"] = content_type,
    ["Title"] = title,
    ["Tags-Count"] = tostring(#tags)
  })
  
  print("Stored Arweave reference: " .. tx_id .. " (" .. title .. ")")
end

--- Query stored Arweave references with filtering
-- Action: "QueryArweaveRefs" with optional ContentType, Tag, or Search parameters
if Msg.Action == "QueryArweaveRefs" then
  local content_type_filter = Msg.ContentType
  local tag_filter = Msg.Tag
  local search_term = Msg.Search
  local limit = tonumber(Msg.Limit) or 20
  
  local matched_refs = {}
  
  -- Filter references based on criteria
  for tx_id, ref in pairs(_G.arweave_refs) do
    local matches = true
    
    -- Content type filter
    if content_type_filter and ref.content_type ~= content_type_filter then
      matches = false
    end
    
    -- Tag filter
    if tag_filter and matches then
      local has_tag = false
      for _, tag in ipairs(ref.tags) do
        if tag == tag_filter then
          has_tag = true
          break
        end
      end
      if not has_tag then
        matches = false
      end
    end
    
    -- Search term filter (searches title and description)
    if search_term and matches then
      local search_lower = string.lower(search_term)
      local title_match = string.find(string.lower(ref.title), search_lower, 1, true)
      local desc_match = string.find(string.lower(ref.description), search_lower, 1, true)
      if not title_match and not desc_match then
        matches = false
      end
    end
    
    if matches then
      table.insert(matched_refs, ref)
    end
  end
  
  -- Sort by stored time (most recent first)
  table.sort(matched_refs, function(a, b) return a.stored_at > b.stored_at end)
  
  -- Limit results
  local limited_refs = {}
  for i = 1, math.min(limit, #matched_refs) do
    table.insert(limited_refs, matched_refs[i])
  end
  
  -- Format response
  local ref_descriptions = {}
  for _, ref in ipairs(limited_refs) do
    local tags_str = table.concat(ref.tags, ", ")
    local desc = ref.tx_id .. " - " .. ref.title .. " (" .. ref.content_type .. ")"
    if #tags_str > 0 then
      desc = desc .. " [" .. tags_str .. "]"
    end
    table.insert(ref_descriptions, desc)
  end
  
  Send({
    Target = Msg.From,
    Action = "QueryArweaveRefsResponse",
    Data = #limited_refs > 0 and table.concat(ref_descriptions, "\n") or "No matching references found",
    ["Results-Count"] = tostring(#limited_refs),
    ["Total-Matches"] = tostring(#matched_refs),
    ["Limit"] = tostring(limit)
  })
  
  print("Query returned " .. #limited_refs .. "/" .. #matched_refs .. " Arweave references")
end

--- Get detailed information about a specific Arweave reference
-- Action: "GetArweaveRef" with TxId parameter
if Msg.Action == "GetArweaveRef" then
  local tx_id = Msg.TxId
  
  if not tx_id then
    Send({
      Target = Msg.From,
      Action = "Error", 
      Data = "Missing required TxId parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  local ref = _G.arweave_refs[tx_id]
  if not ref then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Arweave reference " .. tx_id .. " not found",
      ["Error-Type"] = "ReferenceNotFound"
    })
    return
  end
  
  -- Update access statistics
  ref.access_count = ref.access_count + 1
  ref.last_accessed = os.time()
  
  Send({
    Target = Msg.From,
    Action = "GetArweaveRefResponse",
    Data = ref.description,
    ["Tx-Id"] = ref.tx_id,
    ["Content-Type"] = ref.content_type,
    ["Title"] = ref.title,
    ["Tags"] = table.concat(ref.tags, ", "),
    ["Stored-By"] = ref.stored_by,
    ["Stored-At"] = tostring(ref.stored_at),
    ["Access-Count"] = tostring(ref.access_count),
    ["Last-Accessed"] = ref.last_accessed and tostring(ref.last_accessed) or "never"
  })
  
  print("Retrieved Arweave reference: " .. tx_id)
end

--- Register an AO process for interaction  
-- Action: "RegisterAOProcess" with ProcessId, Name, and ProcessType
if Msg.Action == "RegisterAOProcess" then
  local process_id = Msg.ProcessId
  local process_name = Msg.Name or "Unnamed Process"
  local process_type = Msg.ProcessType or "generic"
  local endpoint_info = Msg.Endpoints or ""
  
  if not process_id or not is_valid_ao_process(process_id) then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Invalid or missing AO process ID (must be 43 characters)",
      ["Error-Type"] = "InvalidProcessId"
    })
    return
  end
  
  -- Parse endpoints
  local endpoints = {}
  if #endpoint_info > 0 then
    for endpoint in string.gmatch(endpoint_info, "[^,]+") do
      endpoint = string.match(endpoint, "^%s*(.-)%s*$")
      if #endpoint > 0 then
        table.insert(endpoints, endpoint)
      end
    end
  end
  
  _G.process_network[process_id] = {
    id = process_id,
    name = process_name,
    type = process_type,
    endpoints = endpoints,
    registered_by = Msg.From,
    registered_at = os.time(),
    last_interaction = nil,
    interaction_count = 0,
    status = "active"
  }
  
  Send({
    Target = Msg.From,
    Action = "RegisterAOProcessResponse", 
    Data = "AO process registered successfully",
    ["Process-Id"] = process_id,
    ["Name"] = process_name,
    ["Type"] = process_type,
    ["Endpoints-Count"] = tostring(#endpoints)
  })
  
  print("Registered AO process: " .. process_name .. " (" .. process_id .. ")")
end

--- Send a query to another AO process
-- Action: "QueryAOProcess" with ProcessId, QueryAction, and QueryData
if Msg.Action == "QueryAOProcess" then
  local process_id = Msg.ProcessId
  local query_action = Msg.QueryAction or "Query"
  local query_data = Msg.QueryData or ""
  local timeout = tonumber(Msg.Timeout) or 30
  
  if not process_id or not is_valid_ao_process(process_id) then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Invalid or missing target process ID", 
      ["Error-Type"] = "InvalidProcessId"
    })
    return
  end
  
  -- Check if process is registered (optional - can query unregistered processes)
  local process_info = _G.process_network[process_id]
  if process_info then
    process_info.interaction_count = process_info.interaction_count + 1
    process_info.last_interaction = os.time()
  end
  
  -- Generate query ID for tracking
  local query_id = generate_query_id()
  
  -- Store pending query
  _G.pending_queries[query_id] = {
    query_id = query_id,
    original_sender = Msg.From,
    target_process = process_id,
    query_action = query_action,
    query_data = query_data,
    sent_at = os.time(),
    timeout = timeout,
    status = "pending"
  }
  
  -- Send query to target process
  Send({
    Target = process_id,
    Action = query_action,
    Data = query_data,
    ["Query-Id"] = query_id,
    ["Response-Target"] = _G.id or "Unknown",
    ["Original-Requester"] = Msg.From,
    ["Timeout"] = tostring(timeout)
  })
  
  -- Update statistics
  _G.interaction_stats.queries_sent = _G.interaction_stats.queries_sent + 1
  _G.interaction_stats.network_calls = _G.interaction_stats.network_calls + 1
  
  -- Confirm query sent
  Send({
    Target = Msg.From,
    Action = "QueryAOProcessResponse",
    Data = "Query sent to " .. process_id,
    ["Query-Id"] = query_id,
    ["Target-Process"] = process_id,
    ["Query-Action"] = query_action,
    ["Timeout"] = tostring(timeout)
  })
  
  print("Sent query " .. query_id .. " to AO process: " .. process_id)
end

--- Handle incoming query responses from other AO processes
-- Action: "QueryResponse" with Query-Id tag
if Msg.Action == "QueryResponse" then
  local query_id = Msg["Query-Id"]
  local response_data = Msg.Data
  
  if not query_id then
    print("Received query response without Query-Id")
    return
  end
  
  local pending = _G.pending_queries[query_id]
  if not pending then
    print("Received response for unknown query: " .. query_id)
    return
  end
  
  -- Update query status
  pending.status = "completed"
  pending.response_received_at = os.time()
  pending.response_time = os.time() - pending.sent_at
  
  -- Forward response to original requester
  Send({
    Target = pending.original_sender,
    Action = "AOProcessQueryResponse",
    Data = response_data,
    ["Query-Id"] = query_id,
    ["Source-Process"] = Msg.From,
    ["Response-Time"] = tostring(pending.response_time) .. " seconds",
    ["Query-Action"] = pending.query_action
  })
  
  -- Update statistics
  _G.interaction_stats.responses_received = _G.interaction_stats.responses_received + 1
  
  -- Clean up completed query (keep for a short time for debugging)
  -- _G.pending_queries[query_id] = nil
  
  print("Forwarded query response for: " .. query_id)
end

--- Implement a distributed content publishing workflow
-- Action: "PublishContent" with ContentData, Title, and optional ArweaveTxId
if Msg.Action == "PublishContent" then
  local content_data = Msg.ContentData
  local title = Msg.Title or "Untitled Content"
  local description = Msg.Description or ""
  local arweave_tx_id = Msg.ArweaveTxId
  local notify_processes = Msg.NotifyProcesses  -- Comma-separated process IDs
  
  if not content_data then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required ContentData parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Generate content ID
  local content_id = "content_" .. os.time() .. "_" .. math.random(1000, 9999)
  
  -- Store content reference
  local content_ref = {
    id = content_id,
    title = title,
    description = description,
    data = content_data,
    arweave_tx_id = arweave_tx_id,
    published_by = Msg.From,
    published_at = os.time(),
    status = "published",
    access_count = 0
  }
  
  _G.content_cache[content_id] = content_ref
  
  -- If Arweave transaction ID provided, also store as Arweave reference
  if arweave_tx_id and is_valid_arweave_tx(arweave_tx_id) then
    _G.arweave_refs[arweave_tx_id] = {
      tx_id = arweave_tx_id,
      content_type = "published_content",
      title = title,
      description = description,
      tags = {"published", "content"},
      stored_by = Msg.From,
      stored_at = os.time(),
      access_count = 0,
      content_id = content_id  -- Link to local content
    }
  end
  
  -- Notify other processes if specified
  local notified_count = 0
  if notify_processes and #notify_processes > 0 then
    for process_id in string.gmatch(notify_processes, "[^,]+") do
      process_id = string.match(process_id, "^%s*(.-)%s*$")
      if is_valid_ao_process(process_id) then
        Send({
          Target = process_id,
          Action = "ContentPublished",
          Data = "New content published: " .. title,
          ["Content-Id"] = content_id,
          ["Title"] = title,
          ["Publisher"] = Msg.From,
          ["Published-At"] = tostring(content_ref.published_at),
          ["Arweave-Tx"] = arweave_tx_id or "none"
        })
        notified_count = notified_count + 1
      end
    end
  end
  
  Send({
    Target = Msg.From,
    Action = "PublishContentResponse",
    Data = "Content published successfully",
    ["Content-Id"] = content_id,
    ["Title"] = title,
    ["Arweave-Tx"] = arweave_tx_id or "none",
    ["Notified-Processes"] = tostring(notified_count)
  })
  
  print("Published content: " .. content_id .. " (" .. title .. ")")
end

--- Discover and list available AO processes in network
-- Action: "DiscoverProcesses" with optional Type filter
if Msg.Action == "DiscoverProcesses" then
  local type_filter = Msg.Type
  local include_inactive = Msg.IncludeInactive == "true"
  
  local discovered_processes = {}
  
  for process_id, process_info in pairs(_G.process_network) do
    local include = true
    
    -- Type filter
    if type_filter and process_info.type ~= type_filter then
      include = false
    end
    
    -- Status filter
    if not include_inactive and process_info.status ~= "active" then
      include = false
    end
    
    if include then
      table.insert(discovered_processes, {
        id = process_id,
        name = process_info.name,
        type = process_info.type,
        interaction_count = process_info.interaction_count,
        last_interaction = process_info.last_interaction
      })
    end
  end
  
  -- Sort by interaction count (most active first)
  table.sort(discovered_processes, function(a, b) return a.interaction_count > b.interaction_count end)
  
  local process_descriptions = {}
  for _, process in ipairs(discovered_processes) do
    local desc = process.name .. " (" .. process.type .. ", " .. process.interaction_count .. " interactions)"
    table.insert(process_descriptions, desc)
  end
  
  Send({
    Target = Msg.From,
    Action = "DiscoverProcessesResponse",
    Data = #discovered_processes > 0 and table.concat(process_descriptions, "\n") or "No processes discovered",
    ["Process-Count"] = tostring(#discovered_processes),
    ["Type-Filter"] = type_filter or "none",
    ["Include-Inactive"] = tostring(include_inactive)
  })
  
  print("Discovered " .. #discovered_processes .. " AO processes")
end

--- Get interaction statistics and network status
-- Action: "InteractionStats"
if Msg.Action == "InteractionStats" then
  local arweave_refs_count = 0
  for _ in pairs(_G.arweave_refs) do arweave_refs_count = arweave_refs_count + 1 end
  
  local network_processes_count = 0
  for _ in pairs(_G.process_network) do network_processes_count = network_processes_count + 1 end
  
  local pending_queries_count = 0
  local completed_queries_count = 0
  for _, query in pairs(_G.pending_queries) do
    if query.status == "pending" then
      pending_queries_count = pending_queries_count + 1
    else
      completed_queries_count = completed_queries_count + 1
    end
  end
  
  local cached_content_count = 0
  for _ in pairs(_G.content_cache) do cached_content_count = cached_content_count + 1 end
  
  Send({
    Target = Msg.From,
    Action = "InteractionStatsResponse",
    Data = "Arweave Interaction Statistics",
    ["Arweave-Refs"] = tostring(arweave_refs_count),
    ["Network-Processes"] = tostring(network_processes_count),
    ["Cached-Content"] = tostring(cached_content_count),
    ["Refs-Stored"] = tostring(_G.interaction_stats.refs_stored),
    ["Queries-Sent"] = tostring(_G.interaction_stats.queries_sent),
    ["Responses-Received"] = tostring(_G.interaction_stats.responses_received),
    ["Cache-Hits"] = tostring(_G.interaction_stats.cache_hits),
    ["Network-Calls"] = tostring(_G.interaction_stats.network_calls),
    ["Pending-Queries"] = tostring(pending_queries_count),
    ["Completed-Queries"] = tostring(completed_queries_count)
  })
  
  print("Interaction stats sent - " .. arweave_refs_count .. " Arweave refs, " .. network_processes_count .. " processes")
end

--- Help for Arweave interaction system
-- Action: "Help"
if Msg.Action == "Help" then
  local help_text = [[
Arweave Interaction System Help:

Arweave Data Management:
- StoreArweaveRef: Store reference to Arweave data (TxId, ContentType, Title, Description, Tags)
- QueryArweaveRefs: Search stored references (ContentType, Tag, Search, Limit)
- GetArweaveRef: Get detailed info about specific reference (TxId)

AO Process Network:
- RegisterAOProcess: Add process to network (ProcessId, Name, ProcessType, Endpoints)
- QueryAOProcess: Send query to another process (ProcessId, QueryAction, QueryData, Timeout)
- QueryResponse: Handle incoming query responses (Query-Id)
- DiscoverProcesses: List available processes (Type, IncludeInactive)

Content Publishing:
- PublishContent: Publish content with optional Arweave backing
  * ContentData: content to publish
  * Title, Description: metadata
  * ArweaveTxId: optional Arweave transaction reference
  * NotifyProcesses: comma-separated process IDs to notify

Statistics:
- InteractionStats: View system statistics and network status

Examples:
- Store Arweave ref: Action="StoreArweaveRef", TxId="<43-char-tx-id>", ContentType="image", Title="My Image"
- Query refs: Action="QueryArweaveRefs", ContentType="image", Limit="10"
- Register process: Action="RegisterAOProcess", ProcessId="<43-char-id>", Name="Data Processor", ProcessType="analytics"
- Query process: Action="QueryAOProcess", ProcessId="<43-char-id>", QueryAction="GetData", QueryData="latest"
- Publish content: Action="PublishContent", ContentData="Hello World", Title="My Post", NotifyProcesses="<id1>,<id2>"

Features:
✓ Arweave transaction reference management
✓ Distributed AO process network coordination  
✓ Query-response patterns with timeout handling
✓ Content publishing with cross-process notifications
✓ Process discovery and network mapping
✓ Comprehensive interaction statistics
✓ Caching and performance optimization
  ]]
  
  Send({
    Target = Msg.From,
    Action = "HelpResponse",
    Data = help_text
  })
  
  print("Help sent to " .. (Msg.From or "Unknown"))
end

--- Cleanup expired queries (run periodically)
local current_time = os.time()
for query_id, query in pairs(_G.pending_queries) do
  if query.status == "pending" and current_time - query.sent_at > query.timeout then
    -- Notify original sender of timeout
    Send({
      Target = query.original_sender,
      Action = "QueryTimeout",
      Data = "Query " .. query_id .. " timed out after " .. query.timeout .. " seconds",
      ["Query-Id"] = query_id,
      ["Target-Process"] = query.target_process,
      ["Timeout"] = tostring(query.timeout)
    })
    
    -- Mark as timed out
    query.status = "timeout"
    
    print("Query " .. query_id .. " timed out")
  end
end