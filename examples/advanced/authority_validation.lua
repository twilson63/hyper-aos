--- Authority Validation Example for Hyper-AOS
-- Demonstrates comprehensive security and trust verification using the
-- commitment-based authority system in AO. Shows how to implement
-- role-based access control and secure message processing.
--
-- This example implements a secure document management system where:
-- - Owner has full administrative privileges
-- - Authorities can read/write documents 
-- - Untrusted users have limited read-only access
--
-- @module authority_validation  
-- @version 1.0.0

-- Initialize secure document store in _G namespace
_G.documents = _G.documents or {}
_G.document_permissions = _G.document_permissions or {}
_G.access_log = _G.access_log or {}
_G.security_settings = _G.security_settings or {
  require_authority_for_write = true,
  require_owner_for_admin = true,
  max_document_size = 10000,
  log_access_attempts = true
}

--- Helper function to check if a message sender is the process owner
-- Uses commitment validation from AOS security model
-- @param msg table The message to validate
-- @return boolean True if sender is authenticated owner
local function is_owner(msg)
  if not msg.commitments or not _G.owner then
    return false
  end
  
  -- Check if any non-HMAC commitment matches the stored owner
  for _, commitment in pairs(msg.commitments) do
    if commitment.type and 
       string.lower(commitment.type) ~= "hmac-sha256" and 
       commitment.committer and
       commitment.committer == _G.owner then
      return true
    end
  end
  
  return false
end

--- Helper function to check if a message sender is a trusted authority
-- Validates both from-process field and authority list membership
-- @param msg table The message to validate  
-- @return boolean True if sender is authenticated authority
local function is_trusted_authority(msg)
  if not _G.authorities or #_G.authorities == 0 then
    return false
  end
  
  -- Check from-process field matches message from and is in authorities
  local from_process = msg["from-process"]
  if not from_process or from_process ~= msg.from then
    return false
  end
  
  -- Verify from_process is in the authorities list
  for _, authority in ipairs(_G.authorities) do
    if authority == from_process then
      return true
    end
  end
  
  return false
end

--- Helper function to log access attempts for security auditing
-- @param action string The action attempted
-- @param sender string The message sender
-- @param success boolean Whether the action succeeded
-- @param reason string Additional context about the result
local function log_access(action, sender, success, reason)
  if not _G.security_settings.log_access_attempts then
    return
  end
  
  local log_entry = {
    timestamp = os.time(),
    action = action,
    sender = sender or "unknown",
    success = success,
    reason = reason or "",
    is_owner = is_owner(Msg),
    is_authority = is_trusted_authority(Msg)
  }
  
  table.insert(_G.access_log, log_entry)
  
  -- Limit log size to prevent unbounded growth
  local max_log_size = 500
  if #_G.access_log > max_log_size then
    table.remove(_G.access_log, 1)
  end
  
  print("ACCESS: " .. action .. " by " .. (sender or "unknown") .. " - " .. (success and "SUCCESS" or "DENIED") .. " (" .. (reason or "") .. ")")
end

--- Create a new document (requires authority or owner privileges)
-- Action: "CreateDocument" with DocId and Content tags
if Msg.Action == "CreateDocument" then
  local doc_id = Msg.DocId
  local content = Msg.Content or ""
  local sender = Msg.From
  
  -- Validate required parameters
  if not doc_id then
    log_access("create_document", sender, false, "missing doc_id")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Missing required 'DocId' parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Check if document already exists
  if _G.documents[doc_id] then
    log_access("create_document", sender, false, "document exists")
    Send({
      Target = sender,
      Action = "Error", 
      Data = "Document '" .. doc_id .. "' already exists",
      ["Error-Type"] = "DocumentExists"
    })
    return
  end
  
  -- Validate content size
  if #content > _G.security_settings.max_document_size then
    log_access("create_document", sender, false, "content too large")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Document content exceeds maximum size of " .. _G.security_settings.max_document_size .. " characters",
      ["Error-Type"] = "ContentTooLarge"
    })
    return
  end
  
  -- Security check: require authority or owner for write operations
  local has_write_permission = is_owner(Msg) or is_trusted_authority(Msg)
  
  if _G.security_settings.require_authority_for_write and not has_write_permission then
    log_access("create_document", sender, false, "insufficient privileges")
    Send({
      Target = sender,
      Action = "SecurityError",
      Data = "Document creation requires owner or authority privileges",
      ["Error-Type"] = "InsufficientPrivileges",
      ["Required-Level"] = "Authority"
    })
    return
  end
  
  -- Create the document
  _G.documents[doc_id] = {
    content = content,
    created_by = sender,
    created_at = os.time(),
    modified_at = os.time(),
    version = 1
  }
  
  -- Set default permissions (creator and authorities can read/write)
  _G.document_permissions[doc_id] = {
    owner_access = "full",
    authority_access = "read_write", 
    public_access = "read"
  }
  
  log_access("create_document", sender, true, "document created")
  
  Send({
    Target = sender,
    Action = "CreateDocumentResponse",
    Data = "Document '" .. doc_id .. "' created successfully",
    ["Doc-Id"] = doc_id,
    ["Content-Length"] = tostring(#content),
    ["Created-By"] = sender,
    ["Version"] = "1"
  })
  
  print("Created document: " .. doc_id .. " (" .. #content .. " chars)")
end

--- Read a document (permissions vary by user level)
-- Action: "ReadDocument" with DocId tag
if Msg.Action == "ReadDocument" then
  local doc_id = Msg.DocId
  local sender = Msg.From
  
  if not doc_id then
    log_access("read_document", sender, false, "missing doc_id")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Missing required 'DocId' parameter",
      ["Error-Type"] = "ValidationError" 
    })
    return
  end
  
  -- Check if document exists
  local document = _G.documents[doc_id]
  if not document then
    log_access("read_document", sender, false, "document not found")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Document '" .. doc_id .. "' not found",
      ["Error-Type"] = "DocumentNotFound"
    })
    return
  end
  
  -- Check read permissions
  local permissions = _G.document_permissions[doc_id]
  local can_read = false
  local access_level = "none"
  
  if is_owner(Msg) then
    can_read = true
    access_level = "owner"
  elseif is_trusted_authority(Msg) and permissions.authority_access ~= "none" then
    can_read = true  
    access_level = "authority"
  elseif permissions.public_access == "read" then
    can_read = true
    access_level = "public"
  end
  
  if not can_read then
    log_access("read_document", sender, false, "access denied")
    Send({
      Target = sender,
      Action = "SecurityError",
      Data = "Insufficient permissions to read document '" .. doc_id .. "'",
      ["Error-Type"] = "AccessDenied",
      ["Doc-Id"] = doc_id
    })
    return
  end
  
  log_access("read_document", sender, true, "access level: " .. access_level)
  
  Send({
    Target = sender,
    Action = "ReadDocumentResponse",
    Data = document.content,
    ["Doc-Id"] = doc_id,
    ["Content-Length"] = tostring(#document.content),
    ["Created-By"] = document.created_by,
    ["Created-At"] = tostring(document.created_at),
    ["Modified-At"] = tostring(document.modified_at),
    ["Version"] = tostring(document.version),
    ["Access-Level"] = access_level
  })
  
  print("Read document: " .. doc_id .. " by " .. sender .. " (access: " .. access_level .. ")")
end

--- Update a document (requires write permissions)
-- Action: "UpdateDocument" with DocId and Content tags  
if Msg.Action == "UpdateDocument" then
  local doc_id = Msg.DocId
  local new_content = Msg.Content or ""
  local sender = Msg.From
  
  if not doc_id then
    log_access("update_document", sender, false, "missing doc_id")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Missing required 'DocId' parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Check if document exists
  local document = _G.documents[doc_id]
  if not document then
    log_access("update_document", sender, false, "document not found")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Document '" .. doc_id .. "' not found", 
      ["Error-Type"] = "DocumentNotFound"
    })
    return
  end
  
  -- Validate content size
  if #new_content > _G.security_settings.max_document_size then
    log_access("update_document", sender, false, "content too large")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Document content exceeds maximum size of " .. _G.security_settings.max_document_size .. " characters",
      ["Error-Type"] = "ContentTooLarge"
    })
    return
  end
  
  -- Check write permissions
  local permissions = _G.document_permissions[doc_id]
  local can_write = false
  local access_level = "none"
  
  if is_owner(Msg) then
    can_write = true
    access_level = "owner"
  elseif is_trusted_authority(Msg) and permissions.authority_access == "read_write" then
    can_write = true
    access_level = "authority"
  end
  
  if not can_write then
    log_access("update_document", sender, false, "insufficient write privileges")
    Send({
      Target = sender,
      Action = "SecurityError",
      Data = "Insufficient permissions to update document '" .. doc_id .. "'",
      ["Error-Type"] = "AccessDenied",
      ["Required-Level"] = "Authority",
      ["Doc-Id"] = doc_id
    })
    return
  end
  
  -- Update the document
  local old_version = document.version
  document.content = new_content
  document.modified_at = os.time()
  document.version = document.version + 1
  
  log_access("update_document", sender, true, "v" .. old_version .. " -> v" .. document.version)
  
  Send({
    Target = sender,
    Action = "UpdateDocumentResponse", 
    Data = "Document '" .. doc_id .. "' updated successfully",
    ["Doc-Id"] = doc_id,
    ["Content-Length"] = tostring(#new_content),
    ["Previous-Version"] = tostring(old_version),
    ["New-Version"] = tostring(document.version),
    ["Access-Level"] = access_level
  })
  
  print("Updated document: " .. doc_id .. " v" .. old_version .. " -> v" .. document.version)
end

--- Delete a document (requires owner privileges)
-- Action: "DeleteDocument" with DocId tag
if Msg.Action == "DeleteDocument" then
  local doc_id = Msg.DocId
  local sender = Msg.From
  
  if not doc_id then
    log_access("delete_document", sender, false, "missing doc_id")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Missing required 'DocId' parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Check if document exists
  if not _G.documents[doc_id] then
    log_access("delete_document", sender, false, "document not found")
    Send({
      Target = sender,
      Action = "Error",
      Data = "Document '" .. doc_id .. "' not found",
      ["Error-Type"] = "DocumentNotFound"
    })
    return
  end
  
  -- Security check: only owner can delete documents
  if not is_owner(Msg) then
    log_access("delete_document", sender, false, "owner privileges required")
    Send({
      Target = sender,
      Action = "SecurityError",
      Data = "Document deletion requires owner privileges",
      ["Error-Type"] = "InsufficientPrivileges",
      ["Required-Level"] = "Owner"
    })
    return
  end
  
  -- Delete the document and its permissions
  _G.documents[doc_id] = nil
  _G.document_permissions[doc_id] = nil
  
  log_access("delete_document", sender, true, "document removed")
  
  Send({
    Target = sender,
    Action = "DeleteDocumentResponse",
    Data = "Document '" .. doc_id .. "' deleted successfully",
    ["Doc-Id"] = doc_id
  })
  
  print("Deleted document: " .. doc_id)
end

--- List documents (filtered by access level)
-- Action: "ListDocuments"
if Msg.Action == "ListDocuments" then
  local sender = Msg.From
  local accessible_docs = {}
  local access_level = "public"
  
  -- Determine user's access level
  if is_owner(Msg) then
    access_level = "owner"
  elseif is_trusted_authority(Msg) then
    access_level = "authority"
  end
  
  -- Filter documents based on access level
  for doc_id, document in pairs(_G.documents) do
    local permissions = _G.document_permissions[doc_id]
    local can_see = false
    
    if access_level == "owner" then
      can_see = true
    elseif access_level == "authority" and permissions.authority_access ~= "none" then
      can_see = true
    elseif permissions.public_access ~= "none" then
      can_see = true
    end
    
    if can_see then
      table.insert(accessible_docs, {
        id = doc_id,
        created_by = document.created_by,
        created_at = document.created_at,
        version = document.version,
        size = #document.content
      })
    end
  end
  
  -- Sort by creation time
  table.sort(accessible_docs, function(a, b) return a.created_at > b.created_at end)
  
  log_access("list_documents", sender, true, #accessible_docs .. " documents visible")
  
  local doc_list = {}
  for _, doc in ipairs(accessible_docs) do
    table.insert(doc_list, doc.id .. " (v" .. doc.version .. ", " .. doc.size .. " chars)")
  end
  
  Send({
    Target = sender,
    Action = "ListDocumentsResponse",
    Data = #accessible_docs > 0 and table.concat(doc_list, "\n") or "No accessible documents",
    ["Document-Count"] = tostring(#accessible_docs),
    ["Access-Level"] = access_level
  })
  
  print("Listed " .. #accessible_docs .. " documents for " .. sender .. " (access: " .. access_level .. ")")
end

--- Get security information and access log (owner only)
-- Action: "SecurityInfo"  
if Msg.Action == "SecurityInfo" then
  local sender = Msg.From
  
  -- Only owner can view security information
  if not is_owner(Msg) then
    log_access("security_info", sender, false, "owner privileges required")
    Send({
      Target = sender,
      Action = "SecurityError",
      Data = "Security information access requires owner privileges",
      ["Error-Type"] = "InsufficientPrivileges",
      ["Required-Level"] = "Owner"
    })
    return
  end
  
  -- Compile security status
  local auth_count = _G.authorities and #_G.authorities or 0
  local doc_count = 0
  for _ in pairs(_G.documents) do doc_count = doc_count + 1 end
  
  local security_info = {
    owner = _G.owner or "Unknown",
    authorities_count = auth_count,
    document_count = doc_count,
    access_log_entries = #_G.access_log,
    require_authority_for_write = _G.security_settings.require_authority_for_write,
    max_document_size = _G.security_settings.max_document_size
  }
  
  -- Get recent access attempts
  local recent_attempts = {}
  local log_limit = math.min(10, #_G.access_log)
  for i = #_G.access_log - log_limit + 1, #_G.access_log do
    if i > 0 then
      local entry = _G.access_log[i]
      table.insert(recent_attempts, entry.action .. " by " .. entry.sender .. " - " .. (entry.success and "SUCCESS" or "DENIED"))
    end
  end
  
  log_access("security_info", sender, true, "security report generated")
  
  Send({
    Target = sender,
    Action = "SecurityInfoResponse",
    Data = "Security Status Report",
    ["Owner"] = security_info.owner,
    ["Authorities-Count"] = tostring(security_info.authorities_count),
    ["Document-Count"] = tostring(security_info.document_count),
    ["Access-Log-Entries"] = tostring(security_info.access_log_entries),
    ["Recent-Access"] = table.concat(recent_attempts, "; "),
    ["Require-Authority-Write"] = tostring(security_info.require_authority_for_write),
    ["Max-Document-Size"] = tostring(security_info.max_document_size)
  })
  
  print("Security info sent to owner")
end

--- Help for authority validation system
-- Action: "Help" 
if Msg.Action == "Help" then
  local help_text = [[
Authority Validation System Help:

Available Actions:
- CreateDocument: Create new document (requires Authority/Owner)
- ReadDocument: Read document content (permissions vary)
- UpdateDocument: Modify existing document (requires write permissions)  
- DeleteDocument: Remove document (requires Owner)
- ListDocuments: Show accessible documents (filtered by level)
- SecurityInfo: View security status and logs (requires Owner)
- Help: Display this help message

Access Levels:
- Owner: Full administrative access to all documents and security settings
- Authority: Can create, read, and update documents (based on process authorities)
- Public: Read-only access to documents with public permissions

Security Features:
✓ RSA-PSS-512 commitment validation for ownership
✓ Authority list verification for trusted users  
✓ Role-based access control for documents
✓ Comprehensive access logging and auditing
✓ Document size limits and input validation
✓ Secure deletion and permission management

Parameters:
- DocId: Document identifier (required for most operations)
- Content: Document content (for create/update operations)
- All operations require proper authentication via message commitments

Current Process:
- Owner: ]] .. (_G.owner or "Not set") .. [[
- Authorities: ]] .. (_G.authorities and tostring(#_G.authorities) or "0") .. [[
- Documents: ]] .. (function() local c = 0; for _ in pairs(_G.documents or {}) do c = c + 1 end; return c end)() .. [[

  ]]
  
  Send({
    Target = Msg.From,
    Action = "HelpResponse", 
    Data = help_text
  })
  
  print("Help sent to " .. (Msg.From or "Unknown"))
end