--- Functional Programming Example for Hyper-AOS
-- Demonstrates comprehensive usage of the utils module for functional
-- programming patterns in AO message processing. Shows real-world applications
-- of map, filter, reduce, compose, and other utilities.
--
-- This example implements a data processing pipeline that analyzes
-- incoming messages using functional programming techniques.
--
-- @module functional_programming
-- @version 1.0.0

-- Initialize data processing state in _G namespace
_G.message_analytics = _G.message_analytics or {
  total_processed = 0,
  by_action = {},
  by_sender = {},
  processing_history = {}
}

_G.data_sets = _G.data_sets or {}  -- Stored data for analysis
_G.processing_pipelines = _G.processing_pipelines or {}  -- Configured pipelines

--- Helper function to log processing steps
-- @param step string The processing step name
-- @param data any The data being processed
-- @param result any The result of processing
local function log_processing(step, data, result)
  local log_entry = {
    timestamp = os.time(),
    step = step,
    input_type = type(data),
    output_type = type(result),
    input_size = type(data) == "table" and #data or 1,
    output_size = type(result) == "table" and #result or 1
  }
  
  table.insert(_G.message_analytics.processing_history, log_entry)
  
  -- Limit history size
  if #_G.message_analytics.processing_history > 100 then
    table.remove(_G.message_analytics.processing_history, 1)
  end
  
  print("PROCESS: " .. step .. " - " .. log_entry.input_type .. "(" .. log_entry.input_size .. ") -> " .. log_entry.output_type .. "(" .. log_entry.output_size .. ")")
end

--- Process an array of numbers using functional utilities
-- Action: "ProcessNumbers" with Numbers tag (comma-separated values)
if Msg.Action == "ProcessNumbers" then
  local numbers_str = Msg.Numbers
  local operation = Msg.Operation or "analyze"  -- analyze, square, filter, sum
  
  if not numbers_str then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required Numbers parameter (comma-separated values)",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Parse numbers from string
  local numbers = {}
  for num_str in string.gmatch(numbers_str, "[^,]+") do
    local num = tonumber(string.match(num_str, "^%s*(.-)%s*$"))
    if num then
      table.insert(numbers, num)
    end
  end
  
  if #numbers == 0 then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "No valid numbers found in input",
      ["Error-Type"] = "InvalidData"
    })
    return
  end
  
  log_processing("parse_numbers", numbers_str, numbers)
  
  local result = {}
  local operation_description = ""
  
  if operation == "square" then
    -- Use utils.map to square all numbers
    result = utils.map(function(x) return x * x end, numbers)
    operation_description = "Squared all numbers using utils.map"
    log_processing("map_square", numbers, result)
    
  elseif operation == "filter" then
    -- Use utils.filter to get only even numbers
    result = utils.filter(function(x) return x % 2 == 0 end, numbers)
    operation_description = "Filtered even numbers using utils.filter"
    log_processing("filter_even", numbers, result)
    
  elseif operation == "sum" then  
    -- Use utils.reduce to sum all numbers
    local sum = utils.reduce(function(acc, x) return acc + x end, 0, numbers)
    result = {sum}
    operation_description = "Calculated sum using utils.reduce: " .. sum
    log_processing("reduce_sum", numbers, result)
    
  elseif operation == "analyze" then
    -- Complex analysis using multiple utils functions
    local even_numbers = utils.filter(function(x) return x % 2 == 0 end, numbers)
    local odd_numbers = utils.filter(function(x) return x % 2 ~= 0 end, numbers)
    local squares = utils.map(function(x) return x * x end, numbers)
    local sum = utils.reduce(function(acc, x) return acc + x end, 0, numbers)
    local max_val = utils.reduce(function(acc, x) return x > acc and x or acc end, numbers[1], numbers)
    local min_val = utils.reduce(function(acc, x) return x < acc and x or acc end, numbers[1], numbers)
    
    result = {
      count = #numbers,
      sum = sum,
      average = sum / #numbers,
      min = min_val,
      max = max_val,
      even_count = #even_numbers,
      odd_count = #odd_numbers,
      sum_of_squares = utils.reduce(function(acc, x) return acc + x end, 0, squares)
    }
    
    operation_description = "Complete analysis using multiple utils functions"
    log_processing("analyze_complete", numbers, result)
    
  else
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Unknown operation: " .. operation .. ". Available: square, filter, sum, analyze",
      ["Error-Type"] = "UnknownOperation"
    })
    return
  end
  
  -- Update analytics
  _G.message_analytics.total_processed = _G.message_analytics.total_processed + 1
  _G.message_analytics.by_action["ProcessNumbers"] = (_G.message_analytics.by_action["ProcessNumbers"] or 0) + 1
  _G.message_analytics.by_sender[Msg.From] = (_G.message_analytics.by_sender[Msg.From] or 0) + 1
  
  -- Format result for response
  local result_str = ""
  if type(result) == "table" and result.count then
    -- Analysis result
    result_str = "Count: " .. result.count .. ", Sum: " .. result.sum .. ", Avg: " .. string.format("%.2f", result.average) .. 
                ", Min: " .. result.min .. ", Max: " .. result.max .. ", Even: " .. result.even_count .. ", Odd: " .. result.odd_count
  else
    -- Array result
    local formatted_nums = utils.map(function(x) return tostring(x) end, result)
    result_str = table.concat(formatted_nums, ", ")
  end
  
  Send({
    Target = Msg.From,
    Action = "ProcessNumbersResponse",
    Data = result_str,
    ["Operation"] = operation,
    ["Input-Count"] = tostring(#numbers),
    ["Output-Count"] = tostring(type(result) == "table" and #result or 1),
    ["Description"] = operation_description
  })
  
  print("Processed " .. #numbers .. " numbers with operation: " .. operation)
end

--- Process text data using functional string operations
-- Action: "ProcessText" with Text and Operation tags
if Msg.Action == "ProcessText" then
  local text = Msg.Text
  local operation = Msg.Operation or "analyze"  -- analyze, words, filter, transform
  
  if not text then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required Text parameter",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  log_processing("input_text", text, text)
  
  local result = {}
  local operation_description = ""
  
  if operation == "words" then
    -- Split text into words and process with utils
    local words = {}
    for word in string.gmatch(text, "%S+") do
      table.insert(words, word)
    end
    
    -- Use utils.map to convert to lowercase and remove punctuation
    local clean_words = utils.map(function(word)
      return string.lower(string.gsub(word, "[%p%c%s]", ""))
    end, words)
    
    -- Filter out empty strings
    result = utils.filter(function(word) return #word > 0 end, clean_words)
    operation_description = "Extracted and cleaned " .. #result .. " words"
    log_processing("extract_words", words, result)
    
  elseif operation == "filter" then
    -- Extract words and filter by length using utils.filter
    local words = {}
    for word in string.gmatch(text, "%S+") do
      table.insert(words, string.lower(string.gsub(word, "[%p%c%s]", "")))
    end
    
    local min_length = tonumber(Msg.MinLength) or 4
    result = utils.filter(function(word) return #word >= min_length end, words)
    operation_description = "Filtered words with length >= " .. min_length
    log_processing("filter_words", words, result)
    
  elseif operation == "transform" then
    -- Split into words and apply transformations using utils.map
    local words = {}
    for word in string.gmatch(text, "%S+") do
      table.insert(words, word)
    end
    
    -- Chain transformations using utils.compose (if available) or manual chaining
    local transform_word = function(word)
      local cleaned = string.gsub(word, "[%p%c%s]", "")
      local lowered = string.lower(cleaned)
      local reversed = string.reverse(lowered)
      return reversed
    end
    
    result = utils.map(transform_word, words)
    result = utils.filter(function(word) return #word > 0 end, result)
    operation_description = "Applied transformation chain: clean -> lowercase -> reverse"
    log_processing("transform_words", words, result)
    
  elseif operation == "analyze" then
    -- Comprehensive text analysis using utils functions
    local words = {}
    for word in string.gmatch(text, "%S+") do
      local clean_word = string.lower(string.gsub(word, "[%p%c%s]", ""))
      if #clean_word > 0 then
        table.insert(words, clean_word)
      end
    end
    
    -- Word frequency analysis
    local word_counts = {}
    for _, word in ipairs(words) do
      word_counts[word] = (word_counts[word] or 0) + 1
    end
    
    -- Find most common words
    local word_freq_list = {}
    for word, count in pairs(word_counts) do
      table.insert(word_freq_list, {word = word, count = count})
    end
    
    -- Sort by frequency (manual sort since we don't have utils.sort)
    table.sort(word_freq_list, function(a, b) return a.count > b.count end)
    
    -- Get word lengths using utils.map
    local word_lengths = utils.map(function(word) return #word end, words)
    local total_length = utils.reduce(function(acc, len) return acc + len end, 0, word_lengths)
    local avg_length = #words > 0 and (total_length / #words) or 0
    
    result = {
      char_count = #text,
      word_count = #words,
      unique_words = #word_freq_list,
      avg_word_length = avg_length,
      most_common = #word_freq_list > 0 and word_freq_list[1].word or "none",
      most_common_count = #word_freq_list > 0 and word_freq_list[1].count or 0
    }
    
    operation_description = "Complete text analysis with word frequency"
    log_processing("analyze_text", text, result)
    
  else
    Send({
      Target = Msg.From,
      Action = "Error", 
      Data = "Unknown operation: " .. operation .. ". Available: words, filter, transform, analyze",
      ["Error-Type"] = "UnknownOperation"
    })
    return
  end
  
  -- Update analytics
  _G.message_analytics.total_processed = _G.message_analytics.total_processed + 1
  _G.message_analytics.by_action["ProcessText"] = (_G.message_analytics.by_action["ProcessText"] or 0) + 1
  _G.message_analytics.by_sender[Msg.From] = (_G.message_analytics.by_sender[Msg.From] or 0) + 1
  
  -- Format result for response
  local result_str = ""
  if type(result) == "table" and result.char_count then
    -- Analysis result
    result_str = "Chars: " .. result.char_count .. ", Words: " .. result.word_count .. ", Unique: " .. result.unique_words ..
                ", Avg Length: " .. string.format("%.1f", result.avg_word_length) .. 
                ", Most Common: '" .. result.most_common .. "' (" .. result.most_common_count .. "x)"
  else
    -- Array result (limit to first 10 items for readability)
    local display_items = {}
    for i = 1, math.min(10, #result) do
      table.insert(display_items, tostring(result[i]))
    end
    result_str = table.concat(display_items, ", ")
    if #result > 10 then
      result_str = result_str .. " ... (+" .. (#result - 10) .. " more)"
    end
  end
  
  Send({
    Target = Msg.From,
    Action = "ProcessTextResponse",
    Data = result_str,
    ["Operation"] = operation,
    ["Input-Length"] = tostring(#text),
    ["Output-Count"] = tostring(type(result) == "table" and #result or 1),
    ["Description"] = operation_description
  })
  
  print("Processed text (" .. #text .. " chars) with operation: " .. operation)
end

--- Store a dataset for later processing
-- Action: "StoreDataset" with DatasetId and Data tags
if Msg.Action == "StoreDataset" then
  local dataset_id = Msg.DatasetId
  local data_str = Msg.Data
  local data_type = Msg.DataType or "numbers"  -- numbers, words, json
  
  if not dataset_id or not data_str then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required DatasetId and Data parameters",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  -- Parse data based on type
  local dataset = {}
  if data_type == "numbers" then
    for num_str in string.gmatch(data_str, "[^,]+") do
      local num = tonumber(string.match(num_str, "^%s*(.-)%s*$"))
      if num then
        table.insert(dataset, num)
      end
    end
  elseif data_type == "words" then
    for word in string.gmatch(data_str, "%S+") do
      table.insert(dataset, word)
    end
  else
    -- Treat as raw string data
    dataset = {data_str}
  end
  
  _G.data_sets[dataset_id] = {
    id = dataset_id,
    data = dataset,
    type = data_type,
    created_by = Msg.From,
    created_at = os.time(),
    size = #dataset
  }
  
  log_processing("store_dataset", data_str, dataset)
  
  Send({
    Target = Msg.From,
    Action = "StoreDatasetResponse",
    Data = "Dataset '" .. dataset_id .. "' stored successfully",
    ["Dataset-Id"] = dataset_id,
    ["Data-Type"] = data_type,
    ["Size"] = tostring(#dataset)
  })
  
  print("Stored dataset: " .. dataset_id .. " (" .. #dataset .. " items)")
end

--- Apply functional operations to stored datasets  
-- Action: "ProcessDataset" with DatasetId and Operations tags
if Msg.Action == "ProcessDataset" then
  local dataset_id = Msg.DatasetId
  local operations_str = Msg.Operations  -- Comma-separated: map_square,filter_even,reduce_sum
  
  if not dataset_id or not operations_str then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Missing required DatasetId and Operations parameters",
      ["Error-Type"] = "ValidationError"
    })
    return
  end
  
  local dataset_info = _G.data_sets[dataset_id]
  if not dataset_info then
    Send({
      Target = Msg.From,
      Action = "Error",
      Data = "Dataset '" .. dataset_id .. "' not found",
      ["Error-Type"] = "DatasetNotFound"
    })
    return
  end
  
  local data = utils.concat({}, dataset_info.data)  -- Make a copy
  local results = {}
  local operation_log = {}
  
  -- Parse and apply operations in sequence
  for operation in string.gmatch(operations_str, "[^,]+") do
    operation = string.match(operation, "^%s*(.-)%s*$")  -- Trim whitespace
    
    local operation_result = nil
    local operation_desc = ""
    
    if operation == "map_square" then
      if dataset_info.type == "numbers" then
        operation_result = utils.map(function(x) return x * x end, data)
        operation_desc = "Squared all values"
      else
        operation_result = data
        operation_desc = "Skipped (not numeric data)"
      end
      
    elseif operation == "map_double" then
      if dataset_info.type == "numbers" then
        operation_result = utils.map(function(x) return x * 2 end, data)
        operation_desc = "Doubled all values"
      else
        operation_result = data
        operation_desc = "Skipped (not numeric data)"
      end
      
    elseif operation == "filter_even" then
      if dataset_info.type == "numbers" then
        operation_result = utils.filter(function(x) return x % 2 == 0 end, data)
        operation_desc = "Filtered even numbers"
      else
        operation_result = data
        operation_desc = "Skipped (not numeric data)"
      end
      
    elseif operation == "filter_positive" then  
      if dataset_info.type == "numbers" then
        operation_result = utils.filter(function(x) return x > 0 end, data)
        operation_desc = "Filtered positive numbers"
      else
        operation_result = data
        operation_desc = "Skipped (not numeric data)"
      end
      
    elseif operation == "filter_long" then
      if dataset_info.type == "words" then
        operation_result = utils.filter(function(word) return #word > 4 end, data)
        operation_desc = "Filtered words longer than 4 characters"
      else
        operation_result = data
        operation_desc = "Skipped (not word data)"
      end
      
    elseif operation == "reduce_sum" then
      if dataset_info.type == "numbers" then
        local sum = utils.reduce(function(acc, x) return acc + x end, 0, data)
        operation_result = {sum}
        operation_desc = "Calculated sum: " .. sum
      else
        operation_result = data
        operation_desc = "Skipped (not numeric data)"
      end
      
    elseif operation == "reduce_concat" then
      if dataset_info.type == "words" then
        local concat = utils.reduce(function(acc, word) return acc .. " " .. word end, "", data)
        operation_result = {string.match(concat, "^%s*(.-)%s*$")}  -- Trim
        operation_desc = "Concatenated all words"
      else
        operation_result = data
        operation_desc = "Skipped (not word data)"
      end
      
    else
      operation_result = data
      operation_desc = "Unknown operation: " .. operation
    end
    
    -- Update data for next operation
    data = operation_result or data
    
    -- Log the operation
    table.insert(operation_log, operation .. " -> " .. operation_desc .. " (" .. #data .. " items)")
    log_processing("dataset_" .. operation, dataset_info.data, data)
  end
  
  -- Format final result
  local final_result_str = ""
  if #data <= 10 then
    local formatted_items = utils.map(function(x) return tostring(x) end, data)
    final_result_str = table.concat(formatted_items, ", ")
  else
    final_result_str = "Large result set with " .. #data .. " items"
  end
  
  -- Update analytics
  _G.message_analytics.total_processed = _G.message_analytics.total_processed + 1
  _G.message_analytics.by_action["ProcessDataset"] = (_G.message_analytics.by_action["ProcessDataset"] or 0) + 1
  _G.message_analytics.by_sender[Msg.From] = (_G.message_analytics.by_sender[Msg.From] or 0) + 1
  
  Send({
    Target = Msg.From,
    Action = "ProcessDatasetResponse",
    Data = final_result_str,
    ["Dataset-Id"] = dataset_id,
    ["Original-Size"] = tostring(#dataset_info.data),
    ["Final-Size"] = tostring(#data),
    ["Operations"] = operations_str,
    ["Operation-Log"] = table.concat(operation_log, "; ")
  })
  
  print("Processed dataset " .. dataset_id .. " with operations: " .. operations_str)
end

--- List stored datasets
-- Action: "ListDatasets"
if Msg.Action == "ListDatasets" then
  local dataset_list = {}
  local total_items = 0
  
  for dataset_id, dataset_info in pairs(_G.data_sets) do
    table.insert(dataset_list, {
      id = dataset_id,
      type = dataset_info.type,
      size = dataset_info.size,
      created_at = dataset_info.created_at
    })
    total_items = total_items + dataset_info.size
  end
  
  -- Sort by creation time
  table.sort(dataset_list, function(a, b) return a.created_at > b.created_at end)
  
  local dataset_descriptions = {}
  for _, dataset in ipairs(dataset_list) do
    table.insert(dataset_descriptions, dataset.id .. " (" .. dataset.type .. ", " .. dataset.size .. " items)")
  end
  
  Send({
    Target = Msg.From,
    Action = "ListDatasetsResponse",
    Data = #dataset_list > 0 and table.concat(dataset_descriptions, "\n") or "No datasets stored",
    ["Dataset-Count"] = tostring(#dataset_list),
    ["Total-Items"] = tostring(total_items)
  })
  
  print("Listed " .. #dataset_list .. " datasets with " .. total_items .. " total items")
end

--- Get processing analytics and statistics
-- Action: "Analytics"
if Msg.Action == "Analytics" then
  local total_messages = _G.message_analytics.total_processed
  local action_stats = {}
  local sender_stats = {}
  
  for action, count in pairs(_G.message_analytics.by_action) do
    table.insert(action_stats, action .. ": " .. count)
  end
  
  for sender, count in pairs(_G.message_analytics.by_sender) do
    table.insert(sender_stats, sender .. ": " .. count)
  end
  
  local recent_processing = {}
  local history_limit = math.min(5, #_G.message_analytics.processing_history)
  for i = #_G.message_analytics.processing_history - history_limit + 1, #_G.message_analytics.processing_history do
    if i > 0 then
      local entry = _G.message_analytics.processing_history[i]
      table.insert(recent_processing, entry.step .. " (" .. entry.input_size .. "->" .. entry.output_size .. ")")
    end
  end
  
  Send({
    Target = Msg.From,
    Action = "AnalyticsResponse",
    Data = "Processing Analytics Report",
    ["Total-Messages"] = tostring(total_messages),
    ["Action-Stats"] = table.concat(action_stats, "; "),
    ["Sender-Stats"] = table.concat(sender_stats, "; "),
    ["Recent-Processing"] = table.concat(recent_processing, "; "),
    ["History-Entries"] = tostring(#_G.message_analytics.processing_history),
    ["Dataset-Count"] = tostring(function() local c = 0; for _ in pairs(_G.data_sets) do c = c + 1 end; return c end())
  })
  
  print("Analytics sent - " .. total_messages .. " messages processed")
end

--- Help for functional programming utilities
-- Action: "Help"
if Msg.Action == "Help" then
  local help_text = [[
Functional Programming Utilities Help:

Number Processing:
- ProcessNumbers: Apply operations to numeric data
  * Numbers: comma-separated values
  * Operation: square, filter, sum, analyze

Text Processing:  
- ProcessText: Apply operations to text data
  * Text: input text to process
  * Operation: words, filter, transform, analyze
  * MinLength: minimum word length for filtering

Dataset Management:
- StoreDataset: Save data for later processing
  * DatasetId: unique identifier
  * Data: comma-separated values or text
  * DataType: numbers, words, or json
- ProcessDataset: Apply operations to stored dataset
  * DatasetId: dataset identifier
  * Operations: comma-separated operations
- ListDatasets: Show all stored datasets

Available Operations:
- map_square, map_double: Transform values
- filter_even, filter_positive, filter_long: Filter data
- reduce_sum, reduce_concat: Aggregate data

Analytics:
- Analytics: View processing statistics and history

Examples:
- Process numbers: Action="ProcessNumbers", Numbers="1,2,3,4,5", Operation="square"
- Analyze text: Action="ProcessText", Text="hello world example", Operation="analyze"
- Store dataset: Action="StoreDataset", DatasetId="test1", Data="10,20,30,40", DataType="numbers"
- Process dataset: Action="ProcessDataset", DatasetId="test1", Operations="map_square,filter_even,reduce_sum"

Features:
✓ Functional programming patterns with utils module
✓ Chainable operations for data transformation
✓ Text and numeric data processing
✓ Dataset storage and batch processing
✓ Comprehensive analytics and logging
✓ Support for map, filter, reduce operations
  ]]
  
  Send({
    Target = Msg.From,
    Action = "HelpResponse",
    Data = help_text
  })
  
  print("Help sent to " .. (Msg.From or "Unknown"))
end