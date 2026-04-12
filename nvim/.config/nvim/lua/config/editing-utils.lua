local M = {}

local replace_node_with_text

-- With cursor on a starting or self-closing tag, toggle
-- wrapping of attributes, e.g. turns
-- ```html
-- <element example="attr">
-- ```
-- into
-- ```html
-- <element 
--   example="attr"
-- >
-- ```
-- and executing it again turns it back.
-- Happens to work for both vue and svelte
function M.toggle_vue_svelte_attribute_wrap()
	local utils = require('nvim-treesitter.ts_utils')
	local node = utils.get_node_at_cursor(0, true)
	while node and not (node:type() == "start_tag" or node:type() == "self_closing_tag") do
		node = node:parent()
	end

	if not node then
		return
	end

	local last_row = nil
	local wrapped = false
	for child in node:iter_children() do
		if child:type() == '<' or child:type() == 'tag_name' then
			goto continue
		end

		local start_row, _, end_row = child:range()
		if last_row == nil then
			last_row = start_row
		end

		if last_row ~= start_row then
			wrapped = true
			break
		end

		last_row = end_row

		::continue::
	end

	local replacement = ""
	if wrapped then
		for child in node:iter_children() do
			if child:type() == '<' or child:type() == '>' or child:type() == 'tag_name' then
				replacement = replacement .. vim.treesitter.get_node_text(child, 0)
			else
				replacement = replacement .. ' ' .. string.gsub(vim.treesitter.get_node_text(child, 0), "\n\r?$", "")
			end
		end
	else
		for child in node:iter_children() do
			if child:type() == '<' or child:type() == 'tag_name' then
				replacement = replacement .. vim.treesitter.get_node_text(child, 0)
			else
				replacement = replacement .. '\n' .. vim.treesitter.get_node_text(child, 0)
			end
		end
	end

	replace_node_with_text({ node = node, replacement = replacement })
end

-- With cursor on a JSX opening or self‑closing element, toggle the wrapping of its attributes.
-- Works for .tsx/.jsx files.
function M.toggle_jsx_attributes_wrap()
	local utils = require('nvim-treesitter.ts_utils')
	local node = utils.get_node_at_cursor(0, true)
	-- Walk up until we find a JSX opening element or a self‑closing element.
	while node and not (node:type() == "jsx_opening_element" or node:type() == "jsx_self_closing_element") do
		node = node:parent()
	end

	if not node then
		return
	end

	-- Determine whether attributes are currently wrapped (i.e., appear on multiple lines).
	local last_row = nil
	local wrapped = false
	for child in node:iter_children() do
		local ctype = child:type()
		-- Skip the '<' token and the element name.
		if ctype == '<' or ctype == 'identifier' or ctype == 'member_expression' then
			goto continue
		end

		local start_row, _, end_row = child:range()
		if last_row == nil then
			last_row = start_row
		end

		if last_row ~= start_row then
			wrapped = true
			break
		end

		last_row = end_row

		::continue::
	end

	local replacement = ""
	if wrapped then
		-- Collapse attributes onto a single line.
		for child in node:iter_children() do
			local ctype = child:type()
			if ctype == '<' or ctype == '>' or ctype == '/' or ctype == 'identifier' or ctype == 'member_expression' then
				replacement = replacement .. vim.treesitter.get_node_text(child, 0)
			else
				-- Insert a space before each attribute (or '/' token) and strip trailing newlines.
				replacement = replacement .. ' ' .. string.gsub(vim.treesitter.get_node_text(child, 0), "\n\r?$", "")
			end
		end
	else
		-- Expand each attribute onto its own line.
		for child in node:iter_children() do
			local ctype = child:type()
			if ctype == '<' or ctype == 'identifier' or ctype == 'member_expression' then
				replacement = replacement .. vim.treesitter.get_node_text(child, 0)
			else
				replacement = replacement .. '\n' .. vim.treesitter.get_node_text(child, 0)
			end
		end
	end

	replace_node_with_text({ node = node, replacement = replacement })
end

---@param opts { node: TSNode, replacement: string }
replace_node_with_text = function (opts)
	local node_start_row, node_start_col, node_end_row, node_end_col = opts.node:range()
	local rep_lines = vim.split(opts.replacement, '\n')
	vim.api.nvim_buf_set_text(0, node_start_row, node_start_col, node_end_row, node_end_col, rep_lines)
	vim.cmd(string.format("%d,%dnormal ==", node_start_row, node_start_row + #rep_lines))
	vim.api.nvim_win_set_cursor(0, { node_start_row + 1, node_start_col + 1 })
end

return M
