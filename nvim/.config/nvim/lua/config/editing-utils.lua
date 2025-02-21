local M = {}

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

	local node_start_row, node_start_col, node_end_row, node_end_col = node:range()

	local rep_lines = vim.split(replacement, '\n')
	vim.api.nvim_buf_set_text(0, node_start_row, node_start_col, node_end_row, node_end_col, rep_lines)
	vim.cmd(string.format("%d,%dnormal ==", node_start_row, node_start_row + #rep_lines))

	vim.api.nvim_win_set_cursor(0, { node_start_row + 1, node_start_col + 1 })
end

return M
