local wk = require("which-key")

local M = {}

M.leader_map = function (opts)
	local prefix = string.format("<leader>%s", opts.prefix)

	local tbl = {}

	table.insert(tbl, {
		prefix,
		group = opts.name,	
	})

	if opts.keys then
		local keymap = {}

		if opts.mode then
			keymap.mode = opts.mode
		end

		for key, mapping in pairs(opts.keys) do
			local remap = mapping[1]
			
			mapping = vim.tbl_extend("error", {}, mapping)
			table.insert(mapping, 1, string.format("%s%s", prefix, key))
			table.insert(keymap, mapping)
		end

		table.insert(tbl, keymap)
	end

	require("which-key").add(tbl)
end

return M
