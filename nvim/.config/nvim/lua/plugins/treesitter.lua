local data = require('data.treesitter-data')

-- TODO: CSS text objects
-- is: inside selector. something like .|some-class|
-- as: around selector. something like |.some-class|
-- ir: inside ruleset. something like .some-class {| some: property |}
-- ar: around ruleset. something like |.some-class { some: property }|

-- doing this because the default keymaps option only sets the keymap
-- if the queries are defined for the top-level language of a buffer,
-- not for injections, so things like JS/TS objects wouldn't work in
-- Vue/Svelte files.
--
-- TODO: there's a weird bug, possibly which-key related where pressing
-- "ci" quickly, then attempting a custom textobj mapping (like "f" for "if"),
-- doesn't work. It does work if I enter the whole thing quickly "cif" or if I
-- wait for which-key to appear after pressing "c". Happens with keymaps plugin
-- option too.
local function map_treesitter_text_objects()
	local select_textobject = require('nvim-treesitter.textobjects.select').select_textobject

	local mappings = {
		['if'] = '@function.inner',
		['af'] = '@function.outer',
		['ia'] = '@parameter.inner',
		['aa'] = '@parameter.outer',
		['ix'] = '@attribute.inner',
		['ax'] = '@attribute.outer',
	}

	for _, mode in ipairs({ 'o', 'x' }) do
		for key, query in pairs(mappings) do
			vim.keymap.set(
				mode,
				key,
				-- NOTE: this has to be a string, for some reason. when I passed what I thought was an
				-- equivalent lua function, the mappings broke if there was a delay, e.g. typing 'yi' quickly, then
				-- pressing the remaining object mapping, e.g. f, wouldn't work
				string.format(
					"<cmd>lua require'nvim-treesitter.textobjects.select'.select_textobject('%s','textobjects','%s')<cr>",
					query,
					mode
				),
				{
					silent = true,
					desc = "TS " .. query,
				})
		end
	end
end

return {
	{ -- Highlight, edit, and navigate code
		'nvim-treesitter/nvim-treesitter',
		dependencies = {
			'nvim-treesitter/nvim-treesitter-context',
			'nvim-treesitter/nvim-treesitter-textobjects',
		},
		build = ':TSUpdate',
		-- [[ Configure Treesitter ]] See `:help nvim-treesitter`
		config = function ()
			local treesitter = require('nvim-treesitter.configs')

			---@diagnostic disable-next-line: missing-fields
			treesitter.setup({
				ensure_installed = data.ensure_installed,
				-- Autoinstall languages that are not installed
				auto_install = true,
				highlight = {
					enable = true,
					-- Some languages depend on vim's regex highlighting system (such as Ruby) for indent rules.
					--  If you are experiencing weird indenting issues, add the language to
					--  the list of additional_vim_regex_highlighting and disabled languages for indent.
					additional_vim_regex_highlighting = { 'ruby' },
				},
				indent = { enable = true, disable = { 'ruby' } },
				incremental_selection = {
					enable = true,
					keymaps = {
						init_selection = "<C-space>",
						node_incremental = "<C-space>",
						scope_incremental = false,
						node_decremental = "<bs>",
					},
				},
				textobjects = {
					select = {
						enable = true,

						-- Automatically jump forward to textobj, similar to targets.vim
						lookahead = true,

						keymaps = {
							-- You can use the capture groups defined in textobjects.scm
							-- ["af"] = "@function.outer",
							-- ["if"] = "@function.inner",
							-- ["ac"] = "@class.outer",
							-- ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
							-- ["as"] = { query = "@local.scope", query_group = "locals", desc = "Select language scope" },
						},
						-- You can choose the select mode (default is charwise 'v')
						--
						-- Can also be a function which gets passed a table with the keys
						-- * query_string: eg '@function.inner'
						-- * method: eg 'v' or 'o'
						-- and should return the mode ('v', 'V', or '<c-v>') or a table
						-- mapping query_strings to modes.
						-- selection_modes = {
						-- 	['@function.outer'] = 'V', -- linewise
						-- },
						-- If you set this to `true` (default is `false`) then any textobject is
						-- extended to include preceding or succeeding whitespace. Succeeding
						-- whitespace has priority in order to act similarly to eg the built-in
						-- `ap`.
						--
						-- Can also be a function which gets passed a table with the keys
						-- * query_string: eg '@function.inner'
						-- * selection_mode: eg 'v'
						-- and should return true or false
						-- include_surrounding_whitespace = true,
					},
				},
			})
			map_treesitter_text_objects()
		end,
	},
	{
		'JoosepAlviste/nvim-ts-context-commentstring',
		opts = {
			enable_autocmd = false,
		},
		init = function ()
			if not _G._my_vim_get_option then
				_G._my_vim_get_option = vim.filetype.get_option
			end

			---@diagnostic disable-next-line: duplicate-set-field
			vim.filetype.get_option = function (filetype, option)
				return option == "commentstring"
					and require("ts_context_commentstring.internal").calculate_commentstring()
					or _G._my_vim_get_option(filetype, option)
			end
		end
	}
}
