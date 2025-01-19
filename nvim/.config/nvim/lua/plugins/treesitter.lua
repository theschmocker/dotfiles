local data = require('data.treesitter-data')

return {
	{ -- Highlight, edit, and navigate code
		'nvim-treesitter/nvim-treesitter',
		build = ':TSUpdate',
		main = 'nvim-treesitter.configs', -- Sets main module to use for opts
		-- [[ Configure Treesitter ]] See `:help nvim-treesitter`
		opts = {
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
		},
		-- There are additional nvim-treesitter modules that you can use to interact
		-- with nvim-treesitter. You should go explore a few and see what interests you:
		--
		--    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
		--    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
		--    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
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
