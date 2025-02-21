local function setup_mini_surround()
	-- configure like vim-surround
	require('mini.surround').setup({
		mappings = {
			add = 'ys',
			delete = 'ds',
			find = '',
			find_left = '',
			highlight = '',
			replace = 'cs',
			update_n_lines = '',

			-- Add this only if you don't want to use extended mappings
			suffix_last = '',
			suffix_next = '',
		},
		search_method = 'cover_or_next',
	})
	-- Remap adding surrounding to Visual mode selection
	vim.keymap.del('x', 'ys')
	vim.keymap.set('x', 'S', [[:<C-u>lua MiniSurround.add('visual')<CR>]], { silent = true })

	-- Make special mapping for "add surrounding for line"
	vim.keymap.set('n', 'yss', 'ys_', { remap = true })
end

local function setup_mini_statusline()
	-- Simple and easy statusline.
	--  You could remove this setup call if you don't like it,
	--  and try some other statusline plugin
	local statusline = require 'mini.statusline'
	-- set use_icons to true if you have a Nerd Font
	local content = function()
		local mode, mode_hl = MiniStatusline.section_mode({ trunc_width = 120 })
		local git           = MiniStatusline.section_git({ trunc_width = 40 })
		local diff          = vim.trim(MiniStatusline.section_diff({ trunc_width = 75, icon = '' }))
		if diff == "-" then
			diff = ""
		end
		local diagnostics   = MiniStatusline.section_diagnostics({ trunc_width = 75 })
		local lsp           = MiniStatusline.section_lsp({ trunc_width = 75 })
		local filename      = MiniStatusline.section_filename({ trunc_width = 140 })
		local fileinfo      = MiniStatusline.section_fileinfo({ trunc_width = 120 })
		local location      = MiniStatusline.section_location({ trunc_width = 75 })
		local search        = MiniStatusline.section_searchcount({ trunc_width = 75 })

		return MiniStatusline.combine_groups({
			{ hl = mode_hl,                  strings = { mode } },
			{ hl = 'MiniStatuslineDevinfo',  strings = { git, diff } },
			'%<', -- Mark general truncate point
			{ hl = 'MiniStatuslineFilename', strings = { filename } },
			'%=', -- End left alignment
			{ hl = 'MiniStatuslineFileinfo', strings = { fileinfo } },
			{ hl = mode_hl,                  strings = { search, location } },
		})
	end
	statusline.setup {
		use_icons = vim.g.have_nerd_font,
		content = {
			active = content,
			inactive = content,
		}
	}

	-- You can configure sections in the statusline by overriding their
	-- default behavior. For example, here we set the section for
	-- cursor location to LINE:COLUMN
	---@diagnostic disable-next-line: duplicate-set-field
	statusline.section_location = function()
		return '%2l:%-2v'
	end
end

local function setup_mini_operators()
	-- main thing I care about is vim-exchange-like gx op
	require('mini.operators').setup()
end

return { -- Collection of various small independent plugins/modules
	'echasnovski/mini.nvim',
	config = function()
		-- Better Around/Inside textobjects
		--
		-- Examples:
		--  - va)  - [V]isually select [A]round [)]paren
		--  - yinq - [Y]ank [I]nside [N]ext [Q]uote
		--  - ci'  - [C]hange [I]nside [']quote
		-- require('mini.ai').setup { n_lines = 500 }

		-- Add/delete/replace surroundings (brackets, quotes, etc.)
		setup_mini_surround()

		setup_mini_statusline()

		setup_mini_operators()

		-- ... and there is more!
		--  Check out: https://github.com/echasnovski/mini.nvim
	end,
}
