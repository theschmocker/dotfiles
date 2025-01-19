local leader_map = require("util.keymap").leader_map

-- Clear highlights on search when pressing <Esc> in normal mode
--  See `:help hlsearch`
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

vim.keymap.set('i', 'jk', '<Esc>')

vim.keymap.set({'v', 'n'}, '<leader>;', function() require('telescope.builtin').commands() end, {
	desc = "M-x", -- ;)
})

vim.keymap.set({'v', 'n'}, '<leader>.', function()
	require('telescope.builtin').find_files({
		cwd = require('telescope.utils').buffer_dir(),
		no_ignore = true,
	})
end, {
	desc = "Find file (relative)",
})

leader_map({
	name = "+window",
	prefix = "w",
	mode = "n",
	keys = {
		w = { '<C-W>w', desc = "other-window" },
		['d'] = { '<C-W>c', desc = 'delete-window' },
		['-'] = { '<C-W>s', desc = 'split-window-below' },
		['h'] = { '<C-W>h', desc = 'window-left' },
		['j'] = { '<C-W>j', desc = 'window-below' },
		['l'] = { '<C-W>l', desc = 'window-right' },
		['k'] = { '<C-W>k', desc = 'window-up' },
		['H'] = { '<C-W>5<', desc = 'expand-window-left' },
		['J'] = { '<cmd>resize +5<cr>', desc = 'expand-window-below' },
		['L'] = { '<C-W>5>', desc = 'expand-window-right' },
		['K'] = { '<cmd>resize -5<cr>', desc = 'expand-window-up' },
		['='] = { '<C-W>=', desc = 'balance-window' },
		['s'] = { '<C-W>s', desc = 'split-window-below' },
		['v'] = { '<C-W>v', desc = 'split-window-vertical' },
		['o'] = { '<cmd>only<cr>', desc = 'only' },
	},
})

leader_map({
	name = "+git",
	prefix = "g",
	mode = "n",
	keys = {
		g = { function() require('neogit').open() end, desc = "Neogit" },
		b = { function() require('gitsigns').blame() end, desc = "Blame" },
	},
})

leader_map({
	name = "+buffer",
	prefix = "b",
	mode = "n",
	keys = {
		b = { function() require('telescope.builtin').buffers() end, desc = "Switch to buffer" },
	},
})

leader_map({
	name = "+open",
	prefix = "o",
	mode = "n",
	keys = {
		['-'] = { '<cmd>Explore<cr>', desc = "Current Dir" },
	},
})

leader_map({
	name = "+help",
	prefix = "h",
	mode = "n",
	keys = {
		['k'] = {
			function ()
				require('telescope.builtin').keymaps()
			end,
			desc = "keymaps"
		},
		['h'] = {
			function ()
				require('telescope.builtin').help_tags()
			end,
			desc = "help tags"
		},
	}
})

local function bind_q_to_close(file_types)
	local group = vim.api.nvim_create_augroup('QToCloseGroup', { clear = true })
	vim.api.nvim_create_autocmd('FileType', {
		pattern = file_types,
		group = group,
		callback = function(data)
			print(data)
			vim.keymap.set('n', 'q', '<cmd>:quit<CR>', { buffer = data.buf })
		end
	})
end

bind_q_to_close({
	'gitsigns-blame',
	'help',
})
