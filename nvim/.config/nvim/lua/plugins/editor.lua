return {
	{
		'hadronized/hop.nvim',
		config = function ()
			local hop = require('hop')
			hop.setup({
				keys = "asdfhjkl",
			})

			vim.keymap.set('', 's', function ()
				 hop.hint_char2({
					multi_windows = true
				})
			end)
		end
	},
	{
		"mattn/emmet-vim",
		init = function ()
			vim.g.user_emmet_leader_key = "<C-H>"
		end
	},
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		---@module "ibl"
		---@type ibl.config
		opts = {},
	},
	{
		'ggml-org/llama.vim',
		init = function ()
			vim.keymap.set('n', '<leader>tl', '<cmd>LlamaToggle<cr>', { desc = 'Toggle Code Completion' })
		end
	},
}
