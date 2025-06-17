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
			-- running in parallels; point at mac host
			if vim.fn.has('win32') == 1 then
				vim.g.llama_config = {
					endpoint = 'http://10.211.55.2:8012/infill',
				}

			end
			vim.keymap.set('n', '<leader>tl', '<cmd>LlamaToggle<cr>', { desc = 'Toggle Code Completion' })
		end
	},
}
