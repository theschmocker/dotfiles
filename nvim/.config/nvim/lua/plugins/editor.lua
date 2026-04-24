return {
	{
		'https://codeberg.org/andyg/leap.nvim',
		commit = 'b960d5038c',
		config = function ()
			vim.keymap.set({ 'n', 'x', 'o' }, 's', '<Plug>(leap-anywhere)')

			-- Disable autojumping to the first match
			require('leap').opts.safe_labels = ''

			-- keep my own settings
			require('leap').opts.vim_opts['wo.scrolloff'] = nil
			require('leap').opts.vim_opts['wo.sidescrolloff'] = nil
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
	-- {
	-- 	'ggml-org/llama.vim',
	-- 	init = function ()
	-- 		-- running in parallels; point at mac host
	-- 		if vim.fn.has('win32') == 1 then
	-- 			vim.g.llama_config = {
	-- 				endpoint = 'http://10.211.55.2:8012/infill',
	-- 			}
	--
	-- 		end
	-- 		vim.keymap.set('n', '<leader>tl', '<cmd>LlamaToggle<cr>', { desc = 'Toggle Code Completion' })
	-- 	end
	-- },
}
