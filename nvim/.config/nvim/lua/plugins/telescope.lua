return {
	'nvim-telescope/telescope.nvim', tag = '0.1.8',
	dependencies = { 
		'nvim-lua/plenary.nvim',
		'nvim-telescope/telescope-ui-select.nvim',
	},
	config = function()
		require("telescope").setup({
			defaults = {
				layout_strategy = 'bottom_pane',
				sorting_strategy = 'ascending',
				layout_config = {
					bottom_pane = {
						width = 0.4,
					}
				}
			}
		})

		pcall(require('telescope').load_extension, 'ui-select')

		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader><leader>", builtin.find_files, { desc = "Find File" })
		vim.keymap.set("n", "<leader>/", builtin.live_grep, { desc = "Search Project" })
	end
}
