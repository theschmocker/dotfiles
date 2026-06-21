return {
	'nvim-telescope/telescope.nvim', tag = 'v0.2.2',
	dependencies = {
		'nvim-lua/plenary.nvim',
		'nvim-telescope/telescope-ui-select.nvim',
	},
	config = function()
		require("telescope").setup({
			defaults = {
				dynamic_preview_title = true,
				layout_strategy = 'bottom_pane',
				sorting_strategy = 'ascending',
				layout_config = {
					bottom_pane = {
						width = 0.4,
					}
				},
				file_sorter = require('telescope.sorters').get_fuzzy_file,
			},
			pickers = {
				live_grep = {
					mappings = {
						i = { ["<c-f>"] = require('telescope.actions').to_fuzzy_refine },
					},
				},
			},
		})

		pcall(require('telescope').load_extension, 'ui-select')
	end
}
