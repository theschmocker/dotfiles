return {
	'nvim-telescope/telescope.nvim', tag = '0.1.8',
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
				}
			},
			pickers = {
				live_grep = {
					mappings = {
						i = { ["<c-f>"] = require('telescope.actions').to_fuzzy_refine },
					},
				},
			}
		})

		pcall(require('telescope').load_extension, 'ui-select')

		local builtin = require("telescope.builtin")
		vim.keymap.set("n", "<leader><leader>", function ()
			if not pcall(builtin.git_files, {
				show_untracked = true,
				use_git_root = false,
			}) then
				builtin.find_files()
			end
		end, { desc = "Find File" })
		vim.keymap.set("n", "<leader>/", builtin.live_grep, { desc = "Search Project" })
	end
}
