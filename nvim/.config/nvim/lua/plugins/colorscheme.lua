return {
	{
		"rose-pine/neovim",
		name = "rose-pine",
		config = function()
			---@diagnostic disable-next-line: missing-fields
			require("rose-pine").setup({
				dark_variant = "moon",
			})
			-- vim.cmd("colorscheme rose-pine")
		end
	},
	{
		"rebelot/kanagawa.nvim",
		config = function() 
			require('kanagawa').setup({
				colors = {
					theme = {
						all = {
							ui = {
								bg_gutter = "none",
							},
						},
					},
				},
				overrides = function (colors)
					return {
						-- indent-blankline overrides
						IblIndent = { fg = colors.palette.dragonBlack4 },
						IblScope = { fg = colors.palette.dragonBlack5 },
					}
				end
			})
			vim.cmd("colorscheme kanagawa-dragon")
		end
	},
}
