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
				undercurl = false,
				colors = {
					theme = {
						all = {
							ui = {
								bg_gutter = "none",
							},
						},
					},
				},

				---@param colors KanagawaColors
				overrides = function (colors)
					return {
						DiagnosticUnderlineWarn = { sp = '#96602A' },
						DiagnosticUnderlineError = { sp = '#C22121', undercurl = true },
						DiagnosticSignError = { fg = '#D52323' },
						DiagnosticUnderlineInfo = { underdotted = true },
						["@string.special.url"] = { undercurl = false, underdotted = true },

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
