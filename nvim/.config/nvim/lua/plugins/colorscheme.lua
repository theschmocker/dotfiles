return {
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
						-- TODO: see if there's a way to just darken the normal highlights
						-- for DiagnosticUnnecessary, instead of making it fully gray
						-- potential leads
						-- for creating highlights based on another and modifying its colors
						-- https://www.reddit.com/r/neovim/comments/qco76a/comment/hhiiuww/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
						--  plus
						-- for the darkening-into-the-background/transparency-like effect I want
						-- the blending algorithm this site uses https://meyerweb.com/eric/tools/color-blend/

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
