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
		opts = {
			colors = {
				theme = {
					all = {
						ui = {
							bg_gutter = "none",
						},
					},
				},
			},
		},
		init = function ()
			vim.cmd("colorscheme kanagawa-dragon")
		end,
	},
}
