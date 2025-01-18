local neogit = {
	"NeogitOrg/neogit",
	dependencies = {
		"nvim-lua/plenary.nvim",         -- required
		"sindrets/diffview.nvim",        -- optional - Diff integration
		--
		-- "nvim-telescope/telescope.nvim", -- optional
	},
	opts = {
		integrations = {
			telescope = true,
		},
	},
}

return {
	neogit,
}
