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

local gitsigns = { -- Adds git related signs to the gutter, as well as utilities for managing changes
	'lewis6991/gitsigns.nvim',
	opts = { },
}

return {
	neogit,
	gitsigns,
}
