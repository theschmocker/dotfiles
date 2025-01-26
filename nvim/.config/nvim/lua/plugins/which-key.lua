return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	init = function ()
		-- fixes an issue I had with custom text objects, esp. when the key after i/a was
		-- a bit tricky to get to before timeout.
		--
		-- If I pressed "c", then waited until which_key showed up, 
		-- then I could use custom text object operator-pending mode mappings
		-- as expected. If I instead pressed "ci" quickly, which_key would not
		-- display, and my custom mappings would time out if I didn't press the
		-- next character quickly enough. This works around that by mapping
		-- operator+inside/around to press the initial operator, then deferring the
		-- i/a input.
		local ops = { 'c', 'd', 'y' }

		for _, op in ipairs(ops) do
			for _, ia in ipairs({ 'i', 'a' }) do
				vim.keymap.set('n', op .. ia, function ()
					vim.api.nvim_feedkeys(op, 'n', false)
					vim.schedule(function ()
						vim.api.nvim_feedkeys(ia, 'n', false)
					end)
				end)
			end
		end
	end,
	opts = {
		-- your configuration comes here
		-- or leave it empty to use the default settings
		-- refer to the configuration section below
	},
	keys = {
		{
			"<leader>?",
			function()
				require("which-key").show({ global = false })
			end,
			desc = "Buffer Local Keymaps (which-key)",
		},
	},
}
