return {
	{
		'hadronized/hop.nvim',
		config = function ()
			local hop = require('hop')
			hop.setup({
				keys = "asdfhjkl",
			})

			vim.keymap.set('', 's', function ()
				 hop.hint_char2({
					multi_windows = true
				})
			end)
		end
	},
	{
		"mattn/emmet-vim",
		init = function ()
			vim.g.user_emmet_leader_key = "<C-H>"
		end
	}
}
