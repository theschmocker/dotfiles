local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require("luasnip.util.events")
local ai = require("luasnip.nodes.absolute_indexer")
local extras = require("luasnip.extras")
local l = extras.lambda
local rep = extras.rep
local p = extras.partial
local m = extras.match
local n = extras.nonempty
local dl = extras.dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local conds = require("luasnip.extras.expand_conditions")
local postfix = require("luasnip.extras.postfix").postfix
local types = require("luasnip.util.types")
local parse = require("luasnip.util.parser").parse_snippet

-- vim.keymap.set("n", "<leader>hrs", "<cmd>source ~/.config/nvim/lua/config/plugins/snippets/init.lua<cr>", {
-- 	desc = "Reload Snippets"
-- })

vim.keymap.set({ "i", "s" }, "<C-E>", function()
	if ls.choice_active() then
		ls.change_choice(1)
	end
end, { silent = true })

local vue_computed_snippet = s(
	'comp',
	fmta("const <name> = computed(<body>);", {
		name = i(1, "NAME"),
		body = d(2, function (args)
			return sn(nil, {
				c(1, {
					fmt("() => {}", { i(1, "value") }),
					fmta(
						[[{
	get: () =>> <value>,
	set: (<name_rep>) =>> <set>,
}]],
						{
							value = i(1),
							name_rep = t(args[1]),
							set = i(2),
						}
					)

				})
			})
		end, { 1 })
	})
)

ls.add_snippets("vue", {
	vue_computed_snippet,
	s({
		trig = "dfp",
		desc = "defineProps"
	}, fmt([[
const props = defineProps<{{
	{}
}}>()
	]], { i(1) }))
})
