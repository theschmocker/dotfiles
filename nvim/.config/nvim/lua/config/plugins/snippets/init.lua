---@diagnostic disable: unused-local
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

local ecma_import_snippet = s(
	'imp',
	fmta("import { <named> } from '<package>';", {
		package = i(1),
		named = i(2),
	})
)

local ecma_arrow_function = s(
	{
		trig = "fn",
		desc = "arrow function",
	},
	fmta("(<params>) =>> {<body>}", {
		params = i(1),
		body = i(2)
	})
)

local ecma_function = s(
	{
		trig = "func",
		desc = "function",
	},
	fmta(
		[[function <name>(<params>) {
	<body>
}]],
		{
			name = i(1),
			params = i(2),
			body = i(3),
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
	]], { i(1) })),
	ecma_import_snippet,
	ecma_arrow_function,
	ecma_function,
	s(
		{
			trig = "vbase",
			desc = "script setup component boilerplate"
		},
		fmt(
			[[
<template>
	{template}
</template>

<script setup lang="ts">
	{script}
</script>

<style{style_attrs}>
</style>
	]],
			{
				style_attrs = i(1),
				template = i(2),
				script = i(3),
			}
		)
	)
})

ls.add_snippets('lua', {
	s('f', fmt([[function {}({})
	{}
end]], { i(1), i(2), i(3) })),
})

ls.add_snippets('typescript', {
	ecma_import_snippet,
	ecma_arrow_function,
	ecma_function,
})
ls.add_snippets('javascript', {
	ecma_import_snippet,
	ecma_arrow_function,
	ecma_function,
})

ls.add_snippets('typescriptreact', {
	s({ trig = 'us', desc = 'useState' }, fmt("const [{var}, set{var_upper}] = useState{before_parens}({value});", {
		var = i(1),
		var_upper = f(function (args)
			local var_name = args[1][1];
			if var_name == "" then
				return ""
			end

			return string.gsub(var_name, "^%a", function (match)
				return match:upper()
			end)
		end, { 1 }),
		before_parens = i(2),
		value = i(3),
	})),
	s(
		{ trig = 'ue', desc = 'useEffect' }, 
		fmt(
			[[useEffect(() => {{
	{body}
}}, [{deps}])]],
			{
				body = i(2),
				deps = i(1),
			}
		)
	),
	ecma_import_snippet,
	ecma_arrow_function,
	ecma_function,
})

ls.add_snippets('cs', {
	s({ trig = '///', desc = "doc comment" },
		fmt(
			[[/// <summary>
/// {}
/// </summary>]],
			{ i(1) }
	))
})
