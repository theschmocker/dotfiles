-- Basic settings
vim.o.number = true
vim.o.relativenumber = true
vim.o.hidden = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = false
vim.o.wrap = false
vim.o.linebreak = true
vim.o.wildmenu = true
vim.o.wildmode = "longest,full"
vim.o.timeoutlen = 300
require('util.keymap').configure_insert_mode_timeout()
vim.o.fixeol = false
vim.o.scrolloff = 8
-- TODO: sidescrolloff=2 in buffer types like text or markdown
vim.o.sidescrolloff = 16
vim.o.hlsearch = false
vim.o.cursorline = true
vim.o.termguicolors = true
vim.o.mouse = "nv"
vim.o.signcolumn = "yes"
vim.o.syntax = "on"
vim.o.swapfile = false
vim.o.updatetime = 250 -- decrease time for CursorHold
vim.o.splitright = true
vim.o.splitbelow = true
vim.o.inccommand = "split"
vim.o.background = "dark"
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true

vim.schedule(function()
	vim.opt.clipboard = 'unnamedplus'
end)

-- EditorConfig_exclude_patterns ["fugitive://.*" "scp://.*"]
vim.g.mapleader = " "
vim.g.maplocalleader = " m"

vim.g.have_nerd_font = true

vim.filetype.add({
	filename = {
		Podfile = "ruby",
		Fastfile = "ruby",
		['.watchmanconfig'] = 'json',
	},
	extension = {
		podspec = "ruby",
		mdx = "markdown",
	},
})

vim.diagnostic.config({
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = '',
			[vim.diagnostic.severity.WARN] = '',
			[vim.diagnostic.severity.INFO] = '',
			[vim.diagnostic.severity.HINT] = "",
		}
	}
})

-- silence deprecation warnings
---@diagnostic disable-next-line: duplicate-set-field
vim.deprecate = function() end

-- TODO: this breaks mouse scroll in normal mode when at bottom (gets stuck)
-- vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "WinScrolled" }, {
-- 	desc = "Fix scrolloff when you are at the EOF",
-- 	group = vim.api.nvim_create_augroup("ScrollEOF", { clear = true }),
-- 	callback = function()
-- 		local is_terminal = vim.api.nvim_get_option_value("buftype", { buf = 0 }) == "terminal"
-- 		if vim.api.nvim_win_get_config(0).relative ~= "" or is_terminal then
-- 			return -- Ignore floating windows
-- 		end
--
-- 		local win_height = vim.fn.winheight(0)
-- 		local scrolloff = math.min(vim.o.scrolloff, math.floor(win_height / 2))
-- 		local visual_distance_to_eof = win_height - vim.fn.winline()
--
-- 		if visual_distance_to_eof < scrolloff then
-- 			local win_view = vim.fn.winsaveview()
-- 			vim.fn.winrestview({ topline = win_view.topline + scrolloff - visual_distance_to_eof })
-- 		end
-- 	end,
-- })

local keep_pre_yank_cursor_pos = function ()
	vim.keymap.set({ "n", "x" }, "y", function()
		vim.b.user_cursor_pre_yank = vim.api.nvim_win_get_cursor(0)
		return "y"
	end, { expr = true })

	vim.keymap.set("n", "Y", function()
		vim.b.user_cursor_pre_yank = vim.api.nvim_win_get_cursor(0)
		return "y$"
	end, { expr = true })

	local group = vim.api.nvim_create_augroup("keep_pre_yank_cursor_pos", { clear = true })
	vim.api.nvim_create_autocmd("TextYankPost", {
		group = group,
		callback = function()
			if vim.v.event.operator == "y" and vim.b.user_cursor_pre_yank then
				vim.api.nvim_win_set_cursor(0, vim.b.user_cursor_pre_yank)
				vim.b.user_cursor_pre_yank = nil
			end
		end,
	})
end

keep_pre_yank_cursor_pos()
