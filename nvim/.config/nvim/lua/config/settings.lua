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
vim.o.timeoutlen = 200
vim.o.fixeol = false
vim.o.scrolloff = 8
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
vim.opt.splitright = true
vim.opt.splitbelow = true

vim.schedule(function()
	vim.opt.clipboard = 'unnamedplus'
end)

-- EditorConfig_exclude_patterns ["fugitive://.*" "scp://.*"]
vim.g.user_emmet_leader_key = "<C-H>"
vim.g.mapleader = " "
vim.g.maplocalleader = " m"
