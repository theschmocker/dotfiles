vim.cmd[[filetype plugin indent on]]
local opt = vim.opt
opt.number = true
opt.relativenumber = true
opt.hidden = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.wrap = false
opt.linebreak = true
opt.wildmenu = true
opt.wildmode = { 'longest', 'full' }
opt.timeoutlen = 200
opt.fixeol = false;
opt.scrolloff = 8
opt.sidescrolloff = 16
opt.hlsearch = false;
opt.cursorline = true;
opt.termguicolors = true
opt.mouse = 'nv'
opt.signcolumn = 'yes'
opt.syntax = 'on'

vim.g.mapleader = ' '
vim.g.maplocalleader = ','
vim.g.typescript_disable_indent = true

vim.cmd[[let $NVIM_TUI_ENABLE_TRUE_COLOR=1]]
vim.cmd[[syntax on]]
