vim.cmd[[filetype plugin indent on]]
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.hidden = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.wrap = false
vim.opt.linebreak = true
vim.opt.wildmenu = true
vim.opt.wildmode = { 'longest', 'full' }
vim.opt.timeoutlen = 200
vim.opt.fixeol = false;
vim.opt.scrolloff = 8
vim.opt.sidescrolloff = 16
vim.opt.hlsearch = false;
vim.opt.cursorline = true;
vim.opt.termguicolors = true
vim.opt.mouse = 'nv'

vim.g.mapleader = ' '
vim.g.maplocalleader = ','
vim.g.typescript_disable_indent = true

vim.cmd[[let $NVIM_TUI_ENABLE_TRUE_COLOR=1]]
vim.cmd[[syntax on]]

vim.g.rose_pine_variant = 'moon'
vim.cmd[[colorscheme rose-pine]]
