require('plugins')

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.hidden = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.wrap = false
vim.opt.ruler = true
vim.opt.linebreak = true
vim.opt.wildmenu = true
vim.opt.wildmode = { 'longest', 'full' }
vim.opt.timeoutlen = 200
vim.opt.fixeol = false;
vim.opt.scrolloff = 6

-- vim.api.nvim_set_keymap('n', 's', '<cmd>HopChar2<cr>', { noremap = true })
vim.api.nvim_set_keymap('n', ';', '<Plug>Lightspeed_;_ft', { noremap = false })
vim.api.nvim_set_keymap('n', ',', '<Plug>Lightspeed_,_ft', { noremap = false })

require'telescope'.setup {
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case',
      '-u'
    }
  },
  pickers = {
    find_files = {
      hidden = true,
      find_command = { 'rg', '--files', '--iglob', '!.git', '--hidden' },
    }
  }
}

require'which-key-config'
