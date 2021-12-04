vim.api.nvim_set_keymap('n', ';', '<Plug>Lightspeed_;_ft', { noremap = false })
vim.api.nvim_set_keymap('n', ',', '<Plug>Lightspeed_,_ft', { noremap = false })
vim.api.nvim_set_keymap('n', 'gh', "<cmd>call CocAction('doHover')<CR>", { noremap = true })
vim.api.nvim_set_keymap('n', 'Y', "y$", { noremap = true })
vim.api.nvim_set_keymap('i', 'jk', '<esc>', { noremap = true })
vim.api.nvim_set_keymap('t', 'jk', '<c-\\><c-n>', { noremap = true })
vim.api.nvim_set_keymap('t', '<M-[>', '<Esc>', { noremap = true })

