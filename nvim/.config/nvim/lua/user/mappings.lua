local map = vim.api.nvim_set_keymap

map("n", ";", "<Plug>Lightspeed_;_ft", { noremap = false })
map("n", ",", "<Plug>Lightspeed_,_ft", { noremap = false })
map("n", "Y", "y$", { noremap = true })
map("i", "jk", "<esc>", { noremap = true })
map("t", "jk", "<c-\\><c-n>", { noremap = true })
map("t", "<M-[>", "<Esc>", { noremap = true })
