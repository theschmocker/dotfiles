(module user.keymap
  {autoload {a aniseed.core}})

(vim.keymap.set "n" ";" "<Plug>Lightspeed_;_ft" { :remap true })
(vim.keymap.set "n" "," "<Plug>Lightspeed__ft" { :remap true })
(vim.keymap.set "n" "Y" "y$")
(vim.keymap.set "i" "jk" "<esc>")
(vim.keymap.set "t" "jk" "<c-\\><c-n>")
(vim.keymap.set "t" "<M-[>" "<Esc>") 
