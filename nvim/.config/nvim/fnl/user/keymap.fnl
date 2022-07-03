(module user.keymap
  {autoload {a aniseed.core}})

;; remap to Lightspeed's multiline versions
(vim.keymap.set "n" ";" "<Plug>Lightspeed_;_ft" { :remap true })
(vim.keymap.set "n" "," "<Plug>Lightspeed__ft" { :remap true })

;; This is now a neovim default, but keeping here for clarify
(vim.keymap.set "n" "Y" "y$")

;; sets gh to whatever K is (uncertain what that runs behind the scenes). Overridden when LSP becomes active and becomes hover
(vim.keymap.set "n" "gh" "K") 

;; the on mapping I can't live without
(vim.keymap.set "i" "jk" "<esc>")

;; into normal mode in a term buffer
(vim.keymap.set "t" "jk" "<c-\\><c-n>")

;; actually send escape to the shell in a term buffer
(vim.keymap.set "t" "<M-[>" "<Esc>") 

;; shell-fzf-like binding to open searchable command line history
(vim.keymap.set "c" "<C-r>" "<cmd>Telescope command_history<cr>")
