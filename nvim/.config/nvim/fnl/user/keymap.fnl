(module user.keymap
  {autoload {a aniseed.core}
   require-macros [user.macros]})

;; remap to Lightspeed's multiline versions
(nmap! ";" "<Plug>Lightspeed_;_ft")
(nmap! "," "<Plug>Lightspeed_,_ft")

;; This is now a neovim default, but keeping here for clarify
(nnoremap! "Y" :y$)

;; sets gh to whatever K is (uncertain what that runs behind the scenes). Overridden when LSP becomes active and becomes hover
(nnoremap! "gh" :K)

; ;; the one mapping I can't live without
(inoremap! "jk" :<esc>)

;; into normal mode in a term buffer
(tnoremap! "jk" "<c-\\><c-n>")

;; actually send escape to the shell in a term buffer
(tnoremap! "<M-[>" :<Esc>)

;; shell-fzf-like binding to open searchable command line history
(cnoremap! "<C-p>" "<cmd>Telescope command_history<cr>")

