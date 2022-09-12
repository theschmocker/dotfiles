(module user.keymap
  {autoload {a aniseed.core
             telescope telescope.builtin}
   require-macros [user.macros]})

;; remap to Lightspeed's multiline versions
(nmap! ";" "<Plug>Lightspeed_;_ft")
(nmap! "," "<Plug>Lightspeed_,_ft")

;; Omnidirectional Lightspeed mappings
(nmap! "s" "<Plug>Lightspeed_omni_s")
(nmap! "gs" "<Plug>Lightspeed_omni_gs")

;; This is now a neovim default, but keeping here for clarity
(nnoremap! "Y" :y$)

;; sets gh to whatever K is (uncertain what that runs behind the scenes). Overridden when LSP becomes active and becomes hover
(nnoremap! "gh" :K)

;; the one mapping I can't live without
(inoremap! "jk" :<esc>)

;; into normal mode in a term buffer
(tnoremap! "jk" "<c-\\><c-n>")

;; actually send escape to the shell in a term buffer
(tnoremap! "<M-[>" :<Esc>)

(leader-map! ["" ""]
             (:<leader> #(telescope.find_files) "Find File")
             (";" #(telescope.command_history) "Fuzzy Command History"))

(leader-map! [:+window :w]
             (:w :<C-W>w :other-window)
             (:d :<C-W>c :delete-window)
             (:- :<C-W>s :split-window-below)
             (:h :<C-W>h :window-left)
             (:j :<C-W>j :window-below)
             (:l :<C-W>l :window-right)
             (:k :<C-W>k :window-up)
             (:H :<C-W>5< :expand-window-left)
             (:J "<cmd>resize +5<cr>" :expand-window-below)
             (:L :<C-W>5> :expand-window-right)
             (:K "<cmd>resize -5<cr>" :expand-window-up)
             (:= :<C-W>= :balance-window)
             (:s :<C-W>s :split-window-below)
             (:v :<C-W>v :split-window-vertical)
             (:o :<cmd>only<cr> :only))

(leader-map! [:+find/file :f]
             (:f "<cmd>lua require'telescope.builtin'.find_files({ hidden = true, find_command = {'rg', '--files', '--iglob', '!.git', '--no-ignore', '--hidden'}})<cr>" "Files (include ignored)")
             (:r "<cmd>Telescope oldfiles<cr>" "Open Recent File")
             (:g "<cmd>Telescope live_grep<cr>" :Grep)
             (:n :<cmd>enew<cr> "New File")
             (:p "<cmd>lua require'telescope.builtin'.find_files({ cwd = '~/dotfiles' })<cr>" "Config files")
             (:b "<cmd>Telescope buffers<cr>" :Buffers)
             (:t :<cmd>Telescope<cr> "Telescope builtins")
             (":" "<cmd>Telescope command_history<cr>" "Command history")
             (:G "<cmd>Telescope git_files<cr>" "Git files"))


(leader-map! [:+tabs :t]
             (:n :<cmd>tabn<cr> "next tab")
             (:p :<cmd>tabp<cr> "previous tab")
             (:c :<cmd>tabclose<cr> "close tab")
             (:t "<cmd>tabe | term<cr>" "open a terminal in a new tab")
             (:o :<cmd>tabe<cr> "open new tab"))

(leader-map! [:+buffers :b]
             (:d :<cmd>bd<cr> "delete buffer")
             (:n :<cmd>bn<cr> "next buffer")
             (:p :<cmd>bp<cr> "previous buffer"))

(leader-map! [:+git :g] 
             (:g (fn []
                   (set! splitbelow false)
                   (vim.cmd "Git")
                   (set! splitbelow true)) "git status")
             (:s "<cmd>echoerr \"Git status was rebound to <leader>gg\"<cr>" "(obsolete git status)")
             (:d :<cmd>Gvdiffsplit!<cr> "git diff in vertical split")
             (:b "<cmd>Git blame<cr>" "git blame")
             (:c ":Git checkout " "git checkout")
             (:p "<cmd>Git pull<cr>" "git pull")
             (:f "<cmd>Git fetch<cr>" "git fetch"))

(leader-map! ["+git pull" :gp]
             (:p "<cmd>Git pull<cr>" "git pull")
             (:r "<cmd>Git pull --rebase<cr>" "git pull --rebase"))

(leader-map! ["+git push" :gP]
             (:P "<cmd>Git push<cr>" "REALLY git push"))

(fn make-setting-toggler [setting on off tbl]
  (let [tbl (or tbl vim.o)]
    (fn []
      (if (= on (. tbl setting))
        (tset tbl setting off)
        (tset tbl setting on))
      (when (= tbl vim.o)
        (vim.cmd (.. "set " setting "?"))))))

(local toggle-background (make-setting-toggler :background :dark :light))
(local toggle-wrap (make-setting-toggler :wrap true false))

(leader-map! ["+setting toggles" :s]
             (:b toggle-background "background light/dark")
             (:w toggle-wrap "wrap"))
