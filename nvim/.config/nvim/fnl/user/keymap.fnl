(module user.keymap
  {autoload {a aniseed.core
             telescope telescope.builtin
             telescope-themes telescope.themes}
   require-macros [user.macros]})

(nmap! "s" "<cmd>HopChar2<cr>")
(nmap! "S" "<cmd>HopChar2MW<cr>")

(augroup! ["fugitive_map" {:clear true}]
          (au! :FileType {:pattern "fugitive"
                          :command "nmap <buffer> <tab> ="})
          (au! :FileType {:pattern ["fugitive" "fugitiveblain"]
                          :command "nmap <buffer> q gq"}))

(augroup! ["netrw" {:clear true}]
          (au! :FileType {:pattern "netrw"
                          :command "nnoremap <buffer> q <cmd>Rexplore<cr>"}))
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
             (";" #(telescope.commands (telescope-themes.get_ivy)) "M-x (let's pretend)")
             ("/" "<cmd>Telescope live_grep<cr>" :Grep)
             ("." "<cmd>Explore<cr>" "Explore current directory")
             ("'" "<cmd>Telescope resume<cr>" "Resume last search")
             ("h" "<cmd>Telescope help_tags<cr>" "help"))

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
             (:g "<cmd>Telescope git_files<cr>" "Git files")
             (:n :<cmd>enew<cr> "New File")
             (:p "<cmd>lua require'telescope.builtin'.find_files({ cwd = '~/dotfiles' })<cr>" "Config files")
             (:b "<cmd>Telescope buffers<cr>" :Buffers)
             (:t :<cmd>Telescope<cr> "Telescope builtins")
             (":" "<cmd>Telescope command_history<cr>" "Command history"))


(leader-map! [:+tabs :t]
             (:n :<cmd>tabn<cr> "next tab")
             (:p :<cmd>tabp<cr> "previous tab")
             (:c :<cmd>tabclose<cr> "close tab")
             (:t "<cmd>tabe | term<cr>" "open a terminal in a new tab")
             (:o :<cmd>tabe<cr> "open new tab"))

(leader-map! [:+buffers :b]
             (:d :<cmd>bd<cr> "delete buffer")
             (:n :<cmd>bn<cr> "next buffer")
             (:p :<cmd>bp<cr> "previous buffer")
             (:b "<cmd>Telescope buffers<cr>" "Switch to buffer"))

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
             (:f "<cmd>Git fetch<cr>" "git fetch")
             (:t "<cmd>%Gclog<cr>" "git time machine (file history)"))

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
