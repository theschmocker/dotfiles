(module user.which-key {autoload {wk which-key}})

(defn register-global-mappings []
      (wk.register {:<leader> ["<cmd>Telescope find_files<cr>" "Find File"]
                    :w {:name :+windows
                        :w [:<C-W>w :other-window]
                        :d [:<C-W>c :delete-window]
                        :- [:<C-W>s :split-window-below]
                        :h [:<C-W>h :window-left]
                        :j [:<C-W>j :window-below]
                        :l [:<C-W>l :window-right]
                        :k [:<C-W>k :window-up]
                        :H [:<C-W>5< :expand-window-left]
                        :J ["<cmd>resize +5<cr>" :expand-window-below]
                        :L [:<C-W>5> :expand-window-right]
                        :K ["<cmd>resize -5<cr>" :expand-window-up]
                        := [:<C-W>= :balance-window]
                        :s [:<C-W>s :split-window-below]
                        :v [:<C-W>v :split-window-vertical]}
                    :f {:name :+find/file
                        :f ["<cmd>lua require'telescope.builtin'.find_files({ hidden = true, find_command = {'rg', '--files', '--iglob', '!.git', '--no-ignore', '--hidden'}})<cr>"
                            "Files (include ignored)"]
                        :r ["<cmd>Telescope oldfiles<cr>" "Open Recent File"]
                        :g ["<cmd>Telescope live_grep<cr>" :Grep]
                        :n [:<cmd>enew<cr> "New File"]
                        :p ["<cmd>lua require'telescope.builtin'.find_files({ cwd = '~/dotfiles' })<cr>"
                            "Config files"]
                        :c ["<cmd>Telescope commands<cr>" :Commands]
                        :b ["<cmd>Telescope buffers<cr>" :Buffers]
                        :t [:<cmd>Telescope<cr> "Telescope builtins"]
                        ":" ["<cmd>Telescope command_history<cr>"
                             "Command history"]}
                    :t {:name :+tabs
                        :n [:<cmd>tabn<cr> "next tab"]
                        :p [:<cmd>tabp<cr> "previous tab"]
                        :c [:<cmd>tabclose<cr> "close tab"]
                        :t ["<cmd>tabe | term<cr>"
                            "open a terminal in a new tab"]
                        :o [:<cmd>tabe<cr> "open new tab"]}
                    :b {:name :+buffers
                        :d [:<cmd>bd<cr> "delete buffer"]
                        :n [:<cmd>bn<cr> "next buffer"]
                        :p [:<cmd>bp<cr> "previous buffer"]}
                    :g {:name :+git
                        :s [:<cmd>Git<cr> "git status"]
                        :d [:<cmd>Gvdiffsplit!<cr>
                            "git diff in vertical split"]
                        :b ["<cmd>Git blame<cr>" "git blame"]}}
                   {:prefix :<leader> :mode :n}))

(defn register-lsp-mappings []
      (wk.register {:c {:name "+code-actions (lsp)"
                        :a ["<cmd>lua vim.lsp.buf.code_action()<cr>"
                            "lsp code actions"]
                        :f ["<cmd>Telescope lsp_references<cr>"
                            "find references"]
                        :r ["<cmd>lua vim.lsp.buf.rename()<cr>"
                            "rename current symbol"]
                        :d ["<cmd>Telescope lsp_definitions<cr>"
                            "jump to definition"]
                        :D ["<cmd>lua vim.lsp.buf.declaration()<cr>"
                            "jump to declaration"]
                        :i ["<cmd>lua vim.lsp.buf.implementation()<cr>"
                            "jump to implmentation"]
                        :t ["<cmd>lua vim.lsp.buf.type_definition()<cr>"
                            "jump to type definition"]}}
                   {:buffer bufnr :mode :n :prefix :<leader>}))

