(module user.plugins {autoload {: packer}
                      require-macros [user.macros]})

(macro with-require [[mod-name var-ident] body]
  `(let [,var-ident (require ,mod-name)]
     ,body))

(macro setup! [plugin-name ...]
  (let [[config] (or [...] {})]
    `(let [setup# (. (require ,plugin-name) :setup)]
       (setup# ,config))))

(packer.startup [(fn [use-]
                   (fn use [name config]
                     (let [config (or config {})]
                       (tset config 1 name)
                       (use- config)))

                   ;; LSP
                   (use :neovim/nvim-lspconfig {:commit :c107a0f})
                   (use :williamboman/nvim-lsp-installer {:commit :9887370})

                   ;; Colorscheme
                   (use :folke/tokyonight.nvim {:commit :8223c97})
                   (use :rose-pine/neovim
                        {:as :rose-pine
                         :tag :v1.1.0
                         :config #(setup! :rose-pine {:dark_variant :moon})})
                   (use :catppuccin/nvim
                        {:as :catppuccin
                         :commit :d46425163dad4cc74910c0c81eeedb00cadf8a61
                         :config #(setup! :catppuccin
                                         {:integrations {:cmp true
                                                         :which_key true
                                                         :lightspeed true}})})
                   (use :EdenEast/nightfox.nvim
                        {:commit :b85c5c3a0e3b309ffa7d0a6ca33e430c91532ba0})

                   ;; File types
                   (use :nelsyeung/twig.vim {:commit :014cd47})
                   (use :fatih/vim-go {:commit :b7506c6d})
                   (use :rust-lang/rust.vim {:commit :4aa69b8})

                   ;; Snippets
                   (use :L3MON4D3/LuaSnip
                        {:commit :a12441e
                         :config #(require :user.snippets)})

                   ;; Completion
                   (use :hrsh7th/cmp-nvim-lsp {:commit :affe808})
                   (use :hrsh7th/cmp-buffer {:commit :62fc67a})
                   (use :hrsh7th/cmp-path {:commit :466b6b8})
                   (use :hrsh7th/cmp-cmdline {:commit :c36ca4b})
                   (use :hrsh7th/nvim-cmp {:commit :df6734a})
                   (use :saadparwaiz1/cmp_luasnip {:commit :a9de941})

                   ;; Tools
                   (use :ggandor/lightspeed.nvim
                        {:commit :79519bfae95741bc99872582ef0f268fd842115b
                         :config #(setup! :lightspeed
                                          {:exit_after_idle_msecs {:labeled 1500
                                                                   :unlabeled 1000}})})
                   (use :nvim-telescope/telescope.nvim
                        {:commit :d88b44d
                         :requires [[:nvim-lua/plenary.nvim]]
                         :config #(setup! :telescope
                                          {:defaults {:path_display {:truncate true}}
                                           :pickers {:find_files {:hidden true
                                                                  :find_command [:rg
                                                                                 :--files
                                                                                 :--iglob
                                                                                 :!.git
                                                                                 :--hidden]}}})})
                   (use :windwp/nvim-autopairs
                        {:commit :4a95b3982be7397cd8e1370d1a09503f9b002dbf
                         :config #(setup! :nvim-autopairs
                                          {:enable_check_bracket_line false})})
                   (use :tpope/vim-surround {:commit :bf3480d})
                   (use :tpope/vim-fugitive {:commit :69ead80})
                   (use :tpope/vim-commentary {:commit :3654775})
                   (use :tpope/vim-repeat {:commit :24afe92})
                   (use :mattn/emmet-vim {:commit :def5d57})
                   (use :tommcdo/vim-exchange
                        {:commit :784d63083ad7d613aa96f00021cd0dfb126a781a})
                   (use :editorconfig/editorconfig-vim {:commit :d354117})

                   ;; Which Key
                   (use :folke/which-key.nvim
                        {:commit :bd4411a
                         :config #(setup! :which-key)})

                   ;; Icons
                   (use :kyazdani42/nvim-web-devicons
                        {:commit :344331467509802e1af200f08ec3da278be5cbba})

                   ;; Status line
                   (use :nvim-lualine/lualine.nvim
                        {:commit :b656978
                         :config #(setup! :lualine
                                          {:options {:icons_enabled true}})})
                   (use :nvim-treesitter/nvim-treesitter
                        {:commit :8eccd82
                         :run ":TSUpdate"
                         :config (fn []
                                   (setup! :nvim-treesitter.configs
                                           {:highlight {:enable true}
                                            :ensure_installed [:html
                                                               :css
                                                               :scss
                                                               :javascript
                                                               :typescript
                                                               :tsx
                                                               :svelte
                                                               :vue
                                                               :lua
                                                               :fennel
                                                               :c_sharp
                                                               :rust
                                                               :markdown
                                                               :json
                                                               :jsonc
                                                               :go
                                                               :gomod
                                                               :haskell]}))})

                   ;; Lisp
                   (use :Olical/aniseed)
                   (use :Olical/conjure))])

