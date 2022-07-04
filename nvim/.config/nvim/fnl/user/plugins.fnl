(module user.plugins {autoload {: packer}})

(packer.startup [(fn [use-]
                   (fn use [name config]
                     (let [config (or config {})]
                       (tset config 1 name)
                       (use- config)))

                   (use :neovim/nvim-lspconfig {:commit :c107a0f})
                   (use :williamboman/nvim-lsp-installer {:commit :9887370})
                   (use :ggandor/lightspeed.nvim
                        {:commit :79519bfae95741bc99872582ef0f268fd842115b
                         :config (fn []
                                   ((. (require :lightspeed) :setup) {:exit_after_idle_msecs {:labeled 1500
                                                                                              :unlabeled 1000}}))})
                   (use :nvim-telescope/telescope.nvim
                        {:commit :d88b44d
                         :requires {1 {1 :nvim-lua/plenary.nvim}}
                         :config (fn []
                                   ((. (require :telescope) :setup) {:defaults {:path_display {:truncate true}}
                                                                     :pickers {:find_files {:hidden true
                                                                                            :find_command [:rg
                                                                                                           :--files
                                                                                                           :--iglob
                                                                                                           :!.git
                                                                                                           :--hidden]}}}))})
                   (use :folke/tokyonight.nvim {:commit :8223c97})
                   (use :rose-pine/neovim
                        {:as :rose-pine
                         :tag :v1.1.0
                         :config (fn []
                                   ((. (require :rose-pine) :setup) {:dark_variant :moon}))})
                   (use :catppuccin/nvim
                        {:as :catppuccin
                         :commit :d46425163dad4cc74910c0c81eeedb00cadf8a61
                         :config (fn []
                                   ((. (require :catppuccin) :setup) {:integrations {:cmp true
                                                                                     :which_key true
                                                                                     :lightspeed true}})
                                   (set vim.g.catppuccin_flavour :macchiato)
                                   (vim.cmd "colorscheme catppuccin"))})
                   (use :EdenEast/nightfox.nvim
                        {:commit :b85c5c3a0e3b309ffa7d0a6ca33e430c91532ba0})
                   (use :nelsyeung/twig.vim {:commit :014cd47})
                   (use :fatih/vim-go {:commit :b7506c6d})
                   (use :rust-lang/rust.vim {:commit :4aa69b8})
                   (use :L3MON4D3/LuaSnip
                        {:commit :a12441e
                         :config (fn []
                                   (require :user.snippets))})
                   (use :hrsh7th/cmp-nvim-lsp {:commit :affe808})
                   (use :hrsh7th/cmp-buffer {:commit :62fc67a})
                   (use :hrsh7th/cmp-path {:commit :466b6b8})
                   (use :hrsh7th/cmp-cmdline {:commit :c36ca4b})
                   (use :hrsh7th/nvim-cmp {:commit :df6734a})
                   (use :saadparwaiz1/cmp_luasnip {:commit :a9de941})
                   (use :windwp/nvim-autopairs
                        {:commit :4a95b3982be7397cd8e1370d1a09503f9b002dbf
                         :config (fn []
                                   ((. (require :nvim-autopairs) :setup)))})
                   (use :tpope/vim-surround {:commit :bf3480d})
                   (use :tpope/vim-fugitive {:commit :69ead80})
                   (use :tpope/vim-commentary {:commit :3654775})
                   (use :tpope/vim-repeat {:commit :24afe92})
                   (use :mattn/emmet-vim {:commit :def5d57})
                   (use :tommcdo/vim-exchange
                        {:commit :784d63083ad7d613aa96f00021cd0dfb126a781a})
                   (use :folke/which-key.nvim
                        {:commit :bd4411a
                         :config (fn []
                                   ((. (require :which-key) :setup))
                                   (local which (require :user.which-key))
                                   ((. which :register-global-mappings)))})
                   (use :editorconfig/editorconfig-vim {:commit :d354117})
                   (use :kyazdani42/nvim-web-devicons
                        {:commit :344331467509802e1af200f08ec3da278be5cbba})
                   (use :nvim-lualine/lualine.nvim
                        {:commit :b656978
                         :config (fn []
                                   ((. (require :lualine) :setup) {:options {:icons_enabled true}}))})
                   (use :nvim-treesitter/nvim-treesitter
                        {:commit :8eccd82
                         :run ":TSUpdate"
                         :config (fn []
                                   ((. (require :nvim-treesitter.configs)
                                       :setup) {:highlight {:enable true}}))})
                   (use :Olical/aniseed)
                   (use :Olical/conjure))])

