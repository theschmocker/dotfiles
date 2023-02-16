(module user.plugins {autoload {: packer} require-macros [user.macros]})

(macro setup! [plugin-name config]
  (let [config (or config {})]
    `(let [setup# (. (require ,plugin-name) :setup)]
       (setup# ,config))))

(macro plugins! [...]
  `(packer.startup [(fn [use#]
                      ,(unpack (icollect [_ form (ipairs [...])]
                                 (let [[ident name config] form
                                       config (or config {})]
                                   (assert-compile (= :use (tostring ident)) "invalid form. plugin declaration must start with 'use'." form)
                                   (tset config 1 name)
                                   `(use# ,config)))))]))

(plugins!
  ;; LSP
  (use :williamboman/mason.nvim {:commit :c609775d1
                                 :config #(setup! :mason)})
  (use :williamboman/mason-lspconfig.nvim {:commit :3751eb5c})
  (use :neovim/nvim-lspconfig {:commit :f0221821})

  ;; Colorscheme
  (use :folke/tokyonight.nvim {:commit :8223c97})
  (use :rose-pine/neovim
       {:as :rose-pine
        :commit :8e800a9
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
  ;; TODO: check compat with 0.8.0
  (use :L3MON4D3/LuaSnip
       {:commit :a12441e :config #(require :user.snippets)})

  ;; Completion
  (use :hrsh7th/cmp-nvim-lsp {:commit :0e6b2ed})
  (use :hrsh7th/cmp-buffer {:commit :3022dbc})
  (use :hrsh7th/cmp-path {:commit :91ff86c})
  (use :hrsh7th/cmp-cmdline {:commit :23c51b2})
  (use :hrsh7th/nvim-cmp {:commit :208d69f})

  (use :saadparwaiz1/cmp_luasnip {:commit :1809552})

  ;; Tools
  (use :phaazon/hop.nvim
       {:commit :90db1b2
        :config #(setup! :hop {:keys "asdfhjkl"})})

  (use :nvim-telescope/telescope.nvim
       {:commit :1ba7278cf08
        :requires [[:nvim-lua/plenary.nvim]]
        :config #(setup! :telescope
                         {:defaults {;:path_display {:truncate true}
                                     :layout_strategy "vertical"
                                     :layout_config {:width 0.99999
                                                     :height 0.99999}}
                          :pickers {:find_files {:hidden true
                                                 :find_command [:rg
                                                                :--files
                                                                :--iglob
                                                                :!.git
                                                                :--hidden]}}})})
  (use :windwp/nvim-autopairs
       {:commit :f00eb3b7
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

  ;; Lisp
  (use :Olical/aniseed)
  (use :Olical/conjure)

  ;; Which Key
  (use :folke/which-key.nvim
       {:commit :98321b6 :config #(setup! :which-key)})

  ;; Icons
  (use :kyazdani42/nvim-web-devicons
       {:commit :6c389263})

  ;; Status line
  (use :nvim-lualine/lualine.nvim
       {:commit :0050b308
        :config #(setup! :lualine {:options {:icons_enabled true}})})

  ;; Treesitter
  (use :nvim-treesitter/nvim-treesitter
       {:commit :bf5be49d
        :run ":TSUpdate"
        :config #(setup! :nvim-treesitter.configs
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
                                             :haskell]})}))

