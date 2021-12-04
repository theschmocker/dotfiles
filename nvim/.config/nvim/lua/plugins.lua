local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd [[packadd packer.nvim]]

-- Misc plugin-related global vars
vim.g.EditorConfig_exclude_patterns = { 'fugitive://.*', 'scp://.*' }
vim.g.user_emmet_leader_key = '<C-H>'
vim.g.vim_jsx_pretty_disable_tsx = 1
vim.g.vue_disable_pre_processors = 1
vim.g.jsx_ext_required = 0

return require('packer').startup(function(use)
  use {
    'ggandor/lightspeed.nvim',
    config = function()
      require'lightspeed'.setup {
        exit_after_idle_msecs = { labeled = 1500, unlabeled = 1000 },
      }
    end
  }

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} },
    config = function()
      require'telescope'.setup {
        pickers = {
          find_files = {
            hidden = true,
            find_command = { 'rg', '--files', '--iglob', '!.git', '--hidden' },
          }
        }
      }
    end
  }

  -- use 'flazz/vim-colorschemes'
  -- use 'dracula/vim'
  use 'Mofiqul/dracula.nvim'
  use 'haishanh/night-owl.vim'
  use 'arcticicestudio/nord-vim'

  -- Syntax/Language
  use 'pangloss/vim-javascript'
  use 'leafgarland/typescript-vim'
  use 'MaxMEllon/vim-jsx-pretty'
  use 'ianks/vim-tsx'
  use 'jparise/vim-graphql'
  use 'hail2u/vim-css3-syntax'
  use 'posva/vim-vue'
  use 'nelsyeung/twig.vim'
  use 'jwalton512/vim-blade'
  use 'evanleck/vim-svelte'
  use 'fatih/vim-go'
  use 'StanAngeloff/php.vim'

  -- Completion
  use {
    'neoclide/coc.nvim',
    branch = 'release'
  }

  -- Utilities
  use 'Townk/vim-autoclose'
  use 'tpope/vim-surround'
  use 'tpope/vim-fugitive'
  use 'tpope/vim-commentary'
  use 'mattn/emmet-vim'

  use {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup()
      require('which-key-config')
    end
  }

  use 'editorconfig/editorconfig-vim'

  use {
    'nvim-lualine/lualine.nvim',
    -- requires = { 'kyazdani42/nvim-web-devicons', opt = true }
    config = function()
      require'lualine'.setup {
        options = {
          icons_enabled = false,
          theme = 'rose-pine'
        }
      }
    end
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function() 
      require'nvim-treesitter.configs'.setup {
        ensure_installed = "maintained",
        highlight = {
          enable = true,
        }
      }
    end
  }

  use {
    'rose-pine/neovim',
    as = 'rose-pine',
    tag = 'v0.1.0', -- Optional tag release
    config = function()
      -- vim.cmd('colorscheme rose-pine')
    end
  }

  use 'shaunsingh/moonlight.nvim'

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end

end)
