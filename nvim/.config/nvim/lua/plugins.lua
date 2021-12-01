local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  -- My plugins here
  -- use 'foo1/bar1.nvim'
  -- use 'foo2/bar2.nvim'
  -- use {
  --   'phaazon/hop.nvim',
  --   branch = 'v1',
  --   config = function()
  --     require'hop'.setup()
  --     print('hop should be initialized')
  --   end
  -- }

  use 'ggandor/lightspeed.nvim'

  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} },
  }

  use 'flazz/vim-colorschemes'
  use 'dracula/vim'
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

  -- php
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
  -- use 'liuchengxu/vim-which-key'
  use 'scrooloose/nerdtree'
  -- use 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  -- use 'junegunn/fzf.vim'
  --
  use {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup {
        -- your configuration comes here
        --       -- or leave it empty to use the default settings
        --             -- refer to the configuration section below
      }
    end
  }

  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end

end)

