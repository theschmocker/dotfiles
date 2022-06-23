local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
	---@diagnostic disable-next-line: lowercase-global
	packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd [[packadd packer.nvim]]

-- Misc plugin-related global vars
vim.g.EditorConfig_exclude_patterns = { 'fugitive://.*', 'scp://.*' }
vim.g.user_emmet_leader_key = '<C-H>'
-- vim.g.vim_jsx_pretty_disable_tsx = 1
vim.g.vue_disable_pre_processors = 1
vim.g.jsx_ext_required = 0

return require('packer').startup({
	function(use)
		use 'neovim/nvim-lspconfig'
		use "williamboman/nvim-lsp-installer"

		use {
			'ggandor/lightspeed.nvim',
			commit = '22eb8615f13e627f4b0440ebcd4381cab3df293e';
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

		use 'folke/tokyonight.nvim'
		-- use 'flazz/vim-colorschemes'
		-- use 'dracula/vim'
		-- use 'Mofiqul/dracula.nvim'
		-- use 'haishanh/night-owl.vim'
		-- use 'arcticicestudio/nord-vim'
		use {
			'rose-pine/neovim',
			as = 'rose-pine',
			tag = 'v1.1.0', -- Optional tag release
			config = function ()
				require'rose-pine'.setup {
					dark_variant = 'moon',
				}

				vim.cmd('colorscheme rose-pine')
			end
		}


		-- Syntax/Language
		-- use 'pangloss/vim-javascript'
		-- use 'leafgarland/typescript-vim'
		-- use 'MaxMEllon/vim-jsx-pretty'
		-- use 'ianks/vim-tsx'
		use 'jparise/vim-graphql'
		use 'hail2u/vim-css3-syntax'
		use 'posva/vim-vue'
		use 'nelsyeung/twig.vim'
		use 'jwalton512/vim-blade'
		use 'evanleck/vim-svelte'
		use 'fatih/vim-go'
		use 'StanAngeloff/php.vim'
		use 'rust-lang/rust.vim'

		-- Snippets
		use {
			'L3MON4D3/LuaSnip',
			config = function ()
				require'snippets'
			end
		}

		-- Completion
		use 'hrsh7th/cmp-nvim-lsp'
		use 'hrsh7th/cmp-buffer'
		use 'hrsh7th/cmp-path'
		use 'hrsh7th/cmp-cmdline'
		use 'hrsh7th/nvim-cmp'
		use 'saadparwaiz1/cmp_luasnip'

		-- Utilities
		-- use 'Townk/vim-autoclose'
		use 'jiangmiao/auto-pairs'
		use 'tpope/vim-surround'
		use 'tpope/vim-fugitive'
		use 'tpope/vim-commentary'
		use 'tpope/vim-repeat'
		use 'mattn/emmet-vim'

		use {
			"folke/which-key.nvim",
			config = function()
				require("which-key").setup()
				local which = require('which-key-config')
				which.register_global_mappings()
			end
		}

		use 'editorconfig/editorconfig-vim'

		use {
			'kyazdani42/nvim-web-devicons',
			commit = '344331467509802e1af200f08ec3da278be5cbba';
		}

		use {
			'nvim-lualine/lualine.nvim',
			config = function()
				require'lualine'.setup {
					options = {
						icons_enabled = true,
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
					highlight = {
						enable = true,
					}
				}
			end
		}

		use 'Olical/conjure'

		-- Automatically set up your configuration after cloning packer.nvim
		-- Put this at the end after all plugins
		if packer_bootstrap then
			require('packer').sync()
		end

	end,
	config = {
		snapshot = "packer-lock",
		snapshot_path = vim.fn.stdpath('config')
	}
})
