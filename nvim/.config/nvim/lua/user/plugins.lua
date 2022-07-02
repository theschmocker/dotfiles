local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	---@diagnostic disable-next-line: lowercase-global
	packer_bootstrap = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
end

vim.cmd([[packadd packer.nvim]])

-- Misc plugin-related global vars
vim.g.EditorConfig_exclude_patterns = { "fugitive://.*", "scp://.*" }
vim.g.user_emmet_leader_key = "<C-H>"
-- vim.g.vim_jsx_pretty_disable_tsx = 1
vim.g.vue_disable_pre_processors = 1
vim.g.jsx_ext_required = 0

return require("packer").startup({
	function(use)
		use({
			"neovim/nvim-lspconfig",
			commit = "c107a0f",
		})
		use({
			"williamboman/nvim-lsp-installer",
			commit = "9887370",
		})

		use({
			"ggandor/lightspeed.nvim",
			commit = "79519bfae95741bc99872582ef0f268fd842115b",
			config = function()
				require("lightspeed").setup({
					exit_after_idle_msecs = { labeled = 1500, unlabeled = 1000 },
				})
			end,
		})

		use({
			"nvim-telescope/telescope.nvim",
			commit = "d88b44d",
			requires = { { "nvim-lua/plenary.nvim" } },
			config = function()
				require("telescope").setup({
					defaults = {
						path_display = {
							truncate = true,
						},
					},
					pickers = {
						find_files = {
							hidden = true,
							find_command = { "rg", "--files", "--iglob", "!.git", "--hidden" },
						},
					},
				})
			end,
		})

		use({
			"folke/tokyonight.nvim",
			commit = "8223c97",
		})
		-- use 'flazz/vim-colorschemes'
		-- use 'dracula/vim'
		-- use 'Mofiqul/dracula.nvim'
		-- use 'haishanh/night-owl.vim'
		-- use 'arcticicestudio/nord-vim'
		use({
			"rose-pine/neovim",
			as = "rose-pine",
			tag = "v1.1.0", -- Optional tag release
			config = function()
				require("rose-pine").setup({
					dark_variant = "moon",
				})

				-- vim.cmd('colorscheme rose-pine')
			end,
		})

		use({
			"catppuccin/nvim",
			as = "catppuccin",
			commit = "d46425163dad4cc74910c0c81eeedb00cadf8a61",
			config = function()
				require("catppuccin").setup({
					integrations = {
						cmp = true,
						which_key = true,
						lightspeed = true,
					},
				})

				vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha
				vim.cmd([[colorscheme catppuccin]])
			end,
		})

		use({
			"EdenEast/nightfox.nvim",
			commit = "b85c5c3a0e3b309ffa7d0a6ca33e430c91532ba0",
			config = function()
				-- vim.cmd("colorscheme nightfox")
			end,
		})

		use({
			"nelsyeung/twig.vim",
			commit = "014cd47",
		})
		use({
			"fatih/vim-go",
			commit = "b7506c6d",
		})
		use({
			"rust-lang/rust.vim",
			commit = "4aa69b8",
		})

		-- Snippets
		use({
			"L3MON4D3/LuaSnip",
			commit = "a12441e",
			config = function()
				require("user.snippets")
			end,
		})

		-- Completion
		use({
			"hrsh7th/cmp-nvim-lsp",
			commit = "affe808",
		})
		use({
			"hrsh7th/cmp-buffer",
			commit = "62fc67a",
		})
		use({
			"hrsh7th/cmp-path",
			commit = "466b6b8",
		})
		use({
			"hrsh7th/cmp-cmdline",
			commit = "c36ca4b",
		})
		use({
			"hrsh7th/nvim-cmp",
			commit = "df6734a",
		})
		use({
			"saadparwaiz1/cmp_luasnip",
			commit = "a9de941",
		})

		-- Utilities
		-- use 'Townk/vim-autoclose'
		-- use 'jiangmiao/auto-pairs'
		use({
			"windwp/nvim-autopairs",
			commit = "4a95b3982be7397cd8e1370d1a09503f9b002dbf",
			config = function()
				require("nvim-autopairs").setup()
			end,
		})

		use({
			"tpope/vim-surround",
			commit = "bf3480d",
		})
		use({
			"tpope/vim-fugitive",
			commit = "69ead80",
		})
		use({
			"tpope/vim-commentary",
			commit = "3654775",
		})
		use({
			"tpope/vim-repeat",
			commit = "24afe92",
		})
		use({
			"mattn/emmet-vim",
			commit = "def5d57",
		})

		use({
			"folke/which-key.nvim",
			commit = "bd4411a",
			config = function()
				require("which-key").setup()
				local which = require("user.which-key")
				which.register_global_mappings()
			end,
		})

		use({
			"editorconfig/editorconfig-vim",
			commit = "d354117",
		})

		use({
			"kyazdani42/nvim-web-devicons",
			commit = "344331467509802e1af200f08ec3da278be5cbba",
		})

		use({
			"nvim-lualine/lualine.nvim",
			commit = "b656978",
			config = function()
				require("lualine").setup({
					options = {
						icons_enabled = true,
						-- theme = 'rose-pine'
					},
				})
			end,
		})

		use({
			"nvim-treesitter/nvim-treesitter",
			commit = "8eccd82",
			run = ":TSUpdate",
			config = function()
				require("nvim-treesitter.configs").setup({
					highlight = {
						enable = true,
					},
				})
			end,
		})

		-- Automatically set up your configuration after cloning packer.nvim
		-- Put this at the end after all plugins
		if packer_bootstrap then
			require("packer").sync()
		end
	end,
})
