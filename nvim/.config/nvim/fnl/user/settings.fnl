(module user.settings {autoload {nvim aniseed.nvim a aniseed.core}
                       require-macros [user.macros]})

;; fnlfmt: skip
(set!
	number true
	relativenumber true
	hidden true
	tabstop 4
	shiftwidth 4
	expandtab false
	wrap false
	linebreak true
	wildmenu true
	wildmode "longest,full"
	timeoutlen 200
	fixeol false
	scrolloff 8
	sidescrolloff 16
	hlsearch false
	cursorline true
	termguicolors true
	mouse "nv"
	signcolumn "yes"
	syntax "on"
    swapfile false
    updatetime 250 ; decrease time for CursorHold
    )

;; fnlfmt: skip
(let-g!
	EditorConfig_exclude_patterns ["fugitive://.*" "scp://.*"]
	user_emmet_leader_key :<C-H>
	vue_disable_pre_processors 1
	jsx_ext_required 0
	mapleader " "
	maplocalleader "\\")

(set! background :dark)
(vim.cmd "colorscheme rose-pine")

;; fnlfmt: skip
(a.run!
	vim.cmd
	["let $NVIM_TUI_ENABLE_TRUE_COLOR=1"
	 "syntax on"
	 "filetype plugin indent on"])

