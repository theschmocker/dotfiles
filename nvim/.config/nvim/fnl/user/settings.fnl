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
    splitright true
    splitbelow true
    inccommand "split"
    )

;; fnlfmt: skip
(let-g!
	EditorConfig_exclude_patterns ["fugitive://.*" "scp://.*"]
	user_emmet_leader_key :<C-H>
	vue_disable_pre_processors 1
	jsx_ext_required 0
	mapleader " "
	maplocalleader "\\"
    conjure#client#scheme#stdio#command "csi -quiet -:c"
    conjure#client#scheme#stdio#prompt_pattern "\n-#;%d-> ")

(set! background :dark)
(vim.cmd "colorscheme rose-pine")

;; fnlfmt: skip
(a.run!
	vim.cmd
	["let $NVIM_TUI_ENABLE_TRUE_COLOR=1"
	 "syntax on"
	 "filetype plugin indent on"])

