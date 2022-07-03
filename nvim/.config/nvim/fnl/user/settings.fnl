(module user.settings {autoload {nvim aniseed.nvim a aniseed.core}})

(defn- settings-fn [t]
	(fn [...]
		(let [opts [...]]
			(for [i 1 (a.count opts) 2]
				(tset t (. opts i) (. opts (+ i 1)))))))

(local options (settings-fn nvim.o))
(local global-options (settings-fn nvim.g))

;; fnlfmt: skip
(options
	:number true
	:relativenumber true
	:hidden true
	:tabstop 4
	:shiftwidth 4
	:expandtab false
	:wrap false
	:linebreak true
	:wildmenu true
	:wildmode "longest,full"
	:timeoutlen 200
	:fixeol false
	:scrolloff 8
	:sidescrolloff 16
	:hlsearch false
	:cursorline true
	:termguicolors true
	:mouse "nv"
	:signcolumn "yes"
	:syntax "on")

;; fnlfmt: skip
(global-options
	:EditorConfig_exclude_patterns ["fugitive://.*" "scp://.*"]
	:user_emmet_leader_key :<C-H>
	:vue_disable_pre_processors 1
	:jsx_ext_required 0
	:mapleader " "
	:maplocalleader ",")

;; fnlfmt: skip
(a.run!
	vim.cmd
	["let $NVIM_TUI_ENABLE_TRUE_COLOR=1"
	 "syntax on"
	 "filetype plugin indent on"])

