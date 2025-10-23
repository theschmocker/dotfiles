# Path to oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="af-magic"
#
zstyle ':omz:update' frequency 30
plugins=(
  git
  nvm
)

source "$ZSH/oh-my-zsh.sh"

if [[ -f /opt/homebrew/bin/brew ]]; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Clean up af-magic theme prompt a bit
RPS1=""
function afmagic_dashes { }

# /oh-my-zsh


# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

source ~/.bash_aliases

bindkey -v
bindkey '^K' up-line-or-history
bindkey '^J' down-line-or-history
bindkey '^P' up-line-or-history
bindkey '^N' down-line-or-history


# Android
# TODO: move these into .env.zsh on work machine
export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin

# Go
export GOPATH=$HOME/go
export PATH=$PATH:${GOPATH//://bin:}/bin

# Deno
export DENO_INSTALL="$HOME/.deno"
export PATH="$DENO_INSTALL/bin:$PATH"

# Emacs
export PATH=$HOME/.emacs.d/bin:$PATH
export PATH=$HOME/.config/emacs/bin:$PATH

# Various path inclusions
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.composer/vendor/bin:$PATH

# FZF
# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

export DOTNET_CLI_TELEMETRY_OPTOUT=1

nvc () {
	local config_path="$DOTFILES_PATH/nvim/.config/nvim"
	nvim "$config_path" -c "cd $config_path"
}

dots () {
	cd "$DOTFILES_PATH"
}

prj () {
	local projects_path="$HOME/projects"
	local choice="$(ls $projects_path | fzf --prompt 'cd to project:')"
	cd "$HOME/projects/$choice"
}

if [[ -f $HOME/.env.zsh ]]; then
  source $HOME/.env.zsh
fi

