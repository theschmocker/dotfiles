alias please='sudo $(history -p !!)'
alias web='cd ~/web-projects'
alias ..='cd ..'
alias ...='cd ../..'

# Git
alias gs='git status'
alias gd='git diff'
alias gp='git push'
alias ga='git add'
alias gc='git commit'

# In case I fall into old habits
alias vim='nvim'

# browser-sync
alias srv="browser-sync start --server --files '**/*.css **/*.html **/*.js'"

# thefuck
eval $(thefuck --alias)
