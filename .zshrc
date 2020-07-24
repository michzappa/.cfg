export PATH=~/.emacs.d/bin:$PATH
export PS1="%n@%M %~ %% "

alias brighten="~/.scripts/increment_screen_backlight.sh"
alias dim="~/.scripts/decrement_screen_backlight.sh"
# zplug
source ~/.zplug/init.zsh
zplug "kazhala/dotbare"

zplug load

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
unsetopt beep
bindkey -v

zstyle :compinstall filename '/home/michael/.zshrc'

autoload -Uz compinit
compinit

# commands to run on startup
neofetch
# update my dotfiles from my github repo
dotbare pull
