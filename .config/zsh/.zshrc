###################################zplug############################################

source $ZPLUG_HOME/init.zsh

zplug mafredri/zsh-async, from:github

# Theme
zplug "sindresorhus/pure", use:pure.zsh, from:github, as:theme
zstyle :prompt:pure:git:stash show yes
zstyle :prompt:pure:path color red
zstyle :prompt:pure:git:branch color yellow
zstyle :prompt:pure:prompt:success color green
zstyle :prompt:pure:git:arrow blue

# zplug "subnixr/minimal", as:theme
# # MNML_NOMRAL_CHAR='-'
# MNML_OK_COLOR=3

# Autojump using z directory
zplug "agkozak/zsh-z"

# syntax
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# autosugestion
zplug "zsh-users/zsh-autosuggestions"

# vim mode
zplug "softmoth/zsh-vim-mode"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

zplug load

# Enable colors and change prompt:
autoload -U colors && colors

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
mkdir -p ~/.cache/zsh
touch ~/.cache/zsh/history
HISTFILE=~/.cache/zsh/history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
# bindkey -v
# export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
# bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# aliases

#dotfiles
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
alias dotstatus='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME status'
alias dotlg='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME lg'
alias dotadd='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME add -u'
alias dotaddnew='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME add'
alias dotcommit='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME commit'
alias dotpush='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME push'
alias lazydotfiles='lazygit --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'

# systemctl
alias ctl='sudo systemctl'
alias ctlstatus='sudo systemctl status'
alias ctlstart='sudo systemctl start'
alias ctlstop='sudo systemctl stop'
alias ctlrestart='sudo systemctl restart'

# better ls
alias ls='lsd --group-dirs first'
alias l='ls -l --blocks name,size,permission,user,group,date'
alias la='ls -a'
alias lla='ls -la --blocks name,size,permission,user,group,date'
alias lt='ls --tree --depth 3'
alias lta='ls --tree'

#ssh conect to linode scumbags
alias linode='ssh eduardo@lonighicode.com'

#ssh to cconecct to archserver
alias archserver='ssh eduardo@176.58.110.157'

#mount alma smb
# alias start-library='cd /home/eduardo/alma/web-volume-viewer && yarn dev'
alias start-library='cd /home/eduardo/alma/web-volume-viewer && yarn startPy3'
alias start-library-live='cd /home/eduardo/alma/web-volume-viewer && yarn dev-live'
alias start-viewer-local='cd /home/eduardo/alma/web-viewer && yarn start-local & kitty -d /home/eduardo/alma/backend-visor-node yarn start '
# dir size
alias dirsize='du -h -d 1'
alias sudodirsize='sudo du -h -d 1'

#pretty cat
alias cat='bat'

# exit vim style
alias :q='exit'
alias :Q='exit'

alias vim='nvim'
# alias nv='nvim'
# alias nv='~/nvim.appimage'
# alias nv='tvim'
# alias tv='tvim'
# alias nv='newvim'
alias nv='towervim'

# alias e='sw emacsclient -c'
alias e='emacsclient -c -a emacs'

alias cp="cp -i"                          # confirm before overwriting something
# alias rm="rm -i"                          # confirm before removing
alias urm="/bin/rm -i"                          # confirm before removing
alias rrm="/bin/rm -i"                          # confirm before removing
alias rm="saferm.sh"                          # confirm before removing
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

# alias nnn='NNN_FIFO="$(mktemp -u)" nnn'
# nnn quit cd
n ()
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}



###################################extract############################################
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# Codi
# Usage: codi [filetype] [filename]
codi() {
  local syntax="${1:-python}"
  shift
  vim -c \
    "let g:startify_disable_at_vimenter = 1 |\
    set bt=nofile ls=0 noru nonu nornu |\
    hi ColorColumn ctermbg=NONE |\
    hi VertSplit ctermbg=NONE |\
    hi NonText ctermfg=0 |\
    Codi $syntax" "$@"
}
