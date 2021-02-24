# Enable colors and change prompt:
autoload -U colors && colors

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}


# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

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

# dir size
alias dirsize='du -h -d 1'
alias sudodirsize='sudo du -h -d 1'

#pretty cat
alias cat='bat'

# exit vim style
alias :q='exit'
alias :Q='exit'

alias vi='nvim'
alias vim='nvim'
alias nv='nvim'


alias cp="cp -i"                          # confirm before overwriting something
alias rm="rm -i"                          # confirm before removing
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

alias nnn='NNN_FIFO="$(mktemp -u)" nnn'

# Load zsh-syntax-highlighting; should be last.
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

#load autojump
source /usr/share/autojump/autojump.zsh 2>/dev/null

# Load autosugestion
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
bindkey '^ ' autosuggest-accept


# pure promt
fpath+=$HOME/.config/zsh/pure
autoload -U promptinit; promptinit
prompt pure

zstyle :prompt:pure:git:stash show yes
zstyle :prompt:pure:path color red
zstyle :prompt:pure:git:branch color yellow
zstyle :prompt:pure:prompt:success color green
zstyle :prompt:pure:git:arrow blue


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
