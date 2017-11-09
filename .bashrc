export PATH=/usr/local/bin:$PATH
# ln -sf $(find edits/ -type f | grep -v git) ./
export PATH=$HOME/node_modules/.bin:$PATH
export PATH=$HOME/Library/Python/3.6/bin:$PATH

export LYNX_LSS=$HOME/lynx.lss
PS1="\h:\W \u\$ "


alias brewski='brew update && brew upgrade --all && brew cleanup; brew doctor'
alias mpdrefresh='{ pkill -f mpd ; rm $HOME/.mpd/mpd.db ; touch $HOME/.mpd/mpd.db ; mpd ; \
($HOME/mpd-loop & mpdkeys &) ; } > /dev/null 2>&1'

shopt -s globstar > /dev/null 2>&1

# Alias vi to $EDITOR, which in turn call editor()
alias emacs='emacs -nw'
	alias vi=editor

# Use neovim instead of vim if installed or vi if all else fails
function editor() {
	if [ $# -ne 0 ]; then
		if hash nvim > /dev/null 2>&1; then
			nvim "$@" "+set viminfo="
		else
			vim "$@" "+set viminfo="
		fi
	else
		if hash nvim > /dev/null 2>&1; then
			nvim
		else
			vim
		fi
	fi
}

GPG_TTY=`tty`
export GPG_TTY

if [ -f /usr/local/etc/bash_completion ]; then
	source /usr/local/etc/bash_completion
elif [ -f /etc/bash_completion ] && ! shopt -oq posix; then
	. /etc/bash_completion
fi
HISTCONTROL=erasedups:ignoredups
HISTSIZE=20000
HISTFILESIZE=20000
