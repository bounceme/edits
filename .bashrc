export PATH=/usr/local/bin:$PATH
# ln -sf $(find edits/ -type f | grep -v git) ./
export PATH=$HOME/node_modules/.bin:$PATH
export PATH=$HOME/Library/Python/3.6/bin:$PATH

export XDG_CONFIG_HOME=$HOME/.config

export LYNX_LSS=$HOME/lynx.lss

PS1="\h:\W \u\$ "


alias brewski='brew update && brew upgrade --all && brew cleanup; brew doctor'
if [ "$(uname)" = "Darwin" ]; then
	alias mposx='{ pkill -f mpd ; rm $HOME/.mpd/mpd.db ; touch $HOME/.mpd/mpd.db ; mpd ; \
($HOME/mpd-loop & mpdkeys &) ; } > /dev/null 2>&1 ; osascript -e '\''quit app "Emacs"'\'' ; sleep 1 ; open -a Emacs'
else
	alias mposx='echo "$(Uname)"'
fi

shopt -s globstar > /dev/null 2>&1

# Alias vi to $EDITOR, which in turn call editor()
alias emacs='emacs -nw'
if hash nvim > /dev/null 2>&1 || hash vim > /dev/null; then
	alias vi=editor
fi

# Use neovim instead of vim if installed or vi if all else fails
function editor() {
	if hash nvim > /dev/null 2>&1; then
		if [ $# -ne 0 ]; then
			nvim -i NONE "$@"
		else
			nvim
		fi
	else
		if [ $# -ne 0 ]; then
			vim -i NONE "$@"
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
