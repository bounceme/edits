export PATH=/usr/local/bin:$PATH
export PATH=$HOME/node_modules/.bin:$PATH
PS1="\h:\W \u\$ "


alias brewski='brew update && brew upgrade --all && brew cleanup; brew doctor'

shopt -s globstar &> /dev/null

# Alias vi to $EDITOR, which in turn call editor()
alias emacs='emacs -nw'
if hash vim >/dev/null 2>&1; then
	alias vi=editor
fi

# Use neovim instead of vim if installed or vi if all else fails
function editor() {
	if [ $# -ne 0 ]; then
		if hash nvim >/dev/null 2>&1; then
			nvim -i NONE "$@"
		else
			vim -i NONE "$@"
		fi
	else
		if hash nvim >/dev/null 2>&1; then
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
