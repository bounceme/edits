export PATH=/usr/local/bin:$PATH
export PATH=$HOME/node_modules/.bin:$PATH
export BASHRC_ACTIVE=1
PS1="\h:\W \u\$ "


alias brewski='brew update && brew upgrade --all && brew cleanup; brew doctor'

shopt -s globstar &> /dev/null

# Alias vi to $EDITOR, which in turn call editor()
alias emacs='emacs -nw'
alias vi=editor

# Use neovim instead of vim if installed or vi if all else fails
function editor() {
	if [ $# -ne 0 ]; then
		if hash nvim >/dev/null 2>&1; then
			nvim "$@" "+set viminfo="
		elif hash vim >/dev/null 2>&1; then
			vim "$@" "+set viminfo="
		else
			vi "$@" "+set viminfo="
		fi
	else
		if hash nvim >/dev/null 2>&1; then
			nvim
		elif hash vim >/dev/null 2>&1; then
			vim
		else
			vi
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
export HISTFILE
HISTCONTROL=erasedups:ignoredups
HISTSIZE=20000
HISTFILESIZE=20000
