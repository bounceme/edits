export PATH=/usr/local/bin:$PATH
# ln -sf $(find edits/ -type f | grep -v git) ./
export PATH=$HOME/node_modules/.bin:$PATH
export LYNX_LSS=$HOME/lynx.lss
PS1="\h:\W \u\$ "


alias brewski='brew update && brew upgrade --all && brew cleanup; brew doctor'
alias mpdrefresh='{ killall mpd; rm ~/.mpd/mpd.db; touch ~/.mpd/mpd.db; mpd; \
if ! ps -x | grep mpdkeys | grep python ; then ( mpdkeys & ) ; fi ;} &>/dev/null'

shopt -s globstar &> /dev/null

# Alias vi to $EDITOR, which in turn call editor()
alias emacs='emacs -nw'
	alias vi=editor

# Use neovim instead of vim if installed or vi if all else fails
function editor() {
	if [ $# -ne 0 ]; then
		if hash nvim >/dev/null 2>&1; then
			nvim "$@" "+set viminfo="
		else
			vim "$@" "+set viminfo="
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

kill $(ps aux | grep mpd-loop | grep -v grep | grep -o '[0-9]*' | head -n 1) &>/dev/null
(nohup ./mpd-loop &>/dev/null &)
