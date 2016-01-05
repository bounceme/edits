[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export PATH=/usr/local/bin:$PATH
export PIP2EVAL_TMP_FILE_PATH=/tmp/

# Alias vi to $EDITOR, which in turn call editor()
alias vi=editor

# Use neovim instead of vim if installed or vi if all else fails
function editor() {
if hash nvim >/dev/null 2>&1; then
	nvim $@
elif hash vim >/dev/null 2>&1; then
	vim $@
else
	vi $@
fi
}

if [ -f /usr/local/etc/bash_completion ]; then
	source /usr/local/etc/bash_completion
elif [ -f /etc/bash_completion ] && ! shopt -oq posix; then
	. /etc/bash_completion
fi

fe() {
	local file
	file=$(fzf --query="$1" --select-1 --exit-0)
	[ -n "$file" ] && ${EDITOR:-vim} "$file"
}

fd() {
	local dir
	dir=$(find ${1:-*} -path '*/\.*' -prune \
		-o -type d -print 2> /dev/null | fzf +m) &&
		cd "$dir"
}
