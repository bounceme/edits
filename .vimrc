filetype indent plugin on
set hidden
set autoindent
set showmode
set tabstop=4
set shiftwidth=4
set noexpandtab
set wildmenu
" set hlsearch
" set autochdir
autocmd BufEnter * silent! lcd %:p:h
" autocmd InsertEnter * let save_cwd = getcwd() | set autochdir
" autocmd InsertLeave * set noautochdir | execute 'cd' fnameescape(save_cwd)
set clipboard=unnamed,unnamedplus
set ttyfast
set lazyredraw
set ignorecase
set smartcase
set incsearch
set mouse=a
set t_vb=
set splitright
set splitbelow
set visualbell
set confirm
set laststatus=2
set ruler
set nostartofline
set autoindent
set backspace=indent,eol,start
set synmaxcol=400
set ffs=unix,dos
set ttimeoutlen=50
set autoread
" set cmdheight=2
set completeopt-=preview
set completeopt+=menuone
let g:netrw_localrmdir='rm -rf' " Allow netrw to remove non-empty local directories
runtime macros/matchit.vim
autocmd BufNewFile,BufRead *.txt,*.md setlocal spell spelllang=en_gb
au FileType vim setl keywordprg=:help

set number
set relativenumber
let g:netrw_bufsettings = 'noma nomod nu nobl nowrap ro'

if has('win32') || has('win64')
	set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

augroup myvimrchooks
	au!
	autocmd bufwritepost .vimrc source ~/.vimrc
augroup END

" Save Position in buffer
if has("autocmd")
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

set undofile
set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files

set statusline=%F
set statusline+=%y
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%=      "left/right separator
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file

nnoremap Q <Nop>

nnoremap zb :ls<cr>:b<space>

nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'

map Y y$
xnoremap <silent> y ygv<Esc>

" common mistakes
cnoreabbrev E! e!
cnoreabbrev W w
cnoreabbrev Q q

"command
nnoremap ! :!
noremap <Space> :

" window movement
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" fixcss
function! FixCSS()
	let pos = line( "." )
	silent :%s/{/{\r/g
	silent :%s/}/}\r\r/g
	silent :%s/;/;\r/g
	exe pos
endfunction
command! Fixcss call FixCSS()

autocmd FileType less nnoremap <Leader>m :w <BAR> !lessc % --autoprefix="last 2 versions" > %:t:r.css<CR><space>
" npm install -g less
" npm install -g less-plugin-autoprefix

"------------------------------------------------------------
"------------------------------------------------------------

" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/bundle')

" libraries, &c.
Plug 'xolox/vim-misc'
Plug 'junegunn/vim-pseudocl'
Plug 'kana/vim-textobj-user'
Plug 'xolox/vim-session'
Plug 'tpope/vim-repeat'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-vinegar'
Plug 'mopp/autodirmake.vim'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
" Plug 'mhinz/vim-signify'

" editing features
Plug 'junegunn/vim-oblique'
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
Plug 'vim-scripts/ReplaceWithRegister'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-function'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'thinca/vim-textobj-comment'
Plug 'tommcdo/vim-exchange'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
" Plug 'zweifisch/pipe2eval'
Plug 'bounceme/pipe2eval'
Plug 'idbrii/renamer.vim'

" syntax,indent &c.
" Plug 'pangloss/vim-javascript'
Plug 'gavocanov/vim-js-indent'
Plug 'moll/vim-node'
" Plug '1995eaton/vim-better-javascript-highlighting'
Plug 'lfilho/cosco.vim'
Plug 'scrooloose/syntastic'

" color,appearance
Plug 'flazz/vim-colorschemes'
Plug 'ap/vim-css-color'
Plug 'bling/vim-bufferline'
Plug 'valloric/MatchTagAlways'
Plug 'junegunn/rainbow_parentheses.vim'

" autocompleting
Plug 'ervandew/supertab'
Plug 'Raimondi/delimitMate'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'mattn/emmet-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'bonsaiben/bootstrap-snippets'

call plug#end()

if has('nvim')
	tnoremap <Esc><Esc> <C-\><C-N>
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
	let g:ctrlp_use_caching = 0
endif

colorscheme xoria256
hi Todo cterm=bold,underline ctermbg=234 ctermfg=96

hi link JavascriptNumber Number

" Tern
let g:tern_show_argument_hints = 'on_move'
" let g:tern_show_argument_hints='on_hold'
au FileType javascript nnoremap <silent> <buffer> K <esc>:TernDoc<CR>
au FileType javascript nnoremap <silent> <buffer> <esc> <C-W>z

" Vim Session
let g:session_persist_colors = 0
let g:session_autoload="yes"
let g:session_autosave="yes"

" ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="jj"
let g:UltiSnipsJumpBackwardTrigger="kk"

" Syntastic
let g:syntastic_check_on_open=1
" let g:syntastic_enable_signs=0

" emmet
let g:user_emmet_install_global = 0
autocmd FileType html,css,php EmmetInstall

" supertab
let g:SuperTabDefaultCompletionType = 'context'
autocmd FileType *
			\ if &omnifunc != '' |
			\   call SuperTabChain(&omnifunc, "<c-p>") |
			\ endif
let g:SuperTabCompletionContexts = ['s:ContextText', 's:ContextDiscover']
let g:SuperTabContextDiscoverDiscovery =
			\ ["&completefunc:<c-x><c-u>", "&omnifunc:<c-x><c-o>"]
" let g:SuperTabClosePreviewOnPopupClose=1

" delimitMate
au FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_expand_cr = 1

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_show_hidden = 1

" cosco
autocmd FileType javascript,css,YOUR_LANG nnoremap <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>

" rainbow parentheses
autocmd VimEnter * RainbowParentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

" pipe2eval
let g:pipe2eval_map_key = '<cr>'

" sneak
let g:sneak#streak = 1
