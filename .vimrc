set nocompatible
set hidden
filetype indent plugin on
set autoindent
set number
set relativenumber
set showmode
set tabstop=4
set shiftwidth=4
set wildmenu
" set hlsearch
autocmd BufEnter * silent! lcd %:p:h
set clipboard=unnamed,unnamedplus
set ttimeoutlen=50
set ttyfast
set lazyredraw
set ignorecase
set smartcase
set incsearch
set mouse=a
set t_vb=
" set completeopt-=preview
set splitright
set splitbelow
set visualbell
set confirm
set laststatus=2
set ruler
set nostartofline
set autoindent
set backspace=indent,eol,start
set gcr=a:blinkon0
set synmaxcol=1200
set ffs=unix,dos
set autoread
set fillchars=vert:┃
set completeopt+=menuone
set foldmethod=marker
set foldlevel=0
let g:netrw_localrmdir='rm -rf' " Allow netrw to remove non-empty local directories
runtime macros/matchit.vim
map Q <nop>

if has('win32') || has('win64')
	set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

augroup reload_vimrc " {
	autocmd!
	autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

set statusline=%F
set statusline+=%y
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%{g:NyanModoki()}
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%=      "left/right separator
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file


"------------------------------------------------------------
"------------------------------------------------------------
"
call plug#begin('~/.vim/bundle')

" libraries, &c.
Plug 'xolox/vim-misc'
Plug 'junegunn/vim-pseudocl'
Plug 'kana/vim-textobj-user'
Plug 'xolox/vim-session'
Plug 'moll/vim-bbye'
Plug 'jaxbot/browserlink.vim'
" Plug 'ctrlpvim/ctrlp.vim'
" Plug 'tacahiroy/ctrlp-funky'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Shougo/unite.vim'
Plug 'Shougo/neomru.vim'
Plug 'Shougo/unite-outline'
Plug 'lambdalisue/unite-grep-vcs'
Plug 'kmnk/vim-unite-giti'
Plug 'tpope/vim-vinegar'
Plug 'idbrii/renamer.vim'
Plug 'mopp/autodirmake.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'bling/vim-airline'
" Plug 'mhinz/vim-signify'

" editing features
Plug 'junegunn/vim-oblique'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
Plug 'tommcdo/vim-exchange'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-repeat'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-function'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'sgur/vim-textobj-parameter'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'AndrewRadev/sideways.vim'
Plug 'maxbrunsfeld/vim-yankstack'
" Plug 'zweifisch/pipe2eval'
Plug 'bounceme/pipe2eval'
Plug 'Shougo/vimshell.vim'

" syntax,indent &c.
" Plug 'jelera/vim-javascript-syntax'
" Plug 'othree/yajs.vim'
" Plug 'pangloss/vim-javascript'
Plug 'rschmukler/pangloss-vim-indent'
" Plug 'jason0x43/vim-js-indent'
Plug 'lfilho/cosco.vim'
Plug 'groenewege/vim-less'
" Plug 'othree/javascript-libraries-syntax.vim'
Plug 'thinca/vim-quickrun'
Plug 'hail2u/vim-css3-syntax'
Plug 'scrooloose/syntastic'

" color,appearance
Plug 'ap/vim-css-color'
Plug 'bling/vim-bufferline'
Plug 'freeo/vim-kalisi'
Plug 'flazz/vim-colorschemes'
Plug 'valloric/MatchTagAlways'
" Plug 'nefo-mi/nyan-modoki.vim'
Plug 'bounceme/nyan-modoki.vim'
Plug 'altercation/vim-colors-solarized'
Plug 'romainl/flattened'
Plug 'chriskempson/base16-vim'
Plug 'junegunn/rainbow_parentheses.vim'

" autocompleting
Plug 'ervandew/supertab'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'bonsaiben/bootstrap-snippets'
Plug 'cohama/lexima.vim'

call plug#end()

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_keymaps()
function! s:unite_keymaps()
    let b:SuperTabDisabled=1
	nmap <buffer> <esc> <Plug>(unite_exit)
endfunction
let g:unite_enable_start_insert=1
call unite#filters#matcher_default#use(['matcher_fuzzy', 'matcher_hide_hidden_files'])
call unite#filters#sorter_default#use(['sorter_rank'])
nnoremap <silent> <c-p> :Unite -auto-resize buffer file_mru<cr>
nnoremap <silent> <c-u> :Unite -auto-resize<cr>

syntax enable
set background=dark
colorscheme kalisi
" hi clear SignColumn

if has('nvim')
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
	colorscheme flattened_dark
endif

set undodir=~/.vim/undodir
set undofile

nnoremap <Leader>m :w <BAR> !lessc % --autoprefix="last 2 versions" > %:t:r.css<CR><space>
" npm install -g less
" npm install -g less-plugin-autoprefix

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
vnoremap < <gv
vnoremap > >gv

call yankstack#setup()
map Y y$
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
" prevent yank from moving cursor
xnoremap <silent> y ygv<Esc>
" Adding and deleting empty lines
nnoremap <silent>]x m`:silent +g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent>[x m`:silent -g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent>]<space> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent>[<space> :set paste<CR>m`O<Esc>``:set nopaste<CR>
" Bubbling
nnoremap <silent> [e   :move-2<CR>==
nnoremap <silent> ]e :move+<CR>==
xnoremap <silent> [e   :move-2<CR>gv=gv
xnoremap <silent> ]e :move'>+<CR>gv=gv
" common mistakes
cnoreabbrev E! e!
cnoreabbrev W w
cnoreabbrev Q q
" colon replacement
nnoremap ! :!
nnoremap <Space> :
vnoremap <Space> :
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

" nyan
let g:nyan_modoki_select_cat_face_number = 1
let g:nayn_modoki_animation_enabled= 0

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
let g:syntastic_enable_signs=0

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
let g:SuperTabClosePreviewOnPopupClose=1

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_extensions = ['funky']

" JavaScript syntax
let g:used_javascript_libs = 'jquery'

" cosco
autocmd FileType javascript,css,YOUR_LANG nnoremap <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>

" rainbow parentheses
autocmd VimEnter * RainbowParentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

" pipe2eval
let g:pipe2eval_map_key = '<cr>'

" vimshell
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'
let g:vimshell_prompt =  '$ '

" sideways
nnoremap [s :SidewaysLeft<CR>
nnoremap ]s :SidewaysRight<CR>

" yankstack
let g:yankstack_map_keys = 0
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste

" oblique
" autocmd! User ObliqueStar
" autocmd User ObliqueStar normal n
