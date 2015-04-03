set nocompatible
set hidden
syntax on              	" syntax highlighting!
filetype indent plugin on     " activates indenting for files
set autoindent          " auto indenting
set number              " line numbers
set relativenumber
set showmode
set tabstop=4
set shiftwidth=4
set backspace=2         " backspace in insert mode works like normal editor
set wildmenu		" Better command-line completion
set hlsearch		" Highlight searches (use <C-L> to temporarily turn off highlighting; see the
set autochdir		"autodirectory
set clipboard=unnamed "clipboard
set ttimeoutlen=50 " delay in ms
set ttyfast " u got a fast terminal
set lazyredraw " to avoid scrolling problems
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
set background=dark
set completeopt+=menuone
let g:netrw_localrmdir='rm -rf' " Allow netrw to remove non-empty local directories
runtime macros/matchit.vim 		"matching tags
map Q <nop>
autocmd FileType css,scss setlocal iskeyword=@,48-57,_,-,?,!,192-255 " Autocomplete ids and classes in CSS
if has('win32') || has('win64')
	set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

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
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'tpope/vim-vinegar'
Plug 'idbrii/renamer.vim'
Plug 'tpope/vim-fugitive'

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
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'justinmk/vim-sneak'
Plug 'AndrewRadev/splitjoin.vim'

" syntax,indent &c.
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'lfilho/cosco.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'scrooloose/syntastic'
" Plug 'zweifisch/pipe2eval'
Plug 'bounceme/pipe2eval'
" Plug 'bigfish/vim-js-context-coloring', { 'do': 'npm install' }
" Plug 'othree/yajs.vim'

" color,appearance
Plug 'ap/vim-css-color' 
Plug 'bling/vim-bufferline'
Plug 'notpratheek/vim-luna'
Plug 'djjcast/mirodark'
Plug 'flazz/vim-colorschemes'
Plug 'romainl/flattened'
Plug 'valloric/MatchTagAlways'
Plug 'mtglsk/mushroom'
Plug 'jonathanfilip/vim-lucius'
Plug 'nefo-mi/nyan-modoki.vim'
" Plug 'luochen1990/rainbow'
Plug 'junegunn/rainbow_parentheses.vim'

" autocompleting
Plug 'ervandew/supertab'
" Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
" Plug 'Shougo/neocomplete.vim'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'othree/jspc.vim'
Plug 'Raimondi/delimitMate'
" Plug 'Shougo/echodoc.vim'

call plug#end()

" let g:neocomplete#enable_at_startup = 1
" let g:neocomplete#auto_completion_start_length = 1
" let g:neocomplete#sources#buffer#cache_limit_size = 50000
" let g:neocomplete#data_directory = $HOME.'/.vim/cache/noecompl'
" let g:neocomplete#enable_smart_case = 1
" let g:neocomplete#sources#syntax#min_keyword_length = 2
" if !exists('g:neocomplete#force_omni_input_patterns')
"   let g:neocomplete#force_omni_input_patterns = {}
" endif
" let g:neocomplete#force_omni_input_patterns.javascript = '[^. \t]\.\w*'
" set cmdheight=2
" 	let g:echodoc_enable_at_startup = 1

" Useful mappings
" Map Y to act like D and C, i.e. to yank until EOL, rather than act as yy,
" which is the default
call yankstack#setup()
map Y y$
" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-L> :nohl<CR><C-L>
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
let g:nyan_modoki_select_cat_face_number = 2
let g:nayn_modoki_animation_enabled= 1

" sneak
"replace 'f' with 1-char Sneak
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F
"replace 't' with 1-char Sneak
nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T
xmap t <Plug>Sneak_t
xmap T <Plug>Sneak_T
omap t <Plug>Sneak_t
omap T <Plug>Sneak_T

" Vim Session
let g:session_autoload="yes"
let g:session_autosave="yes"

" Syntastic
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=0

" emmet
let g:user_emmet_install_global = 0
autocmd FileType html,css,php EmmetInstall

" supertab
let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabContextDefaultCompletionType = '<c-x><c-u>'
autocmd FileType *
    \ if &omnifunc != '' |
    \     call SuperTabChain(&omnifunc, '<c-p>') |
    \ endif
let g:SuperTabClosePreviewOnPopupClose=1

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_extensions = ['funky']

" delimitMate
au FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_expand_cr = 1
" au FileType javascript let b:delimitMate_autoclose = 0

" JavaScript syntax
let g:used_javascript_libs = 'jquery'

" cosco
autocmd FileType javascript,css,YOUR_LANG nnoremap <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>

" yankstack
let g:yankstack_map_keys = 0
nmap <leader>p <Plug>yankstack_substitute_older_paste

" rainbow parentheses
autocmd VimEnter * RainbowParentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]
