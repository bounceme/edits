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
set mouse=a
set t_vb=
set completeopt-=preview
set visualbell
set confirm
set laststatus=2
set ruler
set nostartofline
set autoindent
set backspace=indent,eol,start
set gcr=a:blinkon0
set synmaxcol=1200
let g:netrw_localrmdir='rm -rf' " Allow netrw to remove non-empty local directories
runtime macros/matchit.vim 		"matching tags
map Q <nop>
autocmd FileType css,scss set iskeyword=@,48-57,_,-,?,!,192-255 " Autocomplete ids and classes in CSS

if has('win32') || has('win64')
	set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif


set statusline=%F
" set statusline+=[%{&ff}]
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

" set ttymouse=xterm2
" colorscheme xterm16,busierbee/mustang,candyman
"------------------------------------------------------------
" Mappings {{{1
"
" Useful mappings

" Map Y to act like D and C, i.e. to yank until EOL, rather than act as yy,
" which is the default
map Y y$

" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-L> :nohl<CR><C-L>

" using tab for autocomplete
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<C-g>u\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<c-p>" : "\<c-g>u\<tab>"

"------------------------------------------------------------
"
call plug#begin('~/.vim/bundle')

Plug 'mattn/emmet-vim'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'
Plug 'tpope/vim-surround'
Plug 'valloric/MatchTagAlways'
Plug 'kristijanhusak/vim-multiple-cursors'
Plug 'scrooloose/syntastic'
Plug 'bling/vim-bufferline'
Plug 'moll/vim-bbye'
Plug 'Raimondi/delimitMate'
Plug 'sukima/xmledit'
Plug 'lfilho/cosco.vim'
Plug 'tommcdo/vim-exchange'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-repeat'
Plug 'flazz/vim-colorschemes'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'ervandew/supertab'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-vinegar'
Plug 'jelera/vim-javascript-syntax'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'justinmk/vim-sneak'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'nefo-mi/nyan-modoki.vim'
Plug 'djjcast/mirodark'
Plug 'oplatek/Conque-Shell'
Plug 'tarruda/vim-conque-repl'
Plug 'mtth/scratch.vim'
Plug 'mtglsk/mushroom'
Plug 'jonathanfilip/vim-lucius'


call plug#end()

" nyan
let g:nyan_modoki_select_cat_face_number = 2
let g:nayn_modoki_animation_enabled= 1

" sneak
nmap f <Plug>Sneak_s
nmap F <Plug>Sneak_S
xmap f <Plug>Sneak_s
xmap F <Plug>Sneak_S
omap f <Plug>Sneak_s
omap F <Plug>Sneak_S

" Vim Session
let g:session_autoload="yes"
let g:session_autosave="yes"
" let g:session_default_overwrite=1

" Syntastic
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1

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

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_extensions = ['funky']

" delimitMate
au FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_expand_cr = 1

" xml
let g:xmledit_enable_html = 1
let xml_no_comment_map=1

" JavaScript syntax
let g:used_javascript_libs = 'jquery'

" cosco
autocmd FileType javascript,css,YOUR_LANG nnoremap <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>
" autocmd FileType javascript,css,YOUR_LANG inoremap <silent> <Leader>; <c-o>:call cosco#commaOrSemiColon()<CR>

" easyalign
vmap <Enter> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" yankstack
let g:yankstack_map_keys = 0
nmap <leader>p <Plug>yankstack_substitute_older_paste
