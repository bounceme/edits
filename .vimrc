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
let g:netrw_localrmdir='rm -rf' " Allow netrw to remove non-empty local directories
runtime macros/matchit.vim 		"matching tags
map Q <nop>
autocmd FileType css,scss set iskeyword=@,48-57,_,-,?,!,192-255 " Autocomplete ids and classes in CSS
let g:netrw_liststyle=1
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

" fun! PsWrapper(text)
"      let view=winsaveview()
"      exe 'ps' a:text
"      call winrestview(view)
" endfun

" colorscheme xterm16,busierbee/mustang,candyman
"------------------------------------------------------------
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
Plug 'vim-scripts/renamer.vim', { 'on':  'Renamer' }
Plug 'jelera/vim-javascript-syntax'
" Plug 'othree/yajs.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'justinmk/vim-sneak'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'nefo-mi/nyan-modoki.vim'
Plug 'djjcast/mirodark'
Plug 'bounceme/pipe2eval'
" Plug 'zweifisch/pipe2eval'
Plug 'mtglsk/mushroom'
Plug 'jonathanfilip/vim-lucius'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'
Plug 'romainl/flattened'
Plug 'kana/vim-textobj-function'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'ap/vim-css-color' 
Plug 'notpratheek/vim-luna'
Plug 'othree/jspc.vim'
Plug 'pangloss/vim-javascript'

call plug#end()

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
nnoremap <silent>]x m`:silent +g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent>[x m`:silent -g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent>]<space> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent>[<space> :set paste<CR>m`O<Esc>``:set nopaste<CR>
" common mistakes
cnoreabbrev E! e!
cnoreabbrev W w
cnoreabbrev Q q

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
" let g:SuperTabClosePreviewOnPopupClose=1

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_extensions = ['funky']

" delimitMate
au FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_expand_cr = 1
au FileType javascript let b:delimitMate_autoclose = 0

" xml
let g:xmledit_enable_html = 1
let xml_no_comment_map=1

" JavaScript syntax
let g:used_javascript_libs = 'jquery'

" cosco
autocmd FileType javascript,css,YOUR_LANG nnoremap <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>

" easyalign
vmap <Enter> <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" yankstack
let g:yankstack_map_keys = 0
nmap <leader>p <Plug>yankstack_substitute_older_paste

" scratch
nmap <leader>gs <plug>(scratch-insert-reuse)
nmap <leader>gS <plug>(scratch-insert-clear)
xmap <leader>gs <plug>(scratch-selection-reuse)
xmap <leader>gS <plug>(scratch-selection-clear)
