filetype indent plugin on
set hidden
set showmode
set autoindent
set wildmenu
set hlsearch
" set clipboard=unnamed,unnamedplus
set ttyfast
set lazyredraw
set ignorecase
set smartcase
set incsearch
set mouse=a
set vb t_vb=
set confirm
set laststatus=2
set autoindent
set backspace=indent,eol,start
set synmaxcol=300
set ffs=unix,dos
set ttimeoutlen=50
set autoread
set completeopt=menu,menuone
set complete-=i
set shortmess+=I
" set number
" set relativenumber
let g:netrw_bufsettings = 'noma nomod nu nobl nowrap ro'
let g:netrw_localrmdir='rm -rf'
runtime macros/matchit.vim

set wildignore+=*.swp,*.bak
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=*/min/*,*/vendor/*,*/node_modules/*,*/bower_components/*
set wildignorecase

augroup vimrc
	autocmd!
augroup END

autocmd vimrc FileType vim setl keywordprg=:help
autocmd vimrc bufwritepost .vimrc source $MYVIMRC

autocmd vimrc BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

set smarttab shiftround tabstop=4 shiftwidth=4 noexpandtab
autocmd vimrc Filetype javascript setlocal tabstop=2 shiftwidth=2 expandtab iskeyword+=$
autocmd vimrc FileType gitcommit setl tw=72 fo+=a spell

set undofile
set undodir=~/.vim/tmp/undo//
set backupdir=~/.vim/tmp/backup//
set directory=~/.vim/tmp/swap//
if !isdirectory(expand(&undodir))
	call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
	call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
	call mkdir(expand(&directory), "p")
endif

set statusline=%<%F\ %h%m%r%{fugitive#statusline()}%=%(%l/%L%)

nnoremap Q <Nop>

nnoremap <Leader>cd :cd %:p:h<cr>

nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'\|diffupdate':''<CR><CR><C-L>

map Y y$
xnoremap <silent> y ygv<Esc>

" common mistakes
cnoreabbrev E! e!
cnoreabbrev W w
cnoreabbrev Q q

" command
nnoremap ! :!
nnoremap <space> :
xnoremap <space> :

" window movement
nnoremap zj <C-W><C-J>
nnoremap zk <C-W><C-K>
nnoremap zl <C-W><C-L>
nnoremap zh <C-W><C-H>

autocmd vimrc BufRead,BufNewFile *.less set ft=less
autocmd vimrc FileType less nnoremap <buffer> <Leader>m :w \| !lessc % --autoprefix="last 2 versions" > %:t:r.css<CR>
" npm install -g less
" npm install -g less-plugin-autoprefix

if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor
endif

if has('nvim')
	tnoremap <Esc><Esc> <C-\><C-N>
	" let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd vimrc VimEnter * PlugInstall | source $MYVIMRC
endif
call plug#begin('~/.vim/bundle')

" libraries, &c.
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'kana/vim-textobj-user'
Plug 'ciaranm/detectindent'
Plug 'baverman/vial-http'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'moll/vim-bbye'

" editing features
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-function'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'tommcdo/vim-exchange'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'pgdouyon/vim-evanesco'
" Plug 'justinmk/vim-dirvish'
" let g:dirvish_hijack_netrw = 1
" let g:dirvish_relative_paths = 1

" syntax,indent &c.
Plug 'CandySunPlus/simple-javascript-indenter'
" Plug 'pangloss/vim-javascript'
" Plug 'pangloss/vim-javascript', { 'branch': 'develop' }
Plug 'moll/vim-node'
Plug 'benekastah/neomake'
if has('nvim')
	Plug 'kassio/neoterm'
	nnoremap <silent> z: :call neoterm#do('.exit')\|call neoterm#do('node')<cr>
	nnoremap <silent> z; :TREPLSend<cr>
	xnoremap <silent> z; :TREPLSend<cr>
endif

" color,appearance
Plug 'ap/vim-css-color'
Plug 'valloric/MatchTagAlways'
Plug 'fxn/vim-monochrome'

" autocompleting
Plug 'ervandew/supertab'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'Raimondi/delimitMate'
Plug 'mattn/emmet-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'bonsaiben/bootstrap-snippets'

call plug#end()

" colo desert
" hi link netrwMarkFile CursorLine
" hi! link Visual CursorColumn

colo monochrome

hi SneakPluginTarget           ctermfg=15    ctermbg=239
hi SneakStreakMask             ctermfg=239    ctermbg=239
hi SneakStreakTarget           ctermfg=15    ctermbg=239

autocmd vimrc BufWritePost * Neomake

" JsIndent
" let g:SimpleJsIndenter_DisableAssignment = 1
let g:SimpleJsIndenter_BriefMode = 1

" Tern
let g:tern_show_signature_in_pum = 1
autocmd vimrc FileType javascript nnoremap <silent> <buffer> K :TernDoc<CR>
autocmd vimrc FileType javascript nnoremap <silent> <buffer> <c-]> :TernDef<CR>
autocmd vimrc FileType javascript nnoremap <silent> <buffer> [D :TernRefs<CR>

" ctrlp
let g:ctrlp_mruf_exclude = '\/var\/folders\|\/.git\/'
let g:ctrlp_working_path_mode = 'w'
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_show_hidden = 0

" ultisnips
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="jj"
let g:UltiSnipsJumpBackwardTrigger="kk"

" supertab
let g:SuperTabDefaultCompletionType = 'context'
autocmd vimrc FileType *
			\ if &omnifunc != '' |
			\   call SuperTabChain(&omnifunc, "<c-p>") |
			\ endif
let g:SuperTabCompletionContexts = ['s:ContextText', 's:ContextDiscover']
let g:SuperTabContextDiscoverDiscovery =
			\ ["&completefunc:<c-x><c-u>", "&omnifunc:<c-x><c-o>"]

" delimitMate
autocmd vimrc FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_expand_cr = 1

" sneak
let g:sneak#streak = 1
xmap z <Plug>Sneak_s
