filetype indent plugin on
set hidden
set showmode
set autoindent
set wildmenu
set hlsearch
set ttyfast
set lazyredraw
set ignorecase
set smartcase
set incsearch
set mouse=a
set vb t_vb=
set confirm
set laststatus=2
set numberwidth=3
set autoindent
set backspace=indent,eol,start
set synmaxcol=300
set ffs=unix,dos
set ttimeoutlen=50
set autoread
set completeopt=menu,menuone
set complete-=i
set shortmess+=Ic
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
autocmd vimrc FileType netrw nnoremap <buffer> g? :h netrw-quickhelp<cr>
autocmd vimrc FileType netrw nnoremap <nowait><buffer> q :bd<cr>

autocmd vimrc BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
			\ | wincmd p | diffthis

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

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

nnoremap Q %

nnoremap <Leader>cd :cd %:p:h<cr>

nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'\|diffupdate':''<CR><CR><C-L>

map Y y$
xnoremap <silent> y ygv<Esc>

" commonest mistake
cnoreabbrev E! e!

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
	tnoremap <Esc> <C-\><C-N>
endif

if empty(glob('~/.vim/autoload/plug.vim'))
	silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd vimrc VimEnter * PlugInstall | source $MYVIMRC
endif
call plug#begin('~/.vim/bundle')

" libraries, &c.
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'kana/vim-textobj-user'
Plug 'ctrlpvim/ctrlp.vim'

" editing features
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'
Plug 'kana/vim-textobj-function'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'tommcdo/vim-exchange'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'pgdouyon/vim-evanesco'
Plug 'bounceme/pipe2eval'
Plug 'vim-utils/vim-husk'

" syntax,indent &c.
Plug 'CandySunPlus/simple-javascript-indenter'
Plug 'marcelbeumer/javascript-syntax.vim'
Plug 'moll/vim-node'
Plug 'benekastah/neomake'

" color,appearance
Plug 'sjl/badwolf'
Plug 'ap/vim-css-color'
Plug 'valloric/MatchTagAlways'

" autocompleting
Plug 'ervandew/supertab'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'Raimondi/delimitMate'
Plug 'mattn/emmet-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'bonsaiben/bootstrap-snippets'

call plug#end()

colo badwolf

autocmd vimrc BufWritePost * Neomake

" JsIndent
let g:SimpleJsIndenter_DisableAssignment = 1
let g:SimpleJsIndenter_CaseIndentLevel = -1
let g:SimpleJsIndenter_BriefMode = 1

" Tern
let g:tern_show_signature_in_pum = 1
autocmd vimrc FileType javascript nnoremap <silent> <buffer> K :TernDoc<CR>
autocmd vimrc FileType javascript nnoremap <silent> <buffer> <c-]> :TernDef<CR>
autocmd vimrc FileType javascript nnoremap <silent> <buffer> [D :TernRefs<CR>

" ctrlp
let g:ctrlp_mruf_exclude = '\/var\/folders\|\/.git\/\|\/$'
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

" pipe2eval
let g:pipe2eval_map_key = '<cr>'

" sneak
let g:sneak#streak = 1
xmap z <Plug>Sneak_s
