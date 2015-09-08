set nocompatible
set hidden
filetype indent plugin on
set autoindent
set showmode
set tabstop=4
set shiftwidth=4
set wildmenu
" set wildmode=list:longest,full
" set hlsearch
" set autochdir
autocmd BufEnter * silent! lcd %:p:h
set clipboard=unnamed,unnamedplus
set ttyfast
set lazyredraw
set ignorecase
set smartcase
set incsearch
set mouse=a
set t_vb=
set completeopt-=preview
set splitright
set splitbelow
set visualbell
set confirm
set laststatus=2
set ruler
set nostartofline
set autoindent
set backspace=indent,eol,start
set synmaxcol=1200
set ffs=unix,dos
set ttimeoutlen=50
set autoread
set cmdheight=2
set completeopt+=menuone
set foldmethod=marker
set foldlevel=0
" set virtualedit=block
let g:netrw_localrmdir='rm -rf' " Allow netrw to remove non-empty local directories
runtime macros/matchit.vim
noremap Q gq
autocmd BufNewFile,BufRead *.txt set spell spelllang=en_gb

set number
let g:netrw_bufsettings = 'noma nomod nu nobl nowrap ro'
set relativenumber

if has('win32') || has('win64')
	set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

augroup reload_vimrc " {
	autocmd!
	autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

" Save Position in buffer
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

set statusline=%F
set statusline+=%y
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
" set statusline+=%{fugitive#statusline()}
set statusline+=%l/%L   "cursor line/total lines
set statusline+=%=      "left/right separator
set statusline+=\ %P    "percent through file

"------------------------------------------------------------
"------------------------------------------------------------
"
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.vim/bundle')

" libraries, &c.
Plug 'xolox/vim-misc'
Plug 'junegunn/vim-pseudocl'
Plug 'kana/vim-textobj-user'
Plug 'xolox/vim-session'
Plug 'moll/vim-bbye'
Plug 'tpope/vim-repeat'
" Plug 'jaxbot/browserlink.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'sgur/ctrlp-extensions.vim'
Plug 'tpope/vim-vinegar'
Plug 'mopp/autodirmake.vim'
" Plug 'travisjeffery/vim-auto-mkdir'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-dispatch'
" Plug 'mhinz/vim-signify'

" editing features
Plug 'junegunn/vim-oblique'
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
Plug 'tommcdo/vim-exchange'
Plug 'mattn/emmet-vim'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-function'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'thinca/vim-textobj-comment'
Plug 'Julian/vim-textobj-variable-segment'
Plug 'reedes/vim-textobj-sentence'
Plug 'kana/vim-textobj-fold'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
" Plug 'zweifisch/pipe2eval'
Plug 'bounceme/pipe2eval'
Plug 'idbrii/renamer.vim'

" syntax,indent &c.
" Plug 'pangloss/vim-javascript'
Plug 'rschmukler/pangloss-vim-indent'
Plug 'moll/vim-node'
" Plug '1995eaton/vim-better-javascript-highlighting'
Plug 'lfilho/cosco.vim'
Plug 'JulesWang/css.vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'genoma/vim-less'
Plug 'scrooloose/syntastic'

" color,appearance
Plug 'jasonlollback/vim-tomorrow-theme'
Plug 'romainl/Apprentice'
Plug 'morhetz/gruvbox'
Plug 'flazz/vim-colorschemes'
Plug 'ap/vim-css-color'
Plug 'bling/vim-bufferline'
Plug 'valloric/MatchTagAlways'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'gilgigilgil/anderson.vim'

" autocompleting
Plug 'ervandew/supertab'
Plug 'Raimondi/delimitMate'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
" Plug 'mattn/jscomplete-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'bonsaiben/bootstrap-snippets'

call plug#end()

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif

au FileType vim setl keywordprg=:help
" au FileType javascript setl keywordprg=:terndoc
au FileType javascript nnoremap <silent> <buffer> K <esc>:TernDoc<CR>
" close any preview windows
au FileType javascript nnoremap <silent> <buffer> <esc> <C-W>z

" if executable('pt')
" 	set grepprg=pt\ --nogroup\ --nocolor
" 	let g:ctrlp_user_command = 'pt %s -l --nocolor -g ""'
" 	let g:ctrlp_use_caching = 0
" endif

" set grepprg=ack\ --nogroup\ --nocolor

let g:sneak#streak = 1

" syntax enable
" set background=dark
" let g:gruvbox_invert_selection = 0
" " let g:gruvbox_contrast_dark = 'hard'
" colorscheme gruvbox

colorscheme apprentice
hi Todo cterm=bold,underline ctermbg=234 ctermfg=96
hi link JavascriptNumber Number

" colorscheme tomorrow-night
" highlight special ctermfg=109
" highlight linenr ctermfg=240

" let g:tern_show_argument_hints='on_hold'
let g:tern_show_argument_hints = 'on_move'

" delimitMate
au FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_expand_cr = 1

if has('nvim')
	tnoremap <Esc><Esc> <C-\><C-N>
	let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif

set undodir=~/.vim/undodir
set undofile

nnoremap <Leader>m :w <BAR> !lessc % --autoprefix="last 2 versions" > %:t:r.css<CR><space>
" npm install -g less
" npm install -g less-plugin-autoprefix

inoremap <c-w> <nop>

nnoremap <expr> j v:count ? 'j' : 'gj'
nnoremap <expr> k v:count ? 'k' : 'gk'
vnoremap < <gv
vnoremap > >gv

map Y y$

xnoremap <silent> y ygv<Esc>

" Adding and deleting empty lines
nnoremap <silent>]x m`:silent +g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent>[x m`:silent -g/\m^\s*$/d<CR>``:noh<CR>
nnoremap <silent>]<space> :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent>[<space> :set paste<CR>m`O<Esc>``:set nopaste<CR>

" quickfix,location
nnoremap ]q :cclose<cr>
nnoremap [q :copen<cr>
" nnoremap ]a :lclose<cr>
" nnoremap [a :lopen<cr>

" Bubbling
nnoremap <silent> [e   :move-2<CR>==
nnoremap <silent> ]e :move+<CR>==
xnoremap <silent> [e   :move-2<CR>gv=gv
xnoremap <silent> ]e :move'>+<CR>gv=gv

" common mistakes
cnoreabbrev E! e!
cnoreabbrev W w
cnoreabbrev Q q

"command
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

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_extensions = ['funky']
nnoremap `p :CtrlPYankring<cr>
" let g:ctrlp_show_hidden = 1

" cosco
autocmd FileType javascript,css,YOUR_LANG nnoremap <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>

" rainbow parentheses
autocmd VimEnter * RainbowParentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

" pipe2eval
let g:pipe2eval_map_key = '<cr>'
