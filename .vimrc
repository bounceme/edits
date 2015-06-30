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
set autochdir
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
set fillchars=vert:┃
set cmdheight=2
set completeopt+=menuone
set foldmethod=marker
set foldlevel=0
let g:netrw_localrmdir='rm -rf' " Allow netrw to remove non-empty local directories
let g:netrw_bufsettings = 'noma nomod nu nobl nowrap ro'
runtime macros/matchit.vim
map Q <nop>
autocmd BufNewFile,BufRead *.txt set spell spelllang=en_gb

if has('win32') || has('win64')
	set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

augroup reload_vimrc " {
	autocmd!
	autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END " }

if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

set statusline=%F
set statusline+=%y
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=\ %P    "percent through file
set statusline+=%=      "left/right separator
set statusline+=%l/%L   "cursor line/total lines
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*


"------------------------------------------------------------
"------------------------------------------------------------
"
call plug#begin('~/.vim/bundle')

" libraries, &c.
Plug 'xolox/vim-misc'
Plug 'junegunn/vim-pseudocl'
Plug 'kana/vim-textobj-user'
Plug 'xolox/vim-session'
" Plug 'mhinz/vim-startify'
Plug 'moll/vim-bbye'
Plug 'jaxbot/browserlink.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'tpope/vim-vinegar'
Plug 'mopp/autodirmake.vim'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
" Plug 'mhinz/vim-signify'

" editing features
Plug 'junegunn/vim-oblique'
Plug 'justinmk/vim-sneak'
" Plug 'tomtom/tcomment_vim'
" Plug 'tyru/caw.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'terryma/vim-multiple-cursors'
Plug 'tommcdo/vim-exchange'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-repeat'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-function'
Plug 'thinca/vim-textobj-function-javascript'
" Plug 'glts/vim-textobj-comment'
Plug 'thinca/vim-textobj-comment'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'AndrewRadev/sideways.vim'
" Plug 'maxbrunsfeld/vim-yankstack'
" Plug 'zweifisch/pipe2eval'
Plug 'bounceme/pipe2eval'

" syntax,indent &c.
" Plug 'pangloss/vim-javascript'
Plug 'rschmukler/pangloss-vim-indent'
" Plug '1995eaton/vim-better-javascript-highlighting'
Plug 'lfilho/cosco.vim'
Plug 'JulesWang/css.vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'genoma/vim-less'
" Plug 'othree/javascript-libraries-syntax.vim'
Plug 'scrooloose/syntastic'

" color,appearance
Plug 'chriskempson/base16-vim'
Plug 'w0ng/vim-hybrid'
Plug 'jasonlollback/vim-tomorrow-theme'
Plug 'romainl/flattened'
" Plug 'flazz/vim-colorschemes'
" Plug 'godlygeek/csapprox'
" Plug 'calebsmith/vim-lambdify'
Plug 'ap/vim-css-color'
Plug 'bling/vim-bufferline'
Plug 'valloric/MatchTagAlways'
Plug 'junegunn/rainbow_parentheses.vim'

" autocompleting
Plug 'ervandew/supertab'
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
" Plug 'mattn/jscomplete-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'bonsaiben/bootstrap-snippets'
Plug 'Raimondi/delimitMate'

call plug#end()
" call yankstack#setup()

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

let g:sneak#streak = 1

syntax enable
set background=dark
let g:zenburn_old_Visual = 1
let g:zenburn_alternate_Visual = 1
let g:seoul256_background = 237
let g:zenburn_high_Contrast=1

colorscheme tomorrow-night
highlight special ctermfg=109
highlight linenr ctermfg=240

" " hybrid
" colorscheme hybrid
" highlight Normal ctermbg=235
" highlight function ctermfg=222
" highlight statusline ctermbg=222 ctermfg=240


" au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif

" let g:tern_show_argument_hints='on_hold'
let g:tern_show_argument_hints = 'on_move'

" let g:jscomplete_use = ['dom']

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

" quickfix
nnoremap ]q :cclose<cr>
nnoremap [q :copen<cr>

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
" let g:SuperTabClosePreviewOnPopupClose=1

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

" sideways
nnoremap [s :SidewaysLeft<CR>
nnoremap ]s :SidewaysRight<CR>
