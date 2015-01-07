set nocompatible
set hidden
syntax on              	" syntax highlighting
filetype indent plugin on     " activates indenting for files
set autoindent          " auto indenting
set number              " line numbers
set relativenumber
" colorscheme xterm16      " colorscheme
set backspace=2         " backspace in insert mode works like normal editor
set wildmenu		" Better command-line completion
set hlsearch		" Highlight searches (use <C-L> to temporarily turn off highlighting; see the
" mapping of <C-L> below)
set autochdir		"autodirectory
set clipboard=unnamed "clipboard
set ttimeoutlen=50 " delay in ms
set ttyfast " u got a fast terminal
" set ttyscroll=3
set lazyredraw " to avoid scrolling problems
" Allow netrw to remove non-empty local directories
"
let g:netrw_localrmdir='rm -rf'
" set ttymouse=xterm2
"------------------------------------------------------------
" Usability options {{{1
"
" These are options that users frequently set in their .vimrc. Some of them
" change Vim's behaviour in ways which deviate from the true Vi way, but
" which are considered to add usability. Which, if any, of these options to
" use is very much a personal preference, but they are harmless.

" Use case insensitive search, except when using capital letters
set ignorecase
set smartcase

" Allow backspacing over autoindent, line breaks and start of insert action
set backspace=indent,eol,start

" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
set autoindent

" Stop certain movements from always going to the first character of a line.
" While this behaviour deviates from that of Vi, it does what most users
" coming from other editors would expect.
set nostartofline

" Display the cursor position on the last line of the screen or in the status
" line of a window
set ruler

" Always display the status line, even if only one window is displayed
set laststatus=2

" Instead of failing a command because of unsaved changes, instead raise a
" dialogue asking if you wish to save changed files.
set confirm

" Use visual bell instead of beeping when doing something wrong
set visualbell

" And reset the terminal code for the visual bell. If visualbell is set, and
" this line is also included, vim will neither flash nor beep. If visualbell
" is unset, this does nothing.
set t_vb=

" Enable use of the mouse for all modes
set mouse=a

" Set the command window height to 2 lines, to avoid many cases of having to
" "press <Enter> to continue"
set cmdheight=2

" color bolds
" if &t_Co == 8 && $TERM !~# '^linux'
"   set t_Co=16
" endif

"------------------------------------------------------------
" Mappings {{{1
"
" Useful mappings

" Map Y to act like D and C, i.e. to yank until EOL, rather than act as yy,
" which is the default
map Y y$

" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
" nnoremap <C-L> :nohl<CR><C-L>

"Open In chrome
nnoremap <F5> :exe ':silent !open -a /Applications/Google\ Chrome.app %'<CR>
"------------------------------------------------------------
"
"set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'rstacruz/sparkup'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-session'
" Plugin 'scrooloose/nerdtree'
" Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'tpope/vim-surround'
Plugin 'bling/vim-airline'
" Plugin 'takac/vim-spotifysearch'
Plugin 'valloric/MatchTagAlways'
Plugin 'kristijanhusak/vim-multiple-cursors'
" Plugin 'terryma/vim-multiple-cursors'
Plugin 'scrooloose/syntastic'
Plugin 'bling/vim-bufferline'
Plugin 'moll/vim-bbye'
" Plugin 'Valloric/YouCompleteMe'
Plugin 'terryma/vim-expand-region'
Plugin 'Raimondi/delimitMate'
Plugin 'sukima/xmledit'
"Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-repeat'
Plugin 'flazz/vim-colorschemes'
" Plugin 'vim-scripts/xterm16.vim'
Plugin 'vim-scripts/loremipsum'
Plugin 'kien/ctrlp.vim'
Plugin 'othree/vim-autocomplpop'
" Plugin 'othree/xml.vim'
" Plugin 'dirkwallenstein/vim-autocomplpop'
Plugin 'vim-scripts/L9'
Plugin 'ervandew/supertab'
" Plugin 'tpope/vim-commentary'
Plugin 'tomtom/tcomment_vim'
Plugin 'tpope/vim-vinegar'
" Plugin 'fholgado/minibufexpl.vim'
" Plugin 'Shougo/neocomplcache.vim'
" Plugin 'Shougo/neocomplete.vim'







" " All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line




" Nerdtree

" Vim Session
let g:session_autoload="yes"
let g:session_autosave="yes"
let g:session_default_overwrite=1

" Syntastic
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1

" Airline
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#bufferline#enabled = 1
let g:airline_left_sep=''
let g:airline_right_sep=''
" let g:airline_powerline_fonts = 1


" ycm
let g:ycm_seed_identifiers_with_syntax = 1

"supertab
let g:SuperTabDefaultCompletionType = "<c-n>"

"acp
let g:AutoComplPopDontSelectFirst = 1

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'

" delimitMate
au FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"

" xml
let g:xmledit_enable_html = 1
let xml_no_comment_map=1
