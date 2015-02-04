set nocompatible
set hidden
syntax on              	" syntax highlighting!
filetype indent plugin on     " activates indenting for files
set autoindent          " auto indenting
set number              " line numbers
set relativenumber
" colorscheme xterm16,busierbee/mustang,candyman
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
let g:netrw_localrmdir='rm -rf'
runtime macros/matchit.vim
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
set tabstop=4
set shiftwidth=4

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
" set cmdheight=2

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
" hi MatchParen cterm=bold ctermbg=none ctermfg=none

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
Plugin 'tpope/vim-surround'
" Plugin 'bling/vim-airline'
Plugin 'itchyny/lightline.vim'
Plugin 'valloric/MatchTagAlways'
Plugin 'kristijanhusak/vim-multiple-cursors'
Plugin 'scrooloose/syntastic'
Plugin 'bling/vim-bufferline'
Plugin 'moll/vim-bbye'
Plugin 'terryma/vim-expand-region'
Plugin 'Raimondi/delimitMate'
Plugin 'sukima/xmledit'
Plugin 'lfilho/cosco.vim'
Plugin 'tpope/vim-repeat'
Plugin 'flazz/vim-colorschemes'
Plugin 'vim-scripts/loremipsum'
Plugin 'kien/ctrlp.vim'
" Plugin 'othree/xml.vim'
Plugin 'vim-scripts/L9'
Plugin 'ervandew/supertab'
Plugin 'othree/vim-autocomplpop'
" Plugin 'Valloric/YouCompleteMe'
Plugin 'marijnh/tern_for_vim'
Plugin 'tomtom/tcomment_vim'
" Plugin 'scrooloose/nerdcommenter'
" Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-vinegar'
" Plugin 'drmingdrmer/xptemplate'
" Plugin 'fholgado/minibufexpl.vim'
" Plugin 'nemtsov/JavaScript-Indent'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'othree/javascript-libraries-syntax.vim'


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

" Vim Session
let g:session_autoload="yes"
let g:session_autosave="yes"
" let g:session_default_overwrite=1

" Syntastic
let g:syntastic_check_on_open=1
let g:syntastic_enable_signs=1

" Airline
let g:airline_left_sep=''
let g:airline_right_sep=''
" let g:airline_powerline_fonts = 1

" sparkup
" let g:sparkupExecuteMapping = '<tab>'

" ycm
let g:ycm_seed_identifiers_with_syntax = 1

"supertab
let g:SuperTabDefaultCompletionType = "<c-n>"
let b:SuperTabDisabled = 1
" let g:SuperTabClosePreviewOnPopupClose = 1
" let g:SuperTabDefaultCompletionType = 'context'

" xptemplate
let g:xptemplate_brace_complete = ''
let g:xptemplate_nav_next = '<C-n>'
let g:xptemplate_nav_prev = '<C-p>'

"acp
let g:AutoComplPopDontSelectFirst = 1

" MatchTagAlways
" let g:mta_use_matchparen_group = 0
" let g:mta_set_default_matchtag_color = 0
" hi MatchTag cterm=bold ctermbg=none ctermfg=none

" ctrp
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'c'

" delimitMate
au FileType vim,html,php let b:delimitMate_matchpairs = "(:),[:],{:}"
let delimitMate_expand_cr = 1
" let delimitMate_expand_space = 1

" xml
let g:xmledit_enable_html = 1
let xml_no_comment_map=1

" JavaScript syntax
let g:used_javascript_libs = 'jquery'

" cosco
autocmd FileType javascript,css,YOUR_LANG nnoremap <silent> <Leader>; :call cosco#commaOrSemiColon()<CR>
" autocmd FileType javascript,css,YOUR_LANG inoremap <silent> <Leader>; <c-o>:call cosco#commaOrSemiColon()<CR>

" lightline (from https://github.com/timss/vimconf/blob/master/.vimrc)
    """ Lightline {{{
        let g:lightline = {
            \ 'colorscheme': 'jellybeans',
            \ 'active': {
            \     'left': [
            \         ['mode', 'paste'],
            \         ['ctrlpmark', 'readonly', 'fugitive'],
            \         ['bufferline']
            \     ],
            \     'right': [
            \         ['lineinfo'],
            \         ['percent'],
            \         ['fileformat', 'fileencoding', 'filetype', 'syntastic']
            \     ]
            \ },
            \ 'component': {
            \     'paste': '%{&paste?"!":""}'
            \ },
            \ 'component_function': {
            \     'mode'         : 'MyMode',
            \     'fugitive'     : 'MyFugitive',
            \     'readonly'     : 'MyReadonly',
            \     'ctrlpmark'    : 'CtrlPMark',
            \     'bufferline'   : 'MyBufferline',
            \     'fileformat'   : 'MyFileformat',
            \     'fileencoding' : 'MyFileencoding',
            \     'filetype'     : 'MyFiletype'
            \ },
            \ 'component_expand': {
            \     'syntastic': 'SyntasticStatuslineFlag',
            \ },
            \ 'component_type': {
            \     'syntastic': 'error',
            \ },
            \ 'subseparator': {
            \     'left': '|', 'right': '|'
            \ }
            \ }

        " let g:lightline.mode_map = {
        "     \ 'n'      : ' N ',
        "     \ 'i'      : ' I ',
        "     \ 'R'      : ' R ',
        "     \ 'v'      : ' V ',
        "     \ 'V'      : 'V-L',
        "     \ 'c'      : ' C ',
        "     \ "\<C-v>" : 'V-B',
        "     \ 's'      : ' S ',
        "     \ 'S'      : 'S-L',
        "     \ "\<C-s>" : 'S-B',
        "     \ '?'      : '      ' }

        function! MyMode()
            let fname = expand('%:t')
            return fname == '__Tagbar__' ? 'Tagbar' :
                    \ fname == 'ControlP' ? 'CtrlP' :
                    \ winwidth('.') > 60 ? lightline#mode() : ''
        endfunction

        function! MyFugitive()
            try
                if expand('%:t') !~? 'Tagbar' && exists('*fugitive#head')
                    let mark = '± '
                    let _ = fugitive#head()
                    return strlen(_) ? mark._ : ''
                endif
            catch
            endtry
            return ''
        endfunction

        function! MyReadonly()
            return &ft !~? 'help' && &readonly ? '≠' : '' " or ⭤
        endfunction

        function! CtrlPMark()
            if expand('%:t') =~ 'ControlP'
                call lightline#link('iR'[g:lightline.ctrlp_regex])
                return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
                    \ , g:lightline.ctrlp_next], 0)
            else
                return ''
            endif
        endfunction

        function! MyBufferline()
            call bufferline#refresh_status()
            let b = g:bufferline_status_info.before
            let c = g:bufferline_status_info.current
            let a = g:bufferline_status_info.after
            let alen = strlen(a)
            let blen = strlen(b)
            let clen = strlen(c)
            let w = winwidth(0) * 4 / 11
            if w < alen+blen+clen
                let whalf = (w - strlen(c)) / 2
                let aa = alen > whalf && blen > whalf ? a[:whalf] : alen + blen < w - clen || alen < whalf ? a : a[:(w - clen - blen)]
                let bb = alen > whalf && blen > whalf ? b[-(whalf):] : alen + blen < w - clen || blen < whalf ? b : b[-(w - clen - alen):]
                return (strlen(bb) < strlen(b) ? '...' : '') . bb . c . aa . (strlen(aa) < strlen(a) ? '...' : '')
            else
                return b . c . a
            endif
        endfunction

        function! MyFileformat()
            return winwidth('.') > 90 ? &fileformat : ''
        endfunction

        function! MyFileencoding()
            return winwidth('.') > 80 ? (strlen(&fenc) ? &fenc : &enc) : ''
        endfunction

        function! MyFiletype()
            return winwidth('.') > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
        endfunction

        let g:ctrlp_status_func = {
            \ 'main': 'CtrlPStatusFunc_1',
            \ 'prog': 'CtrlPStatusFunc_2',
            \ }

        function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
            let g:lightline.ctrlp_regex = a:regex
            let g:lightline.ctrlp_prev = a:prev
            let g:lightline.ctrlp_item = a:item
            let g:lightline.ctrlp_next = a:next
            return lightline#statusline(0)
        endfunction

        function! CtrlPStatusFunc_2(str)
            return lightline#statusline(0)
        endfunction

        let g:tagbar_status_func = 'TagbarStatusFunc'

        function! TagbarStatusFunc(current, sort, fname, ...) abort
            let g:lightline.fname = a:fname
            return lightline#statusline(0)
        endfunction

        function! s:syntastic()
            SyntasticCheck
            call lightline#update()
        endfunction

        augroup AutoSyntastic
            autocmd!
            autocmd BufWritePost *.c,*.cpp,*.perl,*.py,*.js,*.css call s:syntastic()
        augroup END
    """ }}}
