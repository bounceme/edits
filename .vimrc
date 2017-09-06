filetype indent plugin on
set nohidden
set showmode
set autoindent
set wildmenu
set wildmode=longest:full,full
set hlsearch
set ttyfast
set lazyredraw
set display=
set ignorecase
set smartcase
set incsearch
set mouse=a
set vb t_vb=
set confirm
set laststatus=2
set autoindent
set backspace=indent,eol,start
set synmaxcol=500
set ffs=unix,dos
set guioptions+=c
set ttimeoutlen=50
set completeopt=menu,menuone,noselect
set complete-=i
set complete-=u
set shortmess+=IcT
set history=10000
set autoread
set foldopen-=block
set foldopen+=search
set nojs
set fo+=j fo-=o fo-=t fo-=r fo+=c
set guicursor=

if !has('nvim')
  set directory-=.
endif
set undofile

set grepprg=grep\ -rnH\ --exclude='.*.swp'\ --exclude='*~'\ --exclude=tags

let g:loaded_vimballPlugin = 1
let g:loaded_getscriptPlugin = 1
" let g:loaded_netrwPlugin = 1
" let g:loaded_netrw = 1
runtime macros/matchit.vim
let g:netrw_use_errorwindow=0
let g:netrw_dirhistmax=0
let g:netrw_altfile=1
let g:netrw_banner = 0

augroup vimrc
  au!
augroup END


if &viminfo isnot ''
  set viminfo^=%25
end

set wildignore+=*.swp,*.bak,*.un~
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=*/min/*,*/vendor/*,*/bower_components/*
silent! set wildignorecase

au vimrc FileType * setl fo<

au vimrc bufwritepost $MYVIMRC source $MYVIMRC

au vimrc FileType netrw nmap <buffer> g? <f1>
au vimrc FileType netrw nnoremap <nowait><buffer> q :bd<cr>
au vimrc FileType help nnoremap <buffer> <cr> <C-]>

au vimrc BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
      \ | wincmd p | diffthis

au vimrc FileType gitcommit setl tw=72 fo+=a spell

set statusline=%<%F\ %h%m%r%Y\ %{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

map Q %

nnoremap c* *``cgn
nnoremap c# #``cgN

nnoremap <LEADER>cd :cd %:p:h<cr>
nnoremap <lEADER>cp :let @* = expand("%:p")<cr>
nnoremap <lEADER>v :e $MYVIMRC<cr>

nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'\|diffupdate':''<CR><CR><C-L>

nnoremap gV `[v`]

nnoremap Y y$
xnoremap <silent> y ygv<Esc>
inoremap <C-u> <C-g>u<C-u>

" line text-objects
xnoremap <silent> il g_o^
omap <silent> il :<C-u>normal vil<CR>
xnoremap <silent> al $o0
omap <silent> al :<C-u>normal val<CR>

" buffer text-object
xnoremap <silent> ae GoggV
onoremap <silent> ae :<C-U>execute "normal! m`"<Bar>keepjumps normal! ggVG<CR>

" command
noremap <space> :

nnoremap <expr><C-P> ":Find . -iname '*'<left>"
nnoremap zo :ls<CR>:b

" window movement
nnoremap zj <C-W><C-J>
nnoremap zk <C-W><C-K>
nnoremap zl <C-W><C-L>
nnoremap zh <C-W><C-H>
nnoremap <silent> z' :only<C-R>=has('diff')?'\|diffoff':''<CR><CR>

au vimrc WinEnter * if winnr('$') == 1 && getbufvar(winbufnr(winnr()), "&buftype") =~? 'quickfix\|loclist'|q|endif

fun! s:VSetSearch()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = temp
endfun
xnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR>
xnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR>

" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
let g:plug_shallow=0
try
call plug#begin('~/.vim/bundle')

" libraries, &c.
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-tbone'
Plug 'airblade/vim-rooter'

" editing features
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-rsi'
Plug 'tommcdo/vim-exchange'

" syntax,indent &c.
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'neomake/neomake'
Plug 'justinmk/molokai'
Plug 'romainl/vim-cool'
Plug 'bounceme/rjsx-compile'
Plug 'bounceme/dim-jump'
Plug 'bounceme/poppy.vim'
Plug 'bounceme/restclient.vim'
Plug 'bounceme/fairedit.vim'
Plug 'bounceme/extendCR.vim'

" autocompletion
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'mattn/emmet-vim'
Plug 'ervandew/supertab'
Plug 'justinmk/vim-dirvish'

call plug#end()
catch
  echo 'NO PLUGINS'
endtry
let g:jsx_check_react_import = 1

let g:surround_indent = 0
let g:poppy_point_enable = 1
let g:no_extend_comment_CR = &fo !~# 'r'

silent! set inccommand=nosplit

" " micro language: assignment expression
" function! s:Le(...)
"   silent! redir END
"   call extend(a:,filter(copy(get(a:000,2,{})),'v:key !~ "^\\d"'))
"   call extend(l:,a:1)
"   let [lhs, rhs] = matchlist(a:2,'\C^\s*\(\k\+\)\s*=\s*\(.*\)$')[1:2]
"   let res = eval(rhs)
"   let res = type(res) == type('') ? string(res) : res
"   redir => a:1[lhs]
"   silent echon res
"   redir END
"   let a:1[lhs] = eval(a:1[lhs])
"   return a:1[lhs]
" endfunction
" call s:Le(s:,'from=s:Le(s:,"g={}")')
" echom string(s:)
" unlet! s:from s:g | delfunc s:Le

imap <CR> <PLUG>extendCR

if exists('g:plugs["fairedit.vim"]')
  nmap C <Plug>Fair_M_C
  nmap D <Plug>Fair_M_D
  omap $ <Plug>Fair_M_dollar
  if maparg('Y','n') ==# 'y$'
    nunmap Y
    nmap Y <Plug>Fair_M_yEOL
  endif
endif

command! MakeTags silent! exe '!find . -iname "*.%:e" | xargs ctags' | redraw!

function! s:InitJBuf()
  noremap <buffer> Z! :w !node -p<cr>
  setl path=.,node_modules,,
  if exists('g:plugs["tern_for_vim"]') && empty(tagfiles())
    nnoremap <silent> <buffer> K :TernDoc<CR>
    nnoremap <silent> <buffer> <c-]> :TernDef<CR>
    nnoremap <silent> <buffer> [D :TernRefs<CR>
  endif
endfunction
au vimrc filetype javascript call <SID>InitJBuf()

silent! if neomake#has_async_support()
au vimrc bufwritepost * Neomake
else
  au vimrc filetype javascript au vimrc bufwritepost <buffer> silent! Neomake
endif

augroup Poppy
  au!
augroup END
nnoremap <silent> <leader>hp :call clearmatches() \| let g:poppy = -get(g:,'poppy',-1) \|
      \ exe 'au! Poppy CursorMoved *' . (g:poppy > 0 ? ' call PoppyInit()' : '') <cr>

au vimrc cmdWinEnter [:>] syntax sync maxlines=1 minlines=1

au vimrc filetype text
      \   ino <buffer> . .<c-g>u
      \ | ino <buffer> ! !<c-g>u
      \ | ino <buffer> ? ?<c-g>u
      \ | ino <buffer> , ,<c-g>u
      \ | ino <buffer> ; ;<c-g>u
      \ | ino <buffer> : :<c-g>u

let g:rooter_use_lcd = 1
let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', '.git/', '_darcs/', '.hg/', '.bzr/', '.svn/', 'package.json']

if !exists('g:colors_name')
  exe 'colo ' . ((strftime('%H') % 20) > 7 ? 'default' : 'molokai')
endif

let g:user_emmet_settings = {
      \    'javascript' : {
      \        'snippets' : {
      \            'f' : "function() {\n\t|${child}\n}",
      \            'fun' : "function |() {\n\t${child}\n}",
      \            'for' : "for (var i = 0; i < |; i++){\n\t${child}\n}",
      \            'forr' : "for (var i = | - 1; i >= 0; i--){\n\t${child}\n}",
      \            'iife' : "(function() {\n\t|${child}\n}());",
      \            'tm' : "setTimeout(function() {\n\t|${child}\n}, 100)",
      \            'cl' : "console.log(|${child})",
      \        },
      \    },
      \}

let g:SuperTabDefaultCompletionType = "context"
let g:tern_show_signature_in_pum = 1
