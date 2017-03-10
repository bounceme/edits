filetype indent plugin on
set nohidden
set showmode
set autoindent
set wildmenu
set wildmode=longest:full,full
set wildignorecase
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
set completeopt=menu,menuone
set complete-=i
set complete-=u
set shortmess+=IcT
set history=10000
set autoread
set foldopen-=block
set foldopen+=search
set nojs
set fo+=j fo-=o fo-=t fo-=r fo+=c

set grepprg=grep\ -rnH\ --exclude='.*.swp'\ --exclude='*~'\ --exclude=tags

runtime macros/matchit.vim
let g:netrw_use_errorwindow=0
let g:netrw_dirhistmax=0
let g:netrw_altfile=1
let g:netrw_banner = 0

augroup vimrc
  au!
augroup END

if argc()
  au vimrc VimLeavePre * set viminfo=
end
set viminfo+=%25

set wildignore+=*.swp,*.bak,*.un~
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=*/min/*,*/vendor/*,*/bower_components/*
set wildignorecase

au vimrc FileType * setl fo<

au vimrc bufwritepost $MYVIMRC silent! source $MYVIMRC

au vimrc FileType netrw nmap <buffer> g? <f1>
au vimrc FileType netrw nnoremap <nowait><buffer> q :bd<cr>
au vimrc FileType help nnoremap <buffer> <cr> <C-]>

au vimrc BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

command! DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
      \ | wincmd p | diffthis

au vimrc FileType gitcommit setl tw=72 fo+=a spell

set directory-=.
set undofile

set statusline=%<%F\ %h%m%r%Y\ %{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

nmap Q %
omap Q %
xmap Q %

nnoremap c* *``cgn
nnoremap c# #``cgN

nnoremap <LEADER>cd :cd %:p:h<cr>
nnoremap <lEADER>cp :let @* = expand("%")"<cr>
nnoremap <lEADER>v :e $MYVIMRC<cr>

nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'\|diffupdate':''<CR><CR><C-L>

nnoremap gV `[v`]

map Y y$
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
nnoremap <space> :
xnoremap <space> :

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

fun! s:MyCR()
  if getline('.')[col('.')-2] == '{' && col('.') == col('$') &&
        \ synIDattr(synID(line('.'),col('.') - 1,0),'name') !~? 'string\|regex\|comment'
    return "\<CR>}\<C-o>O"
  end
  return "\<CR>"
endfun
inoremap <expr> <CR> <SID>MyCR()

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
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-tbone'
Plug 'airblade/vim-rooter'

" editing features
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-rsi'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'tommcdo/vim-exchange'
Plug 'coderifous/textobj-word-column.vim'

" syntax,indent &c.
Plug 'mxw/vim-jsx'
Plug 'pangloss/vim-javascript'
Plug 'neomake/neomake'
Plug 'justinmk/molokai'
Plug 'bounceme/rjsx-compile'
Plug 'bounceme/vim-cool'
Plug 'bounceme/poppy.vim'

" autocompletion
Plug 'marijnh/tern_for_vim', { 'do': 'npm install' }
Plug 'mattn/emmet-vim'
Plug 'ervandew/supertab'

call plug#end()
catch
  echom 'NO PLUGINS'
endtry

command! Jtags silent! exe '!find . -iname "*.js" | xargs ctags' | redraw!

au vimrc filetype javascript map Z! :w !node -p<cr>
au vimrc bufwritepost *.jsx silent! Neomake
au vimrc bufwritepost *.js silent! Neomake

augroup Poppy
  au!
augroup END
nnoremap <silent> <leader>hp :call clearmatches() \| let g:poppy = -get(g:,'poppy',-1) \|
      \ exe 'au! Poppy CursorMoved *' . (g:poppy > 0 ? ' call PoppyInit()' : '') <cr>

au vimrc cmdWinEnter [:>] syntax clear

au vimrc filetype text
      \   ino <buffer> . .<c-g>u
      \ | ino <buffer> ! !<c-g>u
      \ | ino <buffer> ? ?<c-g>u
      \ | ino <buffer> , ,<c-g>u
      \ | ino <buffer> ; ;<c-g>u
      \ | ino <buffer> : :<c-g>u

" let g:javascript_plugin_flow = 1

let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', 'package.json', '.git/', '_darcs/', '.hg/', '.bzr/', '.svn/']

if !exists('g:colors_name')
  exe 'colo ' . ((strftime('%H') % 17) > 7 ? 'default' : 'molokai')
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
if exists('g:plugs["tern_for_vim"]')
  autocmd vimrc FileType javascript nnoremap <silent> <buffer> K :TernDoc<CR>
  autocmd vimrc FileType javascript nnoremap <silent> <buffer> <c-]> :TernDef<CR>
  autocmd vimrc FileType javascript nnoremap <silent> <buffer> [D :TernRefs<CR>
endif
