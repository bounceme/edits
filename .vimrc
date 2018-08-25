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

function! g:PPPP()
  while search('^LETTER','W')
    norm yil
    exe '.+1,$s/\V\^'.escape(@@,'\').'\_.\{-}\ze\n\^LETTER/'
  endwhile
endfunction

set fo+=j fo-=o fo-=t fo+=c
if has('gui_running') || has('nvim')
  set gcr=
  if has("gui_macvim")
    let macvim_skip_cmd_opt_movement = 1
  endif
endif
if has('mac')
  set guifont=Monaco:h16
endif
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

let g:se = [1513620419,4]
if !exists('*g:Rem')
  if &viminfo isnot ''
    set viminfo+=%25
  endif
  function g:Rem()
    let g:sc = (g:se[1] - (localtime() - g:se[0]) / 60 / 60 / 24) % 5
    let g:sc = 0 + tr(g:sc < 0 ? 5 + g:sc : g:sc,0,5)
    au vimrc VimLeave * wv!
  endfunction
  if v:vim_did_enter
    call g:Rem()
  else
    au vimrc VimEnter * call g:Rem() | au! vimrc Vimenter *
  endif
endif

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

set statusline=%<%{fnamemodify(expand('%'),':.')}\ %h%m%r%Y\ %{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

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

if has('mac') && !has('gui_running') && !has('clipboard')
  function! g:Clip(type, ...)
    let sel_save = &selection
    let &selection = "inclusive"
    let reg_save = @@
    if a:0
      silent exe "normal! gvy"
    elseif a:type == 'line'
      silent exe "normal! '[V']y"
    else
      silent exe "normal! `[v`]y"
    endif
    call system('pbcopy',@@)
    let &selection = sel_save
    let @@ = reg_save
  endfunction
  nnoremap <silent> "*y :set opfunc=Clip<CR>g@
  nnoremap <silent> "*Y :set opfunc=Clip<CR>g@$
  xnoremap <silent> "*y :<C-U>call Clip(visualmode(), 1)<CR>
endif

function! s:OF()
  let a = bufnr('')
  while bufnr('') == a
    let c = getpos('.')
    exe "norm! \<C-o>"
    if c == getpos('.')
      break
    endif
  endwhile
endfunction
command! OFile call <SID>OF()

function! s:Align()
  let pos = [search('\m^$\|\%^','bnW'),search('\m^$\|\%$','nW')]
  let bal = max(map(getline(pos[0],pos[1]), "matchend(v:val,'^\\S\\+') + 1"))
  if bal > 0
    execute 'silent keeppatterns keepjumps' join(pos,',')
          \ .'substitute/\m^\S\+\s\+/\=printf("%-".bal."S",submatch(0))'
  endif
endfunction
command! YYY call <SID>Align()

command! MakeTags silent! exe '!find . -iname "*.%:e" | xargs ctags' | redraw!

silent! set inccommand=nosplit

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
" Plug 'airblade/vim-rooter'

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
Plug 'mattn/emmet-vim', { 'for': ['html','php'] }
Plug 'ervandew/supertab'
Plug 'bounceme/remote-viewer'
Plug 'justinmk/vim-dirvish'

call plug#end()
catch
  echo 'NO PLUGINS'
  finish
endtry

let g:jsx_check_react_import = 1
let g:surround_indent = 0
let g:poppy_point_enable = 1
let g:no_extend_comment_CR = &fo !~# 'r'
if exists('##optionset')
  au vimrc optionset formatoptions let g:no_extend_comment_CR = &fo !~# 'r'
endif
let g:CoolTotalMatches=1
let g:dirvish_mode=2

if exists('g:plugs["extendCR.vim"]')
  imap <CR> <PLUG>extendCR
endif

if exists('g:plugs["fairedit.vim"]')
  nmap C <Plug>Fair_M_C
  nmap D <Plug>Fair_M_D
  omap $ <Plug>Fair_M_dollar
  nunmap Y
  nmap Y <Plug>Fair_M_yEOL
endif

nnoremap <silent><expr> <c-]> empty(tagfiles()) ? ":DimJumpPos<cr>" : "<c-]>"

function! s:InitJBuf()
  noremap <buffer> Z! :w !node -p<cr>
  setl path=.,node_modules,,
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
nnoremap <silent> <LEADER>hp :call clearmatches() \| let g:poppy = -get(g:,'poppy',-1) \|
      \ exe 'au! Poppy CursorMoved *' . (g:poppy > 0 ? ' call PoppyInit()' : '') <cr>

au vimrc cmdWinEnter [:>] syntax sync maxlines=1 minlines=1

let g:rooter_use_lcd = 1
let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', '.git/', '_darcs/', '.hg/', '.bzr/', '.svn/', 'package.json']

" inoremap <silent><expr><bs>
"       \ (&indentexpr isnot '' ? &indentkeys : &cinkeys) =~? '!\^F' &&
"       \ &backspace =~? '.*eol\&.*start\&.*indent\&' &&
"       \ !search('\S','nbW',line('.')) ? (col('.') != 1 ? "\<C-U>" : "") .
"       \ "\<bs>" . (getline(line('.')-1) =~ '\S' ? "" : "\<C-F>") : "\<bs>"

function! s:rmrf() abort
  call inputsave()
  if input('delete '.getline('.').' ? (y/n)') ==# 'y'
    if !delete(getline('.'),'rf')
      let p = winsaveview()
      edit
      call winrestview(p)
    endif
  endif
  call inputrestore()
endfunction
au vimrc filetype dirvish nnoremap <buffer> D :call <SID>rmrf()<cr>

if !exists('g:colors_name')
  exe 'colo' ((strftime('%H') % 20) > 7 ? 'default' : 'molokai')
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
