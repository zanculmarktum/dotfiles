se go-=egimrLtT
se gfn=Terminus\ 9
se co=133
se lines=45
se gcr=n-v-c:blinkoff0

se ssop=tabpages
fu SaveTabs()
  let l:tabfile = expand('~/.vimtabs')
  cal delete(l:tabfile)
  exe 'mks! ' . l:tabfile
  let l:lines = []
  for l:l in readfile(l:tabfile)
    if l:l =~ '^edit\s' || l:l =~ '^tabedit\s' || l:l =~ '^set split\(below\|right\)' || l:l =~ '^wincmd\s' || l:l =~ '^tabnext\s'
      cal add(l:lines, l:l)
    en
  endfo
  cal add(l:lines, '" vim: set ft=vim :')
  cal writefile(l:lines, l:tabfile)
endf
",TabEnter
au VimLeave,TabNew,TabClosed * cal SaveTabs()

set bg=dark
"colo bubblegum-256-dark
"colo base16-onedark
colo onedark

pa airline
let g:airline#extensions#whitespace#mixed_indent_algo = 2
" Defaults to ['indent', 'trailing', 'mixed-indent-file']
let g:airline#extensions#whitespace#checks = ['indent', 'trailing']

pa airline-themes
if g:colors_name == 'bubblegum-256-dark'
  let g:airline_theme='bubblegum'
en
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" pa indentLine
if g:colors_name == 'bubblegum-256-dark'
  let g:indentLine_color_gui = '#808080'
elsei g:colors_name == 'base16-onedark'
  let g:indentLine_color_gui = '#545862'
elsei g:colors_name == 'onedark'
  let g:indentLine_color_gui = '#3b4048'
en

" vim:ts=2:sw=2:et
