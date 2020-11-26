setlocal nu

inoremap <Space><Space> <Esc>/<++><Enter>"_c4l

inoremap ;i <em></em><Space><++><Esc>FeT>i
inoremap ;b <b></b><Space><++><Esc>FbT>i
inoremap ;p <p></p><Space><++><Esc>FpT>i
" The following commands use a different form to allow multi-line search.
inoremap ;h1 <h1></h1><br><Return><++><Esc>?1<Return>T>i
inoremap ;h2 <h2></h2><br><Return><++><Esc>?2<Return>T>i

" Make a command that launches the file in your default browser.
" silent !start %
" silent !start %
" This should be severely changed so that the page is refreshed
" if the source file is modified. Check out entr
" eradman.com/entrproject/
! google-chrome *.html

function! ReloadHTML()
        silent !ls *.css *.html | entr $HOME/.vim/pack/rac/reload-browser google-chrome
        echo "html preview refreshed.
endfunction

nm <leader>r :call ReloadHTML()<CR>
