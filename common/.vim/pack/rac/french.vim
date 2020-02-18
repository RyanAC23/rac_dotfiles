" Add function for easy typing of special French characters

let g:fr_char=0

function! FrToggle()
        if g:fr_char==0
            call FrOn()
        else
            call FrOff()
        endif
endfunction

function! FrOn()
        let g:fr_char=1
        echo "French character mapping enabled."
        " L'accent aigu
        imap ;ae é
        " L'accent grave
        imap ;ga à
        imap ;ge è
        imap ;gu ù

        " L'accent circonflexe (chapeau)
        imap ;ca â
        imap ;ce ê
        imap ;ci î
        imap ;co ô
        imap ;cu û

        " La cédille
        imap ;cc ç

        " Le tréma
        imap ;te ë
        imap ;ti ï
        imap ;tu ü

        imap ;oe œ
endfunction

function! FrOff()
        let g:fr_char=0
        echo "French character mapping disabled."
        " L'accent aigu
        iunmap ;ae

        " L'accent grave
        iunmap ;ga
        iunmap ;ge
        iunmap ;gu

        " L'accent circonflexe (chapeau)
        iunmap ;ca
        iunmap ;ce
        iunmap ;ci
        iunmap ;co
        iunmap ;cu

        " La cédille
        iunmap ;cc

        " Le tréma
        iunmap ;te
        iunmap ;ti
        iunmap ;tu

        iunmap ;oe
endfunction

