setlocal nu

"Write the command, escape, go up one line, and enter insert mode.
inoremap \ba \begin{align}<Return><Return>\end{align}\\<esc>1ki
inoremap \tb \textbf{++}<esc>F+T{2xi
inoremap \bi \begin{itemize}<Return>\item <Return>\end{itemize}\\<esc>1ki<end>

