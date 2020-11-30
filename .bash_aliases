alias v='vim'
alias cn='clear ; neofetch'
alias reload='source ~/.bashrc'

alias ytv='youtube-dl --add-metadata -ic'
alias yta='youtube-dl --add-metadata -x --audio-format mp3'

alias FFMRec='ffmpeg -video_size 1920x1080 -framerate 25 -f x11grab -i :0.0+0,0 -f alsa -ac 2 -i pulse -ab 168k'
alias FFMsm='ffmpeg -video_size 1920x1080 -framerate 20 -f x11grab -i :0.0+0,0 -f alsa -ac 2 -i pulse -b:a 168k -b:v 1500k'

# alias e='emacs' # no longer necessary
# alias ec='emacsclient -n' # both commands combined into the better one below.
# tries to run an emacs client on the file; if there is no client,
# falls back on opening a new one. If no file is specified, runs emacs startup screen.
alias e='emacsclient -n -a emacs || emacs'
