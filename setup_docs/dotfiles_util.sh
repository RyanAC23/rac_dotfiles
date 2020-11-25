#!/bin/sh
# install emacs, urxvt, and tmux to get up and running

# check for dotfiles repo
if [ ! -d ~/repos/rac_dotfiles ]; then
    echo "cloning rac_dotfiles to repos directory."
    git clone git@github.com:RyanAC23/rac_dotfiles.git ~/repos/
    echo "Dotfiles repo created in ~/repos/rac_dotfiles."
    echo "Symlinking .emacs.d directory."
    ln -s ~/repos/rac_dotfiles/ubuntu/.emacs.d ~/.emacs.d
    echo "symlinking personal scripts."
    if [ ! -d ~/repos/rac_dotfiles/common ]; then
	ln -s ~/repos/rac_dotfiles/common/scripts ~/bin
    fi
    echo "Checking if ubuntu specific folder is present, then copying essential dotfiles to home directory."
    if [ ! -d ~/repos/rac_dotfiles/ubuntu ]; then
	cp ~/repos/rac_dotfiles/ubuntu/{.bash_aliases,.bashrc,.tmux.conf,.Xresources} ~/
	echo "combining backup .config folders to live ~/.config folder."
	cp -rt ~/repos/rac_dotfiles/ubuntu/.config ~/.config
	echo "symlinking .urxvt config."
	ln -s ~/repos/rac_dotfiles/ubuntu/.urxvt ~/.urxvt
    fi
fi
echo "Dotfiles synced."
echo
