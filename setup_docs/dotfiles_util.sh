#!/bin/sh
# install emacs, urxvt, and tmux to get up and running

## Check functions
# repos directory
Repo_Dir_Check(){
	if [ ! -d ~/repos ]; then
		echo "Creating ~/repos directory."
		mkdir ~/repos
	fi
}

# dotfiles repo
Dotfiles_Repo_Check(){
	if [ ! -d ~/repos/rac_dotfiles ]; then
		echo "Cloning rac_dotfiles repository to ~/repos."
    		git clone git@github.com:RyanAC23/rac_dotfiles.git ~/repos/rac_dotfiles
		echo "Dotfiles repo created in ~/repos/rac_dotfiles."
	else
		echo "Note: rac_dotfiles already exists. Are you trying to update? If so, navigate to the repository and update it with git pull and try again."
	fi
	}

# create symlinks for bin, .emacs.d, .urxvt

# copy other files
Create_Dotfile_Symlinks(){
	if [ ! -d ~/.emacs.d ]; then
		echo "Symlinking .emacs.d directory."
    	   	ln -s ~/repos/rac_dotfiles/ubuntu/.emacs.d ~/.emacs.d
	else
		echo "Note: .emacs.d directory already present. No symlink created."
	fi
	if [ ! -d ~/bin ]; then
    		echo "symlinking personal scripts."
		ln -s ~/repos/rac_dotfiles/common/scripts ~/bin
	else
		echo "Note: ~/bin already exists."
	fi
	if [ ! -d ~/.urxvt ]; then
		echo "symlinking .urxvt config."
		ln -s ~/repos/rac_dotfiles/ubuntu/.urxvt ~/.urxvt
	else
		echo "Note: ~/.urxvt already exists."
	fi
}

Copy_Config_Files(){
	echo "Warning: this will overwrite any previous files you had in ~/. Proceed? (y/n): "
	read PROCEED
	case $PROCEED in
	    y)
		ROOT_DOTFILES=".bash_aliases .bashrc .tmux.conf .Xresources"
		for i in $ROOT_DOTFILES; do
		    rsync -ahu ~/repos/rac_dotfiles/ubuntu/$i ~/
		done
		echo "combining backup .config folders to live ~/.config folder."
		rsync -au ~/repos/rac_dotfiles/ubuntu/.config ~/.config
			;;
	    n) echo "Skipping dotfile copy."
			;;
	    *) echo "Error: Invalid input. Skipping."
			;;
	esac
}

##### main program #####

Repo_Dir_Check

Dotfiles_Repo_Check

Create_Dotfile_Symlinks

Copy_Config_Files

echo "Dotfile util complete."
echo
