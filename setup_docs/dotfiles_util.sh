#!/bin/sh
# install emacs, urxvt, and tmux to get up and running

CheckIfRoot(){
    if [ `whoami` != root ]; then
	echo "Welcome `whoami`."
    else
	echo "Please rerun this script as user and not root."
	exit 1
    fi
}

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
    	   	ln -s ~/repos/rac_dotfiles/.emacs.d ~/.emacs.d
	else
	    echo "Note: .emacs.d directory already present."
	    read -p "Remove existing folder and symlink from repo? (y/n): " SYMLINK
	    case $SYMLINK in
		y) rm -rf $HOME/.emacs.d
		   echo "Symlinking .emacs.d directory."
    	   	   ln -s ~/repos/rac_dotfiles/.emacs.d ~/.emacs.d
		   ;;
		n) echo "Not symlinking .emacs.d directory. "
		   ;;
		*) echo "Invalid choice. Not symlinking."
		   ;;
	    esac
	fi
	# The three (or four) operations here should really be a loop over an array
	# containing the four folders to symlink....
	if [ ! -d ~/bin ]; then
    		echo "symlinking personal scripts."
		ln -s ~/repos/rac_dotfiles/bin ~/bin
	else
		echo "Note: ~/bin already exists."
	fi
	if [ ! -d ~/.urxvt ]; then
		echo "symlinking .urxvt config."
		ln -s ~/repos/rac_dotfiles/.urxvt ~/.urxvt
	else
		echo "Note: ~/.urxvt already exists."
	fi
	if [ ! -d ~/.guayadeque ]; then
		echo "symlinking .guayadeque config."
		ln -s ~/repos/rac_dotfiles/.guayadeque ~/.guayadeque
	else
		echo "Note: ~/.guayadeque already exists."
	fi
}

Copy_Config_Files(){
    echo "Copying dotfiles and config folders to $HOME."
    echo "Warning: this will overwrite any previous files you had in ~/. Proceed? (y/n): "
    read PROCEED
    case $PROCEED in
	y)
	    ROOT_DOTFILES=".bash_aliases .bashrc .tmux.conf .Xresources"
	    for i in $ROOT_DOTFILES; do
		rsync -ahu ~/repos/rac_dotfiles/$i ~/
	    done
	    echo "combining backup .config folders to live ~/.config folder."
	    rsync -au ~/repos/rac_dotfiles/.config/ ~/.config
	    ;;
	n) echo "Skipping dotfile copy."
	   ;;
	*) echo "Error: Invalid input. Skipping."
	   ;;
    esac
}

SetBackgrounds(){
    if [ ! -f "/usr/share/backgrounds/ubuntu-mate-common/login.png" ]; then
	sudo wget https://www.dropbox.com/s/7qpwun1mifq9ssn/login.png -P /usr/share/backgrounds/ubuntu-mate-common/
	cp -r $HOME/repos/rac_dotfiles/etc/lightdm/ /tmp/lightdm
	sudo cp -r /tmp/lightdm/slick-greeter.conf /etc/lightdm/
    fi
    if [ ! -f "$HOME/.config/backgrounds/laptop_wall.png" ]; then
	    wget https://www.dropbox.com/s/m19gngd5sbostw2/laptop_wall.png -P $HOME/.config/backgrounds/
    fi
    if [ ! -f "$HOME/.config/backgrounds/desktop_wall.jpg" ]; then
	    wget https://www.dropbox.com/s/4j1eieeepv6brga/desktop_wall.jpg -P $HOME/.config/backgrounds/
    fi
}

##### main program #####
CheckIfRoot

Repo_Dir_Check

Dotfiles_Repo_Check

Create_Dotfile_Symlinks

Copy_Config_Files

SetBackgrounds

echo "Dotfile util complete."
echo
