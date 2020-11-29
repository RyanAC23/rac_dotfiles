#!/bin/sh

CheckIfRoot(){
    if [ `whoami` != root ]; then
	echo "Welcome `whoami`."
    else
	echo "Please rerun this script as user and not root."
	exit 1
    fi
}

Initializer(){
    CheckIfRoot
    read -p "You need git and xclip to run this script. Apt install now? (y/n):" INITIALIZE
    case $INITIALIZE in
	y) sudo apt install git xclip ;;
	n) echo "Skipping install step." ;;
	q) exit 1 ;;
	*) echo "Invalid option."
	   Initializer ;;
    esac
}

Pause(){
    read -p "Press any key to continue." VARIABLE
}

sshUtil(){
    if [ ! -d ~/.ssh ]; then
	echo "ssh setup and dotfiles clone utility."
	read -p "Please enter a valid email address: " email
	# echo "Your email: $email. Is this correct?"
	ssh-keygen -t rsa -C $email
	cat $HOME/.ssh/id_rsa.pub | xclip -sel clip
	echo "id_rsa.pub has been added to the clipboard."
    else
	read -p "~/.ssh directory already exists. Copy public key anyway? (y/n): " KEY
	case $KEY in
	    y) cat $HOME/.ssh/id_rsa.pub | xclip -sel clip
   	       echo "id_rsa.pub has been added to the clipboard."
	       ;;
	    n) echo "Skipping this step and opening github."
	       ;;
	    q) exit 1 ;;
	    *) echo "Skipping this step and opening github."
	       ;;
	esac
    fi
}

GitRepoGetter(){
    echo "Opening GitHub. Add your ssh key and continue to clone dotfiles."
    xdg-open "https://github.com/"
    Pause
    if [ ! -d "$HOME/repos/rac_dotfiles/" ]; then
	git clone git@github.com:RyanAC23/rac_dotfiles.git $HOME/repos/rac_dotfiles
    fi
    if [ -f $HOME/repos/rac_dotfiles/setup_docs/packageinstall.sh ]; then
    echo "dotfiles repo present. Be sure to run PackageInstall.sh as root, and
    to run DotfilesSetter.sh second.
    (The order might not actually matter.)"
    fi
}

GitSetter(){
    echo "Initializing global git settings."
    read -p "Set your global git settings now? (y/n): " GITSET
    case $GITSET in
	y) read -p "email: " EMAIL_VAL
	   read -p "username: " USER_VAL
	   echo "email: $EMAIL_VAL
	   	  user: $USER_VAL"
	   read -p "Is this ok? (y/n): " CONFIRM_VAL
	   case $CONFIRM_VAL in
	       y) git config --global user.email $EMAIL_VAL
		  git config --global user.name $USER_VAL
		  echo "Setting emacs as default git editor."
		  git config --global core.editor "emacs -nw -Q"
		  ;;
	       n) GitSetter
		  ;;
	       *) echo "Invalid option."
		  GitSetter
		  ;;
	   esac
	   ;;
	n) echo "Not configuring global git settings."
	   ;;
	*) GitSetter
	   ;;
    esac
    # [ ! `git config user.email` ] && git config --global user.email || git config user.email
    # [ ! `git config user.name` ] && git config --global user.name || git config user.name
}

##### Main Routine #####
#Initializer

#sshUtil

#GitRepoGetter

GitSetter
