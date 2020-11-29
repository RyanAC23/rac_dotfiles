#!/bin/sh

USER=ryan
[ -z "$core_packages" ] && core_packages="/home/$USER/repos/rac_dotfiles/packages/core.csv"
[ -z "$full_packages" ] && full_packages="/home/$USER/repos/rac_dotfiles/packages/full.csv"

##### Functions #####

InstallFromList(){
     echo "InstallFromList flag. $package_todo"
     if [ -f $package_todo ]; then
	 sed '/^#/d' $package_todo > /tmp/pack.csv
     else
	 echo "Package .csv file not found."
	 exit 1
     fi
     number_of_packages=$(wc -l < /tmp/pack.csv)
     echo "$number_of_packages packages to install."
     n=0
     while IFS=, read -r tag package description; do
	 n=$((n+1))
	 echo "Installing package $n/$number_of_packages."
	 case "$tag" in
	       "g") if ! grep -q "^deb .*anonbeat/guayadeque" /etc/apt/sources.list /etc/apt/sources.list.d/*; then
	       	       add-apt-repository ppa:anonbeat/guayadeque
		    fi
		    apt install -y $package  ;;
	       "s") echo "Skipping $package for dev install." ;;
	         *) apt install -y $package  ;;
	 esac
     done < /tmp/pack.csv ;
}

CoreInstaller(){
    package_todo=$core_packages
#    echo "core installer flag. $package_todo"
    InstallFromList
}

FullInstaller(){
    CoreInstaller
    package_todo=$full_packages
#    echo "full installer flag. $package_todo"
    InstallFromList
    ExtraInstall
}

InstallMainRoutine(){
    echo "Installing packages from /packages/*.csv; do you want to do a full install?"
    read -p "If no, only the base applications will be installed. Press q to quit. (full/core/q): " ROUTINE
    case $ROUTINE in
	full) FullInstaller   ;;
	core) CoreInstaller   ;;
	q) echo "Exiting."
	   exit 1             ;;
	*) echo "Invalid option."
	   InstallMainRoutine ;;
    esac
}

CheckIfRoot(){
    if [ `whoami` != root ]; then
	echo "To run this program with useful results, you must be root."
	exit 1
    fi
}

Initializer(){
    clear
    echo "Running Ryan's package intaller for a fresh Ubuntu system."
    CheckIfRoot
    read -p "Upgrading apt packages before installing new ones. Are you sure? (y/n/q): " CHECK
    case $CHECK in
	y) apt upgrade ;;
	n) :           ;;
	q) echo "Exiting."
	   exit 1             ;;
	*) echo "Invalid option."
	   Initializer ;;
    esac
}

ChromeGet(){
    echo "Grabbing Chrome from the internet."
    wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -P /tmp/
    sudo dpkg -i /tmp/google-chrome-stable_current_amd64.deb
    rm /tmp/google-chrome-stable_current_amd64.deb
}

DropboxGet(){
    echo "Grabbing Dropbox from the internet."
    cd ~ && wget -O "https://www/dropbox.com/download?plat=lnx.x86_64" | tar xzf -
    ~/.dropbox-dist/dropboxd
}

CondaInstall(){
    echo "Conda Install Not Implemented"
}

ExtraInstall(){
echo "Installing extra things that can't be gotten through the apt manager."
    ChromeGet
    DropboxGet
    CondaInstall
}

CleanupTool(){
    apt upgrade
    apt autoremove
}

Finalizer(){
    read -p "Main routine complete. Clean up? (y/n/q): " CLEANUP
    case $CLEANUP in
	y) CleanupTool ;;
	n) :           ;;
	q) echo "Exiting."
	   exit 1             ;;
	*) echo "Invalid choice."
	   Finalizer   ;;
    esac
    if [ -f "/tmp/pack.csv" ]; then
	rm "/tmp/pack.csv"
    fi
    echo "Install script complete."
    echo
}

##### Main Program #####
Initializer
InstallMainRoutine
Finalizer
