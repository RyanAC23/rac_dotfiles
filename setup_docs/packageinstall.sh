#!/bin/sh

[ -z "$core_packages" ] && core_packages="$HOME/repos/rac_dotfiles/packages/core.csv"
[ -z "$full_packages" ] && full_packages="$HOME/repos/rac_dotfiles/packages/full.csv"

##### Functions #####

InstallFromList(){
     [ -f "$package_todo" ] && sed '/^#/d' $package_todo > /tmp/pack.csv
     number_of_packages=$(wc -l < /tmp/pack.csv)
     echo "$number_of_packages packages to install."
     n=0
     while IFS=, read -r tag package description; do
	 n=$((n+1))
	 echo "Installing package $n/$number_of_packages."
	 case "$tag" in
	       "g") add-apt-repository ppa:anonbeat/guayadeque
		    apt install $package --dry-run
		    ;;
	         *) apt install $package --dry-run
		    ;;
	 esac
     done < /tmp/pack.csv ;
}

CoreInstaller(){
    package_todo=$core_packages
    InstallFromList
}

FullInstaller(){
    CoreInstaller
    package_todo=$full_packages
    InstallFromList
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

Initializer(){
    clear
    echo "Running Ryan's package intaller for a fresh Ubuntu system."
    if [ `whoami` != root ]; then
       echo "To run this program with any useful results, you must be root."
       exit 1
    else
	read -p "Upgrading apt packages before installing new ones. Are you sure? (y/n): " CHECK
        case $CHECK in
	    y) apt upgrade ;;
	    n) exit 1      ;;
	    *) echo "Invalid option."
	       Initializer ;;
	esac
    fi
}

CleanupTool(){
    apt upgrade
    apt autoremove
}

Finalizer(){
    read -p "Main routine complete. Clean up? (y/n): " CLEANUP
    case $CLEANUP in
	y) CleanupTool ;;
	n) :           ;;
	*) echo "Invalid choice."
	   Finalizer   ;;
    esac
    echo "Install script complete."
    echo
}

##### Main Program #####
Initializer
InstallMainRoutine
Finalizer
