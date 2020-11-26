#!/bin/sh

ssh_util(){
	echo "ssh setup utility."
	read -p "Please enter a valid email address: " email
	# echo "Your email: $email. Is this correct?"
	ssh-keygen -t rsa -C $email
}


if [ ! -d ~/.ssh ]; then
	ssh_util
else
	echo "~/.ssh directory already exists. Exiting."
	exit
fi
