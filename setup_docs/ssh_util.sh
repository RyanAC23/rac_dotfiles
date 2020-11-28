#!/bin/sh

ssh_util(){
	echo "ssh setup utility."
	read -p "Please enter a valid email address: " email
	# echo "Your email: $email. Is this correct?"
	ssh-keygen -t rsa -C $email
	cat .ssh/id_rsa.pub | xclip -sel clip
	echo "id_rsa.pub has been added to the clipboard."
        echo "Please paste in bitbucket and github now."
}


if [ ! -d ~/.ssh ]; then
	ssh_util
else
	echo "~/.ssh directory already exists. Exiting."
	exit 1
fi
