#!/bin/sh
# install emacs, urxvt, and tmux to get up and running

guaya=true

case $guaya in
    true)
	echo "success! Guayadeque installed."
	;;
    false)
	echo "Guayadeque install skipped."
	;;
    *)
	echo "Guaya needs to be set to true or false."
esac
echo
