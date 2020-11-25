#!/bin/sh
# install emacs, urxvt, and tmux to get up and running

PRIMARY_PACKAGES="emacs tmux rxvt-unicode"

SECONDARY_PACKAGES="libx11-dev libxft-dev w3m"


echo "Installing primary packages."
for i in $PRIMARY_PACKAGES; do
	sudo apt install -y $i
done

echo "Installing secondary packages."
for i in $SECONDARY_PACKAGES; do
	sudo apt install -y $i
done

echo "Dotfiles mission critical packages installed."
echo
