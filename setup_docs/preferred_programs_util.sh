#!/bin/sh
# install emacs, urxvt, and tmux to get up and running

guaya=true

MEDIA_PACKAGES="ffmpeg pavucontrol nomacs mpv krita"

PRODUCTIVITY_PACKAGES="ncdu tree gparted"

echo "Installing media packages."
for i in $MEDIA_PACKAGES; do
	sudo apt install -y $i
done

echo "Installing productivity packages."
for i in $PRODUCTIVITY_PACKAGES; do
	sudo apt install -y $i
done

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

sudo apt upgrade
sudo apt autoremove

echo "Packages installed."
echo
