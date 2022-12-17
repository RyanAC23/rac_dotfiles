#!/bin/bash

LOGIN="https://www.dropbox.com/s/m19gngd5sbostw2/login.png"
LAPTOP="https://www.dropbox.com/s/d4morj7dt9wriwh/laptop_wall.png"
DESKTOP="https://www.dropbox.com/s/4j1eieeepv6brga/desktop_wall.jpg"

WALLPAPERS=($LOGIN $LAPTOP $DESKTOP)

# Run DownloadBGs to download all images.
# Run SetLaptop/SetDesktop to set up the wallpapers on your appropriate system.

DownloadBGs(){
    for url in ${WALLPAPERS[@]}; do
	wget $url -P /home/ryan/Downloads/temp/
    done
}

SetLoginBG(){
    BG_DIR="/usr/share/backgrounds/ubuntu-mate-common"
    BG_LOCATION="$BG_DIR/login.png"
    CONFIG_FILE="/usr/share/glib-2.0/schemas/30_ubuntu-mate.gschema.override"

    # if [ -f $BG_LOCATION ]; then
    #  	sudo rm $BG_LOCATION
    # fi
    # sudo wget $LOGIN -P $BG_DIR
    sudo sed -in "s,\/usr.*\(png\|jpg\),"$BG_LOCATION",g" $CONFIG_FILE
    # sudo glib-compile-schemas /usr/share/glib-2.0/schemas/
    cat $CONFIG_FILE | grep "background="
}


SetLaptopBG(){
    if [ ! -f "$HOME/.config/backgrounds/laptop_wall.png" ]; then
	    wget $LAPTOP -P $HOME/.config/backgrounds/
    fi
}

SetDesktopBG(){
    if [ ! -f "$HOME/.config/backgrounds/desktop_wall.jpg" ]; then
	wget $DESKTOP -P $HOME/.config/backgrounds/
    fi
}

SetLaptop(){
    SetLoginBG
    SetLaptopBG
}

SetDesktop(){
    SetLoginBG
    SetLaptopBG
}

main(){
    source ./check_root.sh
    CheckIfRoot
}

# --------------------------------
main
"$@"
