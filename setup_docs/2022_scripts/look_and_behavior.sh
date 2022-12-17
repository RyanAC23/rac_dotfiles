#!/bin/bash


disable_login_bell(){
    # https://ubuntu-mate.community/t/how-to-turn-off-stop-drum-sounds-of-the-login-page-of-ubuntu-mate-22-04lts/25735/4
    # override this terrible sound.
    bell_override_file="/usr/share/glib-2.0/schemas/40_arctica-greeter.gschema.override"

    source ./check_root.sh
    CheckIfRoot
    sudo touch $bell_override_file
    echo [org.ArcticaProject.arctica-greeter] > $bell_override_file
    echo play-ready-sound=false >> $bell_override_file
    sudo glib-compile-schemas /usr/share/glib-2.0/schemas/
}


"$@"
