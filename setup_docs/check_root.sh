#!/bin/bash

CheckIfRoot(){
    if [ `whoami` != root ]; then
	echo "Hello `whoami`. Please rerun this script as user and not root."
	exit 1
    fi
}
