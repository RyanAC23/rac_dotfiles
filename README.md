# Neptune : Config
<h3 align="center">D-Clips</h3>

<div align="center">

</div>

## Table of Contents
- [About](#about)
- [Package list](#packages)
- [Config files list](#configs)
- [Bugs](#bugs)
- [To do](#todo)

---

## About <a name = "about"></a>
My personal setup and config files, and scripts I use to quickly reinstall. Options for both a quick install and full install are present, differing only in the number of packages installed.<br>

Obviously on a new system git probably won't be installed, and ssh won't be set up. You'll have to set up one or both.
Grab the initial setup file from the below public repo:
```
wget RyanAC23.github.io/compute/files/dclip-setup.sh
```
Run this script (you may need to change permissions) and you should have this full repository on your machine. Run the two scripts

```
sudo ./packageinstall.sh
./dotfiles_util.sh
```
After this, restart your computer. This should be it.

## Package List <a name = "packages"></a>
Packages are divided into two sets, called core and full. The core list contains lighter and more essential packages, and the full list contains more intensive ones (including resource hog texlive-full) so that I can choose to skip them until later. Both tables are combined into one below.

| Packages                                                   | Description                                          | Notes                                          |
| -----------------------------------------------------------|------------------------------------------------------|------------------------------------------------|
| [emacs](https://www.gnu.org/software/emacs/)               | Extensible, customizable text editor                 | .emacs.d symlinked to $HOME                    |
| [git](https://git-scm.com/)                                | Fast distributed version control system              |                                                |
| [ranger](http://ranger.github.io/)                         | Simple console file manager                          |                                                |
| [urxvt](http://software.schmorp.de/pkg/rxvt-unicode.html)  | Unicode enabled rxvt-clone terminal emulator (urxvt) | .urxvt symlinked to $HOME                      |
| [xclip](https://github.com/astrand/xclip)                  | Command line interface to the X11 clipboard          |                                                |
| [libx11-dev](https://packages.ubuntu.com/xenial/libx11-dev) | Dependencies                                        |                                                |
| [libxft-dev](https://packages.ubuntu.com/xenial/libxft-dev) | Dependencies                                        |                                                |
| [ffmpeg](https://ffmpeg.org/)                              | Cross platform audio and video editor                |                                                |
| [pavucontrol](https://freedesktop.org/software/pulseaudio/pavucontrol/) | Volume mixer                            |                                                |
| [gfortran](https://gcc.gnu.org/wiki/GFortran)              | GNU Fortran compiler                                 |                                                |
| [gcc](https://gcc.gnu.org/)                                | GNU Compiler Collection, C/C++                       |                                                |
| [tmux](https://github.com/tmux/tmux)                       | Terminal multiplexer                                 |                                                |
| [rxvt-unicode](http://software.schmorp.de/pkg/rxvt-unicode.html) | Terminal emulator with unicode support         | Perl extensions in .urxvt semi-essential       |
| [RANGER](https://github.com/ranger/ranger)                 | Console file manager with vi-like bindings           |                                                |
| [neofetch](https://github.com/dylanaraps/neofetch)         | System information tool                              |                                                |
| [tree](https://linux.die.net/man/1/tree)                   | List contents of directories in tree format          |                                                |
| [ncdu](https://dev.yorhel.nl/ncdu)                         | NCurses Disk Usage disk analyzer                     |                                                |
| [vim](https://www.vim.org/)                                | Lightweight text editor.                             | For occasional use with programs               |
| [numlockx](http://manpages.ubuntu.com/manpages/trusty/man1/numlockx.1.html) | Control the state of numlock  | For startup numlock option on desktop with numpad    |
| [nomacs](https://nomacs.org/)                              | Image viewer                                         | eom is awful                                   |
| [guayadeque](https://www.guayadeque.org/)                  | Audio player                                         |                                                |
| [mpv](https://mpv.io/)                                     | Cross platform media player                          |                                                |
| [texlive-full](https://tug.org/texlive/)                   | TeX document production system                       | THE killer of a quick install                  |
| [stellarium](http://stellarium.org/)                       | Open source planetarium software                     |                                                |
| [asunder](http://littlesvr.ca/asunder/)                    | CD ripper                                            | For trips to the thrift store                  |
| [krita](https://krita.org/en/)                             | Painting/image editor                                |                                                |
| [gparted](https://gparted.org/)                            | GUI partition manager                                |                                                |
| [gstreamer1.0-plugins-bad](https://packages.debian.org/sid/gstreamer1.0-plugins-bad) | Audio codecs               | For straggling .m4a files                      |
| [Transmission](https://transmissionbt.com/download/)       | Torrent client                                       |                                                |
## Config files list <a name = "configs"></a>
| file / directory name               | Description                                                                       |
|-------------------------------------|-----------------------------------------------------------------------------------|
| /bin                                | Contains dumb scripts                                                             |
| /conda-envs                         | Conda environments to install                                                     |
| /etc/lightdm/slick-greeter.conf     | For setting the login background and numlock                                      |
| /packages                           | .csv lists of packages used by install scripts                                    |
| /setup_docs                         | The scripts for installing a new setup                                            |
| /.emacs.d                           | emacs config folder to be symlinked to $HOME. Main file is racinit.org            |
| /.urxvt                             | rxvt-unicode functionality scripts                                                |
| /.vim                               | vim configurations. This could be vetted.                                         |
| /.guayadeque                        | guayadeque music player config. .gitignore passes its (massive) database          |
| .bashrc                             | executed on login to a new shell                                                  |
| .bash_aliases                       | Contains useful aliases; sourced by .bashrc                                       |
| .tmux.conf                          | Key bindings and behavior for tmux. Made to be sort of emacs-like                 |
| .Xresources                         | .urxvt display settings and bindings. Source with xrdb                            |
| /.config                            | Folder with config for nicer packages that didn't want to clutter $HOME           |
| /.config/autostart                  | Turns off blueman by default. When do you use bluetooth?                          |
| /.config/dconf                      | Main display and toolbar settings for Ubuntu-Mate.                                |
| /.config/neofetch                   | My settings for what info neofetch displays, plus a custom ascii logo.            |
| /.config/ranger                     | Minor settings telling ranger to preview images and use emacs.                    |
| /.config/.environment_site          | Command for quickly running a jupyter server. Could be merged with .bash_aliases. |

## Other Packages
Most of these are things I've played with in the past but no longer use.
- Wireshark
- Lynx
- MC
- Terminator
- Chirp
- vscode
- xscreensaver, xscreensaver-data-extra, xscreensaver-gl-extra



# Updates

## Saturday 17 December 2022
[Since a version update](https://unix.stackexchange.com/questions/644819/is-it-possible-to-move-tmux-conf-to-config-folder), tmux can now store its config file in `.config`. I'm all about keeping the root directory clean.

## Tuesday March 08 2022
- Started a Makefile for Miniconda setup. Expand to other things eventually.

##  Tuesday September 14 2021
- Fixed .environment_site conda location
- Updated emacs with ppa repository

```
sudo add-apt-repository -y ppa:ubuntu-elisp
sudo apt update
sudo apt install emacs-snapshot
```

## Saturday August 21 2021
- New quotes.

## Monday February 08 2020
- Added libreoffice and gnuplot to installer.


# Bugs <a name = "bugs"></a>
## January 10 2021
- Dropbox will hang if run from the script. Move this to the end or find a way to close the Dropbox session after installing that won't kill the install script.
- Chrome reinstalls every time you run the full script, even if already present.
- .config/dconf does not copy properly. Change ~rsync~ to ~cp -r~ command.

## November 29 2020

- [x] possible bug with dconf file not copying with rsync command. Try on VM and see if this is reproducible on another machine.

## December 07 2020
- [ ] Should create a small new script to check for changes between config files, and to sync them

## November 28 2020
- [x] .csv files must end with newline or the installer will miss the last package.

# To Do <a name = "todo"></a>
- [x] Add wallpaper copy/set from Dropbox public link
  ```
  sudo cp [dropbox]/login.png /usr/share/backgrounds/ubuntu-mate-common/
  ```
- [x] Add config file explanations
- [x] Add Dropbox setup
- [ ] Add Conda setup
- [ ] Add Conda environment install
- [x] Make one config folder symlink routine
- [x] Turn off bluetooth controller by default
- [x] Add numlockx to package list
