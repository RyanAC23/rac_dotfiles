# rac_dotfiles
<h3 align="center">D-Clips</h3>

<div align="center">

</div>

## Table of Contents
- [About](#about)
- [Package List](#packages)
- [Bugs](#bugs)
- [To Do](#todo)

---

## About <a name = "about"></a>
My personal dotfiles, and scripts I use to quickly reinstall. Options for both a quick install and full install are present, differing only in the number of packages installed.<br>

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
| [w3m](http://w3m.sourceforge.net/)                         | text-based web browser                               | Installed as a dependency for urxvt and ranger |
| [xclip](https://github.com/astrand/xclip)                  | Command line interface to the X11 clipboard          |                                                |
| [mercurial](https://www.mercurial-scm.org)                 | Distributed source control management tool           |                                                |
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
|                                                            |                                                      |                                                |
| [nomacs](https://nomacs.org/)                              | Image viewer                                         | eom is awful                                   |
| [guayadeque](https://www.guayadeque.org/)                  | Audio player                                         |                                                |
| [mpv](https://mpv.io/)                                     | Cross platform media player                          |                                                |
| [texlive-full](https://tug.org/texlive/)                   | TeX document production system                       | THE killer of a quick install                  |
| [stellarium](http://stellarium.org/)                       | Open source planetarium software                     |                                                |
| [screenkey](https://gitlab.com/screenkey/screenkey)        | Key screencast tool                                  |                                                |
| [virtualbox](https://www.virtualbox.org/)                  | Cross platform virtualization                        |                                                |
| [asunder](http://littlesvr.ca/asunder/)                    | CD ripper                                            | For trips to the thrift store                  |
| [krita](https://krita.org/en/)                             | Painting/image editor                                |                                                |

## Bugs <a name = "bugs"></a>
(2020-11-28)

	- .csv files must end with new line or the last package will be missed by the installer. Maybe this is expected.

## To Do <a name = "todo"></a>

(2020-11-28)

	- Add Dropbox setup
	- Add Conda setup