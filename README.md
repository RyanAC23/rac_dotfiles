# rac_dotfiles
<h3 align="center">dotfiles</h3>

<div align="center">

</div>

---

<p align="center"> My personal dotfiles.
    <br>
</p>

## Table of Contents
- [About](#about)
- [Getting Started](#getting_started)
- [Package List](#packages)
- [To Do](#todo)

## About <a name = "about"></a>


## Getting Started <a name = "getting_started"></a>

### Ubuntu Mate

```
code block
```

## Package List <a name = "packages"></a>
Packages are divided into two sets, called core and full. The core list contains lighter and more essential packages, and the full list contains more intensive ones (including resource hog texlive-full) so that I can choose to skip them until later. Both tables are combined into one below.

| Packages                                                  | Description                                          | Notes                                          |
| ----------------------------------------------------------|------------------------------------------------------|------------------------------------------------|
| [emacs](https://www.gnu.org/software/emacs/)              | The extensible, customizable text editor             | .emacs.d symlinked to $HOME                    |
| [git](https://git-scm.com/)                               | fast distributed version control system              |                                                |
| [ranger](http://ranger.github.io/)                        | Simple console file manager                          |                                                |
| [urxvt](http://software.schmorp.de/pkg/rxvt-unicode.html) | Unicode enabled rxvt-clone terminal emulator (urxvt) | .urxvt symlinked to $HOME                      |
| [w3m](http://w3m.sourceforge.net/)                        | text-based web browser                               | Installed as a dependency for urxvt and ranger |
| [xclip](https://github.com/astrand/xclip)                 | Command line interface to the X11 clipboard          |                                                |
| [mercurial]                                               |                                                      |                                                |
| [libx11-dev]                                              |                                                      |                                                |
| [libxft-dev]                                              |                                                      |                                                |
| [ffmpeg]                                                  |                                                      |                                                |
| [pavucontrol]                                             |                                                      |                                                |
| [gfortran]                                                |                                                      |                                                |
| [gcc]                                                     |                                                      |                                                |
| [tmux]                                                    |                                                      |                                                |
| [rxvt-unicode]                                            |                                                      |                                                |
| [ranger]                                                  |                                                      |                                                |
| [neofetch]                                                |                                                      |                                                |
| [tree]                                                    |                                                      |                                                |
| [ncdu]                                                    |                                                      |                                                |
|-----------------------------------------------------------|------------------------------------------------------|------------------------------------------------|
| [nomacs]                                                  |                                                      |                                                |
| [guayadeque]                                              |                                                      |                                                |
| [mpv]                                                     |                                                      |                                                |
| [texlive-full]                                            |                                                      |                                                |
| [stellarium]                                              |                                                      |                                                |
| [screenkey]                                               |                                                      |                                                |
| [virtualbox]                                              |                                                      |                                                |
| [asunder]                                                 |                                                      |                                                |
| [krita]                                                   |                                                      |                                                |

## To Do <a name = "todo"></a>
(2020-11-27) - There is currently an indexing bug in the package install loop which causes the final package in the .csv file to be missed.

	     - Most of the packages need links and short descriptions still, but it's 01:06:00.

	     - Ubuntu-Mate toolbars and customization are missing. Find out which files these are and make sure they're in the repo.

	     - Move music_util to private, new repo and forget about it for a while.

	     - Remove redundant files and ensure you have all the main dotfiles you'd like for now.