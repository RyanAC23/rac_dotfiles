MINICONDA_LOC = "https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh"

# ------------------------------------------------------------------------------------
# Program settings
# ------------------------------------------------------------------------------------

gitConfig:
	git config --global --replace-all user.name "Ryan A. Corbin"
	git config --global core.excludesfile "/home/ryan/.config/.gitignore_global"
	git config --global --replace-all core.editor "emacs -nw -Q -load ~/.emacs.d/light_init.el"
	git config --global alias.st 'status -s'
	git config --global alias.lg  'log --graph --all -20 --pretty=format:"%C(bold magenta)%h%C(reset) - %C(dim magenta)(%ar)%C(reset)%C(auto)%d%C(reset)%n%C(dim white)%s%C(dim magenta) - %an%C(reset)"'
	git config --global init.defaultBranch master
	echo "Don't forget to set git config --global user.email!"


Miniconda:
	if [ ! "~/apps/" ]; then \
	mkdir ~/apps/; \
	fi
	cd ~/apps/; \
	wget $(MINICONDA_LOC); \
	bash Miniconda3-latest-Linux-x86_64.sh

# ------------------------------------------------------------------------------------
# Look and appearance
# ------------------------------------------------------------------------------------

disableBell:
	sudo ./${script_dir}/look_and_behavior.sh disable_login_bell

laptopWalls:
	sudo ./backgrounds.sh SetLaptop

desktopWalls:
	sudo ./backgrounds.sh SetDesktop

_loginWall:
	sudo ./backgrounds.sh SetLoginBG

dlWallpapers:
	./backgrounds.sh DownloadBGs

theme:
	# This should have two options : 90s, Neptune. You know what I mean.
	echo "Not Implemented."
