MINICONDA_LOC = "https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh"

Miniconda:
	if [ ! "~/apps/" ]; then \
	mkdir ~/apps/; \
	fi
	cd ~/apps/; \
	wget $(MINICONDA_LOC); \
	bash Miniconda3-latest-Linux-x86_64.sh

gitConfig:
	git config --global --replace-all user.name "ryan"
	git config --global core.excludesfile "/home/ryan/.config/.gitignore_global"
	git config --global --replace-all core.editor "emacs -nw -Q -load ~/.emacs.d/light_init.el"
	git config --global alias.lg 'log  --graph --all -20 --pretty=format:"%C(yellow)%h\ %Cblue[%cn]%Creset\ (%cr)\ %Cred%d%Creset%n\ \ %s"'
	echo "Don't forget to set git config --global user.email!"
