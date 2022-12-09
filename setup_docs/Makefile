MINICONDA_LOC = "https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh"

Miniconda:
	if [ ! "~/apps/" ]; then \
	mkdir ~/apps/; \
	fi
	cd ~/apps/; \
	wget $(MINICONDA_LOC); \
	bash Miniconda3-latest-Linux-x86_64.sh

GitSettings:
	git config --global user.name "ryan"
	git config --global core.excludesfile "/home/ryan/.config/.gitignore_global"
	git config --global core.editor "emacs -nw -Q -load ~/.emacs.d/light_init.el"
	git config --global alias.lg 'log --pretty=format:"%C(yellow)%h\ %Cblue[%cn]%Creset\ (%cr)%n\ \ %s" --graph -10'
	echo "Don't forget to set git config --global user.email!"
