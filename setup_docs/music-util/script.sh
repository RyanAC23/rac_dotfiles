#!/bin/sh
# attempt to incorporate parts of script from
# https://www.howtoforge.com/backing-up-with-rsync-and-managing-previous-versions-history

##### Functions #####
PreviewDryRunLog(){
    read -p "Log for dry run created. Preview? (y/n): " PREVIEW
    case $PREVIEW in
	y) emacs -nw -Q "$HOME/Downloads/test/log/localhost/$timestamp.dryrun" --eval '(setq buffer-read-only t)'
	   ;;
	n) echo "end."
	   ;;
	*) echo "Invalid selection."
	   PreviewDryRunLog
	   ;;
    esac
}

DoDryRun(){
    echo "Beginning dry run."
    rsync --dry-run -verbose --itemize-changes --out-format="%i|%n|" --relative --recursive --update --delete --owner --group --times --links --safe-links --super --one-file-system --devices $DirToBackup $HOME/Downloads/test/backup/localhost | sed '/^ *$/d' > "$HOME/Downloads/test/log/localhost/$timestamp.dryrun"

    read -p "Log for dry run created. Continue? (y/n): " CONTINUE
    case $CONTINUE in
	y) PreviewDryRunLog
	   echo "End."
	   ;;
	n) echo "Not syncing."
	   exit
	   ;;
	*) echo "Invalid selection. Not syncing."
	   exit
	   ;;
    esac
}

DoBackupOfBackup(){
    echo "Beginning backup."
    rsync --relative --update --owner --group --times --links --super $HOME/Downloads/test/backup/localhost/ "$HOME/Downloads/test/history/localhost/$timestamp"
}

DoRealBackup(){
    read -p "Doing the real backup. Are you sure? (y/n): " BACKUP
    case $BACKUP in
	y)  DoBackupOfBackup
	    rsync --relative --recursive --update --delete --owner --group --times --links --safe-links --super --one-file-system --devices $DirToBackup $HOME/Downloads/test/backup/localhost
	   ;;
	n) echo "Aborting."
	   exit
	   ;;
	*) echo "Invalid choice."
	   DoRealBackup
	   ;;
	esac
}

##### Variables #####
DirToBackup=$HOME/Downloads/test/o_files
# DirSource=
# DirToLogs=
timestamp=$(date "+%Y-%m-%d %H:%M:%S")

##### Main #####
install --directory "$HOME/Downloads/test/history/localhost/$timestamp"
install --directory $HOME/Downloads/test/backup/localhost/
install --directory $HOME/Downloads/test/log/localhost/

DoDryRun

DoRealBackup

echo "Finished."
echo
