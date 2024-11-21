FILES="state.pickle" \
	"library.db"

BeetConfigDir = ~/.config/beets

clean_beets:
	for FILE in $(FILES); do \
		if [ -f $(BeetConfigDir)/$$FILE ]; then \
			rm $(BeetConfigDir)/$$FILE; \
		fi; \
	done

echoguy:
	echo guy

beet_library: clean_beets
	beet import -A .

get_album_art:
	beet fetchart -f
