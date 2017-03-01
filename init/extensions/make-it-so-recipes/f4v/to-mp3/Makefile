# ——— parameters ———————————————————————————————————————————————————————————————

# ——— implementation ———————————————————————————————————————————————————————————
DIRF4V = $(shell dir *.f4v)
DIRMP3 = $(DIRF4V:.f4v=.mp3)

all: clean Makefile $(DIRMP3)

%.mp3: %.f4v
		ffmpeg -i "$<" "$@"
		echo "$@" >> provide

clean:
		rm -f *.mp3 provide

install-tools:
		sudo apt-get install ffmpeg

.PHONY: all install-tools clean
