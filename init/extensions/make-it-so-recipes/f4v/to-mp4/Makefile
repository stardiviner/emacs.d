# ——— parameters ———————————————————————————————————————————————————————————————

# ——— implementation ———————————————————————————————————————————————————————————
DIRF4V = $(shell dir *.f4v)
DIRMP4 = $(DIRF4V:.f4v=.mp4)

all: clean Makefile $(DIRMP4)

%.mp3: %.f4v
		ffmpeg -i "$<" "$@"
		echo "$@" >> provide

clean:
		rm -f *.mp4 provide

install-tools:
		sudo apt-get install ffmpeg

.PHONY: all install-tools clean
