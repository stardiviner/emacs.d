FROM maxexcloo/arch-linux
MAINTAINER stardiviner numbchild@gmail.com

# add more steps for preparing.

# Emacs dependence

# install Emacs and extension requirements
RUN pacman -Syu --noconfirm && \
  pacman -S --noconfirm emacs \
  sbcl guile racket clojure rlwrap \
  ruby python php perl \
  gcc make automake autoconf cmake \
  go \
  nodejs typescript ts-node \
  julia r \
  gnuplot minted \
  poppler poppler-data poppler-glib poppler-qt5 \
  pygmentize \
  mu

RUN git clone git@github.com:stardiviner/emacs.d.git ~/.config/emacs/

# run Emacs
CMD ["export LC_CTYPE=zh_CN.UTF-8 && emacs"]
