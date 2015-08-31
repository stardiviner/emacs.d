FROM maxexcloo/arch-linux
MAINTAINER stardiviner numbchild@gmail.com

# add more steps for preparing.

# Emacs dependence

# install Emacs
RUN pacman -Syu
RUN pacman -S emacs

git clone git@github.com:stardiviner/emacs.d.git ~/.emacs.d/

# run Emacs
export LC_CTYPE=zh_CN.UTF-8 && emacs
