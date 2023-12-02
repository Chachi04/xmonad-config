#!/bin/sh

# $@ = list of folders to be searched for images

# tested with
# feh version 3.1.3
# Compile-time switches: curl exif help stat64 verscmp xinerama
#
# feh version 3.4.1
# Compile-time switches: curl exif inotify help stat64 verscmp xinerama

# NOTE: feh distiguishes between closing a window and exiting completely.
# the default key for closing a window is x
# the default key for exiting is q
# so if you look at a wallpaper full size, then press 'q', feh will close completely, incl. the thumbnail window.
# you could change this by unsetting the quit action and assigning 'q' to the close action:
# quit
# close q
# (add this to ~/.config/feh/keys)
# that way you have to press 'q' twice to exit completely.

# NOTE 2:
# scrolling up and down works with arrow keys Up/Down.
# the Left/Right arrow keys do not work in an intuitive way.
# you can scroll up and down with keys but will need to click on the desired image.

# get screen resolution
width="$(xwininfo -root | awk '/Width/ {print $2}')"
height="$(xwininfo -root | awk '/Height/ {print $2}')"
# width=800
# height=600
# limit window size to 90% of screen size
width="$(((width/20)*9))"
height="$(((height/10)*9))"

# how many rows and columns of thumbnails?
divider=8
thumbheight=$((height/divider))
thumbwidth=$((width/divider))

# feh adds 5 pixels of background below each thumbnail
fehquirk=5

# it scrolls down 1 row of images at a time
scrollstep=$(( (thumbheight+fehquirk) * 1 ))

# recursive? add -r
# feh -r -t --cache-thumbnails --no-menus --index-info '' --ignore-aspect --stretch \
# -g ${width}x$height --limit-width $width --thumb-width $thumbwidth --thumb-height $thumbheight \
# --scroll-step $scrollstep --action ";feh -Z. --title='%f --- %wx%hpx' -g ${width}x$height %F" "$@"
feh -r -t --cache-thumbnails --no-menus --index-info '' --ignore-aspect --stretch \
-g ${width}x$height --limit-width $width --thumb-width $thumbwidth --thumb-height $thumbheight \
--scroll-step $scrollstep --action ";feh --bg-fill --no-xinerama %F" "$@"
#--bg $HOME/.config/scripts/feh/grey.gif
# feh --randomize --bg-fill --no-xinerama /usr/share/backgrounds/custom/wallpapers/*
