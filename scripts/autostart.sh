#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

#Set your native resolution IF it does not exist in xrandr
#More info in the script
#run $HOME/.xmonad/scripts/set-screen-resolution-in-virtualbox.sh

#Find out your monitor name with xrandr or arandr (save and you get this line)
#xrandr --output VGA-1 --primary --mode 1360x768 --pos 0x0 --rotate normal
#xrandr --output DP2 --primary --mode 1920x1080 --rate 60.00 --output LVDS1 --off &
#xrandr --output LVDS1 --mode 1366x768 --output DP3 --mode 1920x1080 --right-of LVDS1
#xrandr --output HDMI2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off
# xrandr --output HDMI-1 --primary --auto --output eDP-1 --right-of HDMI-1 --auto
# screens=($(xrandr | grep -w connected | awk '{print $1}'))
# xrandr --output ${screens[1]} --primary --auto --output ${screens[0]} --right-of ${screens[1]} --auto --output ${screens[2]} --&
autorandr --change &

####################################### Launch Polybar ##########################################
(sleep 2; run $HOME/.config/polybar/xmonad-launch.sh) &

####################################### change your keyboard if you need it #####################
# fcitx5 -d &
ibus-daemon -rxRd
# setxkbmap -model pc104 -layout us,bg,cn -variant ,phonetic,altgr-pinyin -option grp:alt_shift_toggle &

# disable touchpad
xinput --set-prop "ELAN1200:00 04F3:3090 Touchpad" "Device Enabled" 0
xinput set-prop 'ELAN0774:00 04F3:3244 Touchpad' 'libinput Natural Scrolling Enabled' 1
xinput set-prop 'ELAN0774:00 04F3:3244 Touchpad' 'libinput Tapping Enabled' 1
# xinput set-prop 11 318 1
xinput set-prop 15 344 1
xinput set-prop 15 317 1

#cursor active at boot
xsetroot -cursor_name left_ptr &

####################################### Set wallpaper ##########################################
# feh --randomize --bg-fill --no-xinerama /usr/share/backgrounds/wallpapers/*
# feh --randomize --bg-fill /home/chachi/personal/wallpapers/*
feh --bg-fill -z /usr/share/backgrounds/wallpapers/ &
# /home/chachi/.xmonad/scripts/random_background.sh &

####################################### Starting utility applications at boot time #############
run nm-applet &
run pamac-tray &
run xfce4-power-manager &
numlockx on &
blueberry-tray &
picom --config $HOME/.xmonad/scripts/picom.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &

####################################### Redshift ###############################################
# redshift -l $(curl -s "https://location.services.mozilla.com/v1/geolocate?key=geoclue" | jq -r '"\(.location.lat):\(.location.lng)"') &
# redshift -t 5700:3600 &

thunar --daemon
# run volumeicon &
# run variety &
# emacs --daemon

#starting user applications at boot time
#run caffeine &
#run vivaldi-stable &
#run firefox &
#run thunar &
#run spotify &
#run atom &
#run telegram-desktop &
#run discord &
#run dropbox &
#run insync start &
#run ckb-next -b &
