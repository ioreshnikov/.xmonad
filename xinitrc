# Monitors
xrandr --output HDMI-1 --left-of VGA-1 --primary
xset s off # Disable blanking
xset -dpms # Disable poweroff

# Keyboards configuration
setxkbmap -layout "us, ru"
setxkbmap -option "grp:toggle, grp_led:scroll, ctrl:swapcaps"

# Appearance
compton -C -D 2 &
xsetroot -cursor_name left_ptr
xrdb -all ~/.Xresources &
sh ~/.fehbg &

# Daemons
emacs --daemon &
start-pulseaudio-x11 &
gnome-screensaver &
dropbox start &

exec xmonad
