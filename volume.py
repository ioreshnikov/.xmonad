#!/usr/bin/env python3


from subprocess import run, PIPE


ICON_VOLUME_OFF = "<fn=1>\uf026</fn>"
ICON_VOLUME_DOWN = "<fn=1>\uf027</fn>"
ICON_VOLUME_UP = "<fn=1>\uf028</fn>"


command = "pamixer --get-volume"
status = run(command.split(), stdout=PIPE)
status = status.stdout.decode("utf-8").strip()

level = int(status)

command = "pamixer --get-mute"
status = run(command.split(), stdout=PIPE)
status = status.stdout.decode("utf-8").strip()

mute = status == "true"


icon = None
if level == 0 or mute:
    icon = ICON_VOLUME_OFF
elif level <= 50:
    icon = ICON_VOLUME_DOWN
else:
    icon = ICON_VOLUME_UP


print("%s  %d%%" % (icon, level))
