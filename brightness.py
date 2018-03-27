#!/usr/bin/env python3


from subprocess import run, PIPE


ICON_BRIGHTNESS = "<fn=1>\uf042</fn>"


command = "brightnessctl -m"
status = run(command.split(), stdout=PIPE)
status = status.stdout.decode("utf-8").strip()

*_, level, _ = status.split(",")


print("%s  %s" % (ICON_BRIGHTNESS, level))
