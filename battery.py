#!/usr/bin/env python3


import psutil


ICON_EMPTY = "<fn=1>\uf244</fn>"
ICON_QUARTER = "<fn=1>\uf243</fn>"
ICON_HALF = "<fn=1>\uf242</fn>"
ICON_THREE_QUARTERS = "<fn=1>\uf241</fn>"
ICON_FULL = "<fn=1>\uf240</fn>"


battery = psutil.sensors_battery()


icon = None
percent = battery.percent
secsleft = battery.secsleft


minutes = secsleft // 60
hours, minutes = divmod(minutes, 60)

if percent < 5:
    icon = ICON_EMPTY
    icon="<fc=#a22921>%s</fc>" % icon
elif percent < 35:
    icon = ICON_QUARTER
elif percent < 60:
    icon = ICON_HALF
elif percent < 85:
    icon = ICON_THREE_QUARTERS
else:
    icon = ICON_FULL


print("%s  %d%%  %02d:%02d" % (icon, percent, hours, minutes))
