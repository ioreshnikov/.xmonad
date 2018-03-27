#!/usr/bin/env python3


from subprocess import check_output


ICON_WIFI = "<fn=1>\uf1eb</fn>"
TYPE_WIFI = "802-11-wireless"


command = "nmcli -t connection show --active"
status = check_output(command.split())
status = status.decode("utf-8").strip()

if not status:
    exit()

name, uuid, type_, device = status.split(":")

if type_ == TYPE_WIFI:
    print("%s  %s" % (ICON_WIFI, name))
