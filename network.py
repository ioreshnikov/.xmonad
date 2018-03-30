#!/usr/bin/env python3


from subprocess import check_output


ICON_WIFI = "<fn=1>\uf1eb</fn>"
TYPE_WIFI = "802-11-wireless"

ICON_VPN = "<fn=1>\uf023</fn>"
TYPE_VPN = "vpn"


command = "nmcli -t connection show --active"
status = check_output(command.split())
status = status.decode("utf-8").strip()

if not status:
    exit()

connections = status.splitlines()
indicators = []

for connection in connections:
    name, uuid, type_, device = connection.split(":")

    if type_ not in [TYPE_WIFI, TYPE_VPN]:
        continue

    if type_ == TYPE_WIFI:
        icon = ICON_WIFI
    if type_ == TYPE_VPN:
        icon = ICON_VPN

    indicator = "%s  %s" % (icon, name)
    indicators.append(indicator)


print("    ".join(indicators))
