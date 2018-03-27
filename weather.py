#!/usr/bin/env python3


import requests


appid = "a15f6536c16ce0acaf32a8471656edaa"
location = "Saint Petersburg,RU"


url = (
    "http://api.openweathermap.org"
    "/data/2.5/weather"
    "?q={}&appid={}"
    .format(location, appid))


response = requests.get(url)
print(response.json())
