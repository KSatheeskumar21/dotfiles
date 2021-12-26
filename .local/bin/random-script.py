#!/usr/bin/env python3

#  _  ______
# | |/ / ___|  # Author: Kishore Satheeskumar
# | ' /\___ \  # Script: random.py
# | . \ ___) | # Description: Little script that generates a random number and then outputs a different commands based on the value
# |_|\_\____/  #


import random
import os

def randomInteger():
    randomInteger.random_number = random.randint(1,4)
    return randomInteger.random_number

randomInteger()

if randomInteger.random_number == 1:
    os.system("pokemon-colorscripts -r")

elif randomInteger.random_number == 2:
    os.system("pipes")

elif randomInteger.random_number == 3:
    os.system("colorscript random")

elif randomInteger.random_number == 4:
    os.system("neofetch | lolcat")
