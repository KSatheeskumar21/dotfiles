#!/usr/bin/env python

import os

print("""
What would you like to do:
1) Update system
2) Update Doom Emacs
3) Clear pacman cache
4) Clear AUR cache
5) Sync Doom Emacs
6) Clean Doom Emacs
7) Run 'doom doctor'
""")

user_choice = int(input())

if user_choice == 1:
    print("####################")
    print("#Updating system...#")
    print("####################")
    print("\n\n")
    os.system("sudo pacman -Syyu")

if user_choice == 2:
    print("########################")
    print("#Updating Doom Emacs...#")
    print("########################")
    print("\n\n")
    os.system("~/.emacs.d/bin/doom upgrade")

if user_choice == 3:
    print("##########################")
    print("#Clearing System cache...#")
    print("##########################")
    print("\n\n")
    os.system("sudo pacman -Scc")

if user_choice == 4:
    print("########################")
    print("#Clearing AUR cache... #")
    print("########################")
    print("\n\n")
    os.system("paru -Scc")

if user_choice == 5:
    print("########################")
    print("# Syncing Doom Emacs...#")
    print("########################")
    print("\n\n")
    os.system("~/.emacs.d/bin/doom sync -e")

if user_choice == 6:
    print("########################")
    print("#Cleaning Doom Emacs...#")
    print("########################")
    print("\n\n")
    os.system("~/.emacs.d/bin/doom clean")

if user_choice == 7:
    print("##########################")
    print("#Running 'doom doctor'...#")
    print("##########################")
    print("\n\n")
    os.system("~/.emacs.d/bin/doom doctor")

print("\n\n\n Done!")
