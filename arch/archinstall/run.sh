#!/bin/bash
sh clean.sh
losetup -fP ../testimage.img
python3 arch-install2.py
