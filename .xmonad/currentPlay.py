#!/usr/bin/env python
import sys
import re
import subprocess
import time
songs = []
songs.append("")
songs.append("")
songs.append("")


def mediaa():
    try:
        global songs
        players = subprocess.run(["playerctl", "-l"], capture_output=True).stdout.decode('utf-8')
        status = subprocess.run(["playerctl", "status"], capture_output=True).stdout.decode('utf-8')
        if players.__contains__("spotify"):
            metadata = subprocess.run(["playerctl", "metadata"], capture_output=True).stdout.decode('utf-8')
            artist = re.search(r"spotify xesam:artist\s+(.+)", metadata).group(1)
            track = re.search(r"spotify xesam:title\s+(.+)", metadata).group(1)
            current_song = track + " - " + artist
            o = "" #pw_sym("DBD89e")
           #if songs[len(songs)-1] != current_song:
           #    if songs[len(songs)-2] == current_song:
           #        o += " "
           #        songs.pop(len(songs)-1)
           #    else:
           #        o+= " "
           #        songs.append(current_song)
           #elif status == "Playing\n":
           #    o += " "
           #else:
           #    o += " "
            o += " "
            o += current_song
            
            if len(songs) > 100:
                songs.pop(0)
                
            return o
    except:
        return "AusCyber"


while True:
     try:
        sys.stdout.write(" " + mediaa()+"\n")
     except:
         sys.stdout.write(" \n")
     sys.stdout.flush()
     time.sleep(1)
