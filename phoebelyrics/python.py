import lyricsgenius
import re
import os

genius = lyricsgenius.Genius(
    "UztaqqrO-cKarYne1d4HGvoDn7S7Nt-y0xdmA6JLwiOjHzk1wObF7jR4rUVRsmCh")

artist = genius.search_artist(
    "Phoebe Bridgers", sort='popularity', include_features=True)
if artist is None:
    exit()

print(artist.id)
songs = []
for song in artist.songs:
    print(song)
    if input("Keep? ") == "y":
        songs.append(song)
        print("keeping")
expr1 = re.compile(r"\[.*", re.MULTILINE)
expr2 = re.compile(r"\n{2,}", re.MULTILINE)

with open("lyricslist", "w+") as f:
    for song in songs:
        text = expr2.sub('\n', expr1.sub('', song.lyrics).lower())
        f.write(text)
        album = song.to_dict()['album']['name']
        for (t, b) in [(text, "cleaned"), (song.lyrics, "complete")]:
            try:
                os.mkdir(f"{b}/{album}")
            except:
                pass
            a = open(f"{b}/{album}/{song.title}", "w+")
            a.write(t)

#    for lyrics in
