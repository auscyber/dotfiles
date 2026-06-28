var content = await Deno.readTextFile("Lyrics_PhoebeBridgers.json");
let obj = JSON.parse(content);
for (const elem of obj) {
    console.log(elem)
}
