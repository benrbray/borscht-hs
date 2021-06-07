# `borscht-hs`

> Borscht is in development, so most of the features below are still forthcoming!

`borscht` is a command line tool for organizing your personal music library.

* Search for missing music metadata from the [Discogs](https://www.discogs.com/my) and [MusicBrainz](https://musicbrainz.org/) databases.
* Write custom [Datalog](https://en.wikipedia.org/wiki/Datalog) inference rules to automatically tag and re-tag your music, and create playlists from saved Datalog queries.

`borscht` takes inspiration from [`beets`](https://beets.io/), with a few key differences:

* `borscht` works natively with both MusicBrainz and Discogs, whereas the Discogs plugin for `beets` is has been somewhat neglected 
* `beets` assumes your music is organized into albums, while `borscht` is happy to work with individual songs
* `borscht` has a powerful tagging system based on datalog
* `borscht` is optimized for my personal use case, and doesn't have all the same bells and whistles as `beets`

## FAQ

**Why the name?**  [Borscht](https://en.wikipedia.org/wiki/Borscht) is a type of soup made from [beets](https://beets.io/)!

## Development

### Dependency:  `text-icu`

```
sudo apt install libicu-dev
```

### Dependency:  `taglib`

Borscht uses [htaglib](https://hackage.haskell.org/package/htaglib), which requires that we install [TagLib](https://taglib.org/) separately.  There's not much documentation, but the following worked for me on Ubuntu 20.04.

```
sudo apt install libtagc0-dev
```

### Dependency:  `kakasi`

The ICU library [cannot](https://sourceforge.net/p/icu/mailman/message/27144335/) transliterate Japanese kanji characters.  Instead, Borscht relies on [`kakasi`](http://kakasi.namazu.org/index.html.en), which is not commonly avilable in package managers.  First, download the `kakasi` source, then

```
$ ./config
$ make
$ make install
```

After installing, the necessary header and static library files should be found in `/usr/local`.  For a local install, use `./config --prefix=$(pwd)/build` before running `make`.

```
./usr/local/include/libkakasi.h
./usr/local/lib/libkakasi.a
```

Cabal statically links the `kakasi` library from these locations.