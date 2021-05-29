# `borscht-hs`

### Why the name?

Borscht is a type of soup made from [beets](https://beets.io/)!

## Development

### Dependency:  `text-icu`

```
sudo apt install libicu-dev
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