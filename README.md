# `borscht-hs`

> Borscht is in development, so most of the features below are still forthcoming!

`borscht` is a command line tool for organizing your personal music library.

* Search for missing music metadata from the [Discogs](https://www.discogs.com/my) and [MusicBrainz](https://musicbrainz.org/) databases.
* Write custom [Datalog](https://en.wikipedia.org/wiki/Datalog) inference rules to automatically tag and re-tag your music, and create playlists from saved Datalog queries.

`borscht` takes inspiration from [`beets`](https://beets.io/), with a few key differences:

* `borscht` works natively with both MusicBrainz and Discogs, whereas the Discogs plugin for `beets` is has been somewhat neglected 
* `beets` assumes your music is organized into albums, while `borscht` is happy to work with individual songs
* `borscht` has a powerful tagging system based on datalog
* `borscht` helps you assign an acceptable set of metadata to digital music files, but is not too concerned with preserving the correspondence between your digial and physical libraries
* `borscht` is optimized for my personal use case, and doesn't have all the same bells and whistles as `beets`

## About

I created `borscht` not only to create a music database that suits my use case, but also to practice writing non-trivial applications with Haskell.  Before this project, my experience was mostly small single-file experiments compiled directly with `ghc`.  Since the goal of this project was to learn as much as possible about "getting stuff done" in Haskell, the code is a bit of a mishmash of different styles, abstractions, and language extensions.  During this project, I gained experience with the following topics:

* [x] I learned how to use `cabal` to manage project dependencies
* [x] I use [`req`](https://hackage.haskell.org/package/req) to make HTTP requests, including API authentication
* [x] I use [`aeson`](https://hackage.haskell.org/package/aeson) to parse JSON responses from the [Discogs](https://www.discogs.com/developers) and [MusicBrainz](https://musicbrainz.org/doc/MusicBrainz_API) APIs
* [x] I use [`persistent`](https://www.yesodweb.com/book/persistent) to manage a `sqlite` database for music libraries created with `borscht`
* [x] I use [`mtl`](https://hackage.haskell.org/package/mtl) to manage a monad transformer stack
* [x] I wrote a monadic parser combinator for Datalog inference rules `R(X,Y) :- P(X,Z), P(Z,Y).`
* [ ] I implemented [semi-naive evaluation](http://pages.cs.wisc.edu/~paris/cs838-s16/lecture-notes/lecture8.pdf) for Datalog, which powers the music tagging system.
* [x] I wrote a [foreign function interface](http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html) wrapper for the [Kakasi](http://kakasi.namazu.org/index.html.en) C library, which transliterates Japanese kanji to romaji.
* [x] I merged a [pull request](https://github.com/pjones/byline/pull/20) into the [`Byline`](https://hackage.haskell.org/package/byline) package adding support for vivid ANSI terminal colors, and a [second one](https://github.com/pjones/byline/pull/21) to repair an unlawful semigroup instance.

Here's a list of some tangential topics that I read about while working on `borscht`.  These are all really interesting to me and I hope to work with them directly on future projects.

* [x] I learned about `stm` concurrency in Haskell by reading Peyton-Jones 2007, ["Beautiful Concurrency"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/beautiful.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fstm%2Fbeautiful.pdf), and inspecting the source code for the [`rate-limit`](https://hackage.haskell.org/package/rate-limit) package, which `borscht` uses to respect the rate limit intervals for API requests to the Discogs and MusicBrainz databases
* [x] While reading about Datalog, I learned about the [`datafun`](http://www.rntz.net/datafun/), [`flix`](https://flix.dev/) projects, which extend Datalog with cool features like incrementalization, function abstraction, and lattices.  I hope to add some of these features to my own datalog implementation!  There's also the incremental [`differential-dataflow`](https://github.com/TimelyDataflow/differential-dataflow) project.

## FAQ

**Why the name?**  [Borscht](https://en.wikipedia.org/wiki/Borscht) is a type of soup made from [beets](https://beets.io/)!

**What music do you listen to?** Check out [Tinariwen](https://www.youtube.com/watch?v=PItnw3Z7WgY), [Dakh Daughters](https://www.youtube.com/watch?v=OMiAw7AHJvg), and [Shaolin Afronauts](https://www.youtube.com/watch?v=mVphcpIoTpc).

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