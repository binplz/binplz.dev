![binplz.dev](./binplz.dev.png)

------------

[binplz.dev][binplz.dev] provides easily `curl`-able statically-linked Linux binaries for
many popular packages and tools.

For instance, if you're in a pinch and you need the `tree` binary, you can
get it from [binplz.dev][binplz.dev] like the following:

```console
$ curl binplz.dev/tree > tree
$ chmod +x ./tree
```

You can then use this `tree` binary like normal:

```console
$ ./tree --version
tree v1.8.0 (c) 1996 - 2018 by Steve Baker, ...
```

You can confirm that this `tree` binary is statically linked:

```console
$ file ./tree
./tree: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, not stripped
```

The `statically linked` part of the output of `file` signifies that this binary
is statically linked.

## Demo

(TODO: create an asciinema for binplz.dev)

<script src="https://asciinema.org/a/262583.js" id="asciicast-262583" async data-autoplay="true" data-loop="true"></script>

## Quick start

TODO

### Who made this?

[binplz.dev][binplz.dev] was written by:

- [Jonas Carpay](https://jonascarpay.com) ([@jonascarpay](https://github.com/jonascarpay))
- Hideaki Kawai ([@kayhide](https://github.com/kayhide))
- [Dennis Gosnell](https://functor.tokyo) ([@cdepillabout](https://github.com/cdepillabout))

[binplz.dev]: https://binplz.dev
