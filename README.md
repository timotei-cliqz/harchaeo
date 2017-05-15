# harchaeo

To run the server:
```sh
$ stack setup
$ stack build
$ stack exec harchaeo-exe -- <path to uncompressed export>
```

Query:
```sh
$ curl http://127.0.0.1:3000/channel
$ curl http://127.0.0.1:3000/channel/general
```

# TODO

1. Allow importing from an archive
2. Implement Elm front-end
3. Clean-up and comment code

