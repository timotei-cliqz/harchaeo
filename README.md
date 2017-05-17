# harchaeo

## Quick start

It will expect the find a directory named `archive` in your current directory
which is the compressed (or uncompressed) archive of a slack export.
```sh
$ make run
```


## Manual setup
### Setup

```sh
$ stack setup
$ stack build
```

### Backend

To run the server:
```sh
$ stack exec backend -- <path compressed export>
```

Query:
```sh
$ curl http://localhost:8000/channel
$ curl http://localhost:8000/channel/general
```

### Generate Elm code

To generate elm code interfacing with the backend:
```sh
$ stack exec codegen
```

# TODO

1. Implement Elm front-end
2. Clean-up and comment code
