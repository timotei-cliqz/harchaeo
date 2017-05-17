# harchaeo

## Setup

```sh
$ stack setup
$ stack build
```

## Backend

To run the server:
```sh
$ stack exec backend -- <path to uncompressed export>
```

Query:
```sh
$ curl http://localhost:8000/channel
$ curl http://localhost:8000/channel/general
```

## Generate Elm code

To generate elm code interfacing with the backend:
```sh
$ stack exec codegen
```

# TODO

1. Allow importing from an archive
2. Implement Elm front-end
3. Clean-up and comment code

