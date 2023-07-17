# Development

### 1. Install the system dependencies

```sh
brew install fswatch llvm

sudo mkdir -p /opt/local/lib
```

### 2. Install your dev environment
```sh
# This will take a few minutes...
opam switch create . ocaml-variants.5.0.0+options --no-install
opam install . --deps-only -t
```

### 3. Setup VS Code
While the above is compiling, install the `OCaml Platform` VS Code extension.
Add this block to your VS Code's `settings.json` file:
```json
"[ocaml]": {
    "editor.formatOnSave": true,
},
"files.associations": {
    ".ocamlinit": "ocaml",
    "*.ml": "ocaml",
    "*.mli": "ocaml",
    "dune": "dune"
},
```
Then restart your terminal window and (fully) restart VS Code.

### 4. Building and running depcheck

#### MacOS development
```sh
# Build:
dune build -w

# Build and execute:
dune exec src/app/depcheck.exe arguments...

# Release:
# Don't forget to update the version number in [depcheck.ml]!
dune clean \
&& DUNE_PROFILE=release dune build \
&& rm -f depcheck.mac \
&& cp _build/default/src/app/depcheck.exe depcheck.mac \
&& chmod +x depcheck.mac
```

#### Linux/Docker
```sh
# Don't forget to update the version number in [depcheck.ml]!
docker build . -t depcheck:latest

DEPCHECK_CID="$(docker create depcheck:latest)" \
&& rm -f depcheck.linux \
&& docker cp "$DEPCHECK_CID":/app/depcheck.exe depcheck.linux \
&& docker rm "$DEPCHECK_CID"

# Trying it on Ubuntu 22.04
docker run -it --rm \
  -v "$(pwd):/app" \
  -v "$(realpath "$(pwd)/../francis-tuttle"):/repo" \
  -w /repo \
  ubuntu:22.04
  # Then run:
  ## apt-get update && apt-get install -y musl npm
  ## /app/depcheck.linux /repo
```
