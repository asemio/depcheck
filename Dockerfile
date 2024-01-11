FROM asemio/mountain-caravan:2.3.0
WORKDIR /app
RUN sudo apk update \
  && sudo apk upgrade \
  && sudo apk add --no-cache perl cmake npm xz patchelf

COPY depcheck.opam .

ENV DUNE_PROFILE release

RUN opam update \
  && OPAMYES=1 opam install . --deps-only

COPY src src
COPY dune dune
COPY dune-project dune-project

RUN sudo chown -R opam /app

RUN echo '=== Building ===' \
  && opam exec -- dune build src/app/depcheck.exe \
  && cp /app/_build/default/src/app/depcheck.exe . \
  && chmod 755 depcheck.exe \
  && strip depcheck.exe
