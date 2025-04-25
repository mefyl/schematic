FROM alpine:3.21.0 AS opam-base

RUN apk add --no-cache autoconf bash curl docker-cli docker-cli-buildx gcc g++ git make npm openssh patch tzdata

# Install the Dune Developer Preview version to be able to use `dune pkg` instead of `opam`
RUN curl -fsSL https://get.dune.build/install | sh
ADD dune-config /root/.config/dune/config
ENV PATH="/root/.local/bin:$PATH"
