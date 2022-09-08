FROM debian:10
# Based on https://github.com/haskell/docker-haskell/blob/fb5c774c38a7ab2e7d233d95368be0f899353496/8.8/buster/Dockerfile
# LTS Haskell 16.25 (2020-12-06) GHC 8.8.4
# See https://www.stackage.org/lts-16.25 for package versions

ENV LANG=C.UTF-8

RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends gnupg ca-certificates dirmngr; \
    rm -rf /var/lib/apt/lists/*;

ENV GHC=8.8.4 CABAL_INSTALL=3.2 DEBIAN_KEY=427CB69AAC9D00F2A43CAF1CBA3CBA3FFE22B574 LTS_HASKELL=16.25
RUN set -ex; \
    export GNUPGHOME="$(mktemp -d)"; \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys ${DEBIAN_KEY}; \
    gpg --batch --armor --export ${DEBIAN_KEY} > /etc/apt/trusted.gpg.d/haskell.org.gpg.asc; \
    gpgconf --kill all; \
    echo 'deb http://downloads.haskell.org/debian buster main' > /etc/apt/sources.list.d/ghc.list; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        cabal-install-${CABAL_INSTALL} \
        curl \
        g++ \
        ghc-${GHC} \
        git \
        libsqlite3-dev \
        libtinfo-dev \
        make \
        netbase \
        openssh-client \
        xz-utils \
        zlib1g-dev \
# regex-pcre requires pcre
        libpcre3 \
        libpcre3-dev \
    ; \
    rm -rf "$GNUPGHOME" /var/lib/apt/lists/* /tmp/* /var/tmp/*;

ENV STACK=2.5.1 STACK_RELEASE_KEY=C5705533DA4F78D8664B5DC0575159689BEFB442
RUN set -ex; \
    export GNUPGHOME="$(mktemp -d)"; \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys ${STACK_RELEASE_KEY}; \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v${STACK}/stack-${STACK}-linux-x86_64.tar.gz -o stack.tar.gz; \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v${STACK}/stack-${STACK}-linux-x86_64.tar.gz.asc -o stack.tar.gz.asc; \
    gpg --batch --verify stack.tar.gz.asc stack.tar.gz; \
    gpgconf --kill all; \
    tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1; \
    stack config set system-ghc --global true; \
    stack config set install-ghc --global false; \
    rm -rf "$GNUPGHOME" /var/lib/apt/lists/* /stack.tar.gz.asc /stack.tar.gz;

ENV PATH=/opt/cabal/${CABAL_INSTALL}/bin:/opt/ghc/${GHC}/bin:$PATH


RUN set -ex; \
    useradd --create-home codewarrior; \
    mkdir -p /workspace; \
    chown codewarrior: /workspace;

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
ENV PATH=/home/codewarrior/.cabal/bin:/home/codewarrior/.local/bin:$PATH

COPY --chown=codewarrior:codewarrior workspace/ /workspace/
WORKDIR /workspace
RUN stack build
# Sanity check
RUN set -ex; \
    stack ghc -- -O -isrc:test --make -o tests -outputdir /tmp test/Main.hs && ./tests; \
    rm src/Example.hs test/ExampleSpec.hs tests /tmp/*;
