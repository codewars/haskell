FROM debian:buster-slim
# Based on https://github.com/haskell/docker-haskell/blob/9f3a7b0038a6ff9feb668a51cfebebdd4301c9c3/9.2/slim-buster/Dockerfile

ENV LANG=C.UTF-8

# common haskell + stack dependencies
RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        dpkg-dev \
        git \
        gcc \
        gnupg \
        g++ \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        libnuma-dev \
        libtinfo-dev \
        make \
        netbase \
        xz-utils \
        zlib1g-dev \
    ; \
    rm -rf /var/lib/apt/lists/*;

ENV STACK=2.7.5 STACK_RELEASE_KEY=C5705533DA4F78D8664B5DC0575159689BEFB442
RUN set -eux; \
    cd /tmp; \
    STACK_URL="https://github.com/commercialhaskell/stack/releases/download/v${STACK}/stack-${STACK}-linux-x86_64.tar.gz"; \
    # sha256 from https://github.com/commercialhaskell/stack/releases/download/v${STACK}/stack-${STACK}-linux-x86_64.tar.gz.sha256
    STACK_SHA256="9bcd165358d4dcafd2b33320d4fe98ce72faaf62300cc9b0fb86a27eb670da50"; \
    curl -sSL "$STACK_URL" -o stack.tar.gz; \
    echo "$STACK_SHA256 stack.tar.gz" | sha256sum --strict --check; \
    \
    curl -sSL "$STACK_URL.asc" -o stack.tar.gz.asc; \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    gpg --batch --keyserver keyserver.ubuntu.com --receive-keys "$STACK_RELEASE_KEY"; \
    gpg --batch --verify stack.tar.gz.asc stack.tar.gz; \
    gpgconf --kill all; \
    \
    tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1 "stack-$STACK-linux-x86_64/stack"; \
    stack config set system-ghc --global true; \
    stack config set install-ghc --global false; \
    \
    rm -rf /tmp/*; \
    \
    stack --version;

ENV CABAL_INSTALL=3.8.1.0 CABAL_INSTALL_RELEASE_KEY=E9EC5616017C3EE26B33468CCE1ED8AE0B011D8C
RUN set -eux; \
    cd /tmp; \
    CABAL_INSTALL_TAR="cabal-install-$CABAL_INSTALL-x86_64-linux-deb10.tar.xz"; \
    CABAL_INSTALL_URL="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL/$CABAL_INSTALL_TAR"; \
    CABAL_INSTALL_SHA256SUMS_URL="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL/SHA256SUMS"; \
    # sha256 from https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL/SHA256SUMS
    CABAL_INSTALL_SHA256='c71a1a46fd42d235bb86be968660815c24950e5da2d1ff4640da025ab520424b'; \
    curl -fSL "$CABAL_INSTALL_URL" -o cabal-install.tar.gz; \
    echo "$CABAL_INSTALL_SHA256 cabal-install.tar.gz" | sha256sum --strict --check; \
    \
    curl -sSLO "$CABAL_INSTALL_SHA256SUMS_URL"; \
    curl -sSLO "$CABAL_INSTALL_SHA256SUMS_URL.sig"; \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    gpg --batch --keyserver keyserver.ubuntu.com --receive-keys "$CABAL_INSTALL_RELEASE_KEY"; \
    gpg --batch --verify SHA256SUMS.sig SHA256SUMS; \
    # confirm we are verifying SHA256SUMS that matches the release + sha256
    grep "$CABAL_INSTALL_SHA256  $CABAL_INSTALL_TAR" SHA256SUMS; \
    gpgconf --kill all; \
    \
    tar -xf cabal-install.tar.gz -C /usr/local/bin; \
    \
    rm -rf /tmp/*; \
    \
    cabal --version;

ENV GHC=9.2.4 GHC_RELEASE_KEY=88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4
RUN set -eux; \
    cd /tmp; \
    GHC_URL="https://downloads.haskell.org/~ghc/$GHC/ghc-$GHC-x86_64-deb10-linux.tar.xz"; \
    # sha256 from https://downloads.haskell.org/~ghc/$GHC/SHA256SUMS
    GHC_SHA256='a77a91a39d9b0167124b7e97648b2b52973ae0978cb259e0d44f0752a75037cb'; \
    curl -sSL "$GHC_URL" -o ghc.tar.xz; \
    echo "$GHC_SHA256 ghc.tar.xz" | sha256sum --strict --check; \
    \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    curl -sSL "$GHC_URL.sig" -o ghc.tar.xz.sig; \
    gpg --batch --keyserver keyserver.ubuntu.com --receive-keys "$GHC_RELEASE_KEY"; \
    gpg --batch --verify ghc.tar.xz.sig ghc.tar.xz; \
    gpgconf --kill all; \
    \
    tar xf ghc.tar.xz; \
    cd "ghc-$GHC"; \
    ./configure --prefix "/opt/ghc/$GHC"; \
    make install; \
    # remove profiling support to save space
    find "/opt/ghc/$GHC/" \( -name "*_p.a" -o -name "*.p_hi" \) -type f -delete; \
    # remove some docs
    rm -rf "/opt/ghc/$GHC/share/"; \
    \
    rm -rf /tmp/*; \
    \
    "/opt/ghc/$GHC/bin/ghc" --version;

ENV PATH=/opt/ghc/${GHC}/bin:$PATH

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
