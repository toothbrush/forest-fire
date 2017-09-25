FROM fpco/stack-build:lts-9.5 as builder
COPY forest-fire.cabal stack.yaml /
RUN stack --system-ghc --resolver=lts-9.5 --local-bin-path=/usr/local/bin setup
RUN stack --system-ghc --resolver=lts-9.5 --local-bin-path=/usr/local/bin build --only-dependencies
COPY . /
RUN stack --system-ghc --resolver=lts-9.5 --local-bin-path=/usr/local/bin build
RUN stack --system-ghc --resolver=lts-9.5 --local-bin-path=/usr/local/bin test

FROM debian:stretch
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
    python3-pip python3-setuptools libgmp10
RUN pip3 install --upgrade pip
RUN pip3 install wheel
RUN pip3 install --upgrade awscli

COPY --from=builder /.stack-work/install/x86_64-linux/lts-9.5/8.0.2/bin/forest-fire /
ENTRYPOINT ["/forest-fire"]
