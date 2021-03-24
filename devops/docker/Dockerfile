from fpco/stack-build:lts-16.26

RUN apt-get update && \
    apt-get install -y git libigraph0-dev && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -v /deps && \
    cd /deps && \
    git clone https://gitlab.iscpif.fr/gargantext/clustering-louvain-cplusplus && \
    cd clustering-louvain-cplusplus && \
    ./install
