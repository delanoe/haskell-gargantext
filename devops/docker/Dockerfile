FROM fpco/stack-build:lts-17.13

RUN apt-key adv --keyserver hkp://pool.sks-keyservers.net:80 --recv-keys 8B1DA6120C2BF624
RUN apt-get update && \
    apt-get install -y git libigraph0-dev && \
    rm -rf /var/lib/apt/lists/*

