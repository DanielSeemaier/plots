FROM rocker/r-ver:4.4.1

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Berlin

RUN apt-get update -qq \
 && apt-get install -y --no-install-recommends --no-install-suggests \
    libssl-dev \
    libfontconfig1-dev \
    libcurl4-openssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libxml2-dev \
    texlive-full \
    cmake \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /work
