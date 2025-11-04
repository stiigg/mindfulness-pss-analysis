FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libxml2-dev libssl-dev libcurl4-openssl-dev pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work
COPY . /work
RUN Rscript R/install_packages.R

CMD ["Rscript", "R/analysis.R"]
