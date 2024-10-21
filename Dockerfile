FROM inwt/r-shiny:4.4.1

ENV RUNNING_IN_DOCKER=TRUE

ADD . .

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libglpk40 \
    qpdf \
    pandoc \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site \
    && installPackage
    #DiagrammeRsvg

CMD ["Rscript", "-e", "library(TraceR);startApplication(3838)"]
