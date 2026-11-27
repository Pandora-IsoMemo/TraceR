FROM inwt/r-shiny:4.4.3

ENV RUNNING_IN_DOCKER=TRUE

RUN Rscript -e "remotes::install_github('r-lib/httr2@v1.2.3')" \
    && Rscript -e "remotes::install_github('tidyverse/ellmer@v0.4.1')"

ADD . .

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libglpk40 \
    qpdf \
    pandoc \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/*

# The zip install line below can be removed once inwt/r-shiny is upgraded to a version shipping zip >= 2.3.3
RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site \
    && Rscript -e "remotes::install_version('zip', version = '2.3.3', repos = 'https://cloud.r-project.org', upgrade = 'never'); stopifnot(packageVersion('zip') == '2.3.3')" \
    && installPackage
    #DiagrammeRsvg

CMD ["Rscript", "-e", "library(TraceR);startApplication(3838)"]
