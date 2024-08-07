FROM inwt/r-shiny:4.2.3

ADD . .

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    qpdf \
    pandoc \
    && echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site \
    && installPackage

# Copy the secret .pem files
COPY private_key.pem /inst/app/private_key.pem
COPY public_key.pem /inst/app/public_key.pem

CMD ["Rscript", "-e", "library(TraceR);startApplication(3838)"]
