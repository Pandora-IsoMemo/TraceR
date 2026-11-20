# TraceR

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/pkgdown.yaml)
[![docker-publish](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/docker-publish.yml)
<!-- badges: end -->

An app to create network-like representations. 

### Release notes (Changelog)

See the latest release notes in [NEWS.md](./NEWS.md)

### Documentation

- Documentation home: https://pandora-isomemo.github.io/TraceR/

### Experimental signed downloads

Signed network-table JSON exchange is currently for testing only. Its format
and verification behavior may change; future versions may not be able to verify
or import signed files downloaded today. Do not rely on these files as your only
long-term copy. Keep an unsigned network-table JSON download as well.

Signing uses the configured private key. Importing a signed file requires
successful verification with the public key from the same key pair. A different
key pair cannot verify the signature, so signed files are not currently guaranteed
to work across local and server environments. Public keys are not yet distributed,
and private keys will never be shared. Signature verification remains mandatory
when importing a file that claims to be signed.

## Notes for developers

### Documentation Updates

When adding information to help pages, docstrings, or vignettes, please update documentation locally as follows. The documentation of
the main branch is built automatically via GitHub Actions. Run these commands before opening a PR with doc or vignette changes.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

### Local Docker Container

When testing with a local docker container, please make sure to rebuild the docker image after changes in the R code or dependencies. You can do this from the root of the repository via:

```bash
docker build -t tracer:latest .
```

After that, start the container as usual via:

```bash
docker run -p 3838:3838 tracer:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the container with `CTRL + C` in the terminal.

**Optional:**

Add `-it` for interactive mode, or `--rm` to remove the container after stopping.
