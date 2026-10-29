# TraceR

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end --

An app to create network-like representations. 

## Notes for developers

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
