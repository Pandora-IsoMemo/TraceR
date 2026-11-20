# TraceR

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/pkgdown.yaml)
[![docker-publish](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/docker-publish.yml)
<!-- badges: end -->

TraceR is a Shiny application and R package for describing a directed network
as an editable table and visualizing it with DiagrammeR. The current app edits
one network level, with ordinary nodes, edges, and capsule rows.

## Get Started

Install the published package and its dependencies from the Pandora repository
and CRAN:

```r
install.packages("TraceR", repos = c(
	PANDORA = "https://Pandora-IsoMemo.github.io/drat/",
	CRAN = "https://cloud.r-project.org"
))
TraceR::startApplication(launch.browser = TRUE)
```

From a development checkout, use `devtools::load_all()` followed by
`startApplication(launch.browser = TRUE)`, as in
[inst/main.R](https://github.com/Pandora-IsoMemo/TraceR/blob/main/inst/main.R).

1. Click **Load Example** to load the research workflow.
2. Edit a cell in **Network Table** by double-clicking it and leaving the cell
	 to commit the change. Add/delete rows or rename optional columns as needed.
3. Open **Network Graph** to inspect the graph derived from the same table.
4. Download unsigned table JSON to retain the current network outside the app.

Validation feedback preserves incomplete edits. IDs must be unique, Type must
be `node`, `edge`, or `capsule`, and edge endpoints refer to node/capsule IDs.
Errors block Save and table JSON exchange. Unresolved references are warnings:
they permit Save and JSON exchange but prevent graph rendering until resolved.

## Save and Exchange Data

**Save is a session-local checkpoint, not permanent storage or a download.**
**Reset** restores that checkpoint after confirmation. Loading or importing a
network changes only the working table and asks before discarding unsaved work.
Closing or restarting the Shiny session loses its checkpoint.

| Option | What it retains |
|--------|-----------------|
| Unsigned table download (`data.json`) | Current rows, values, column names and order, including unsaved edits. |
| Signed table download (`data-signed.json`) | The same table payload plus an experimental RSA-SHA256 signature. |
| Download Session (`.tracer`) | Working table and application inputs in an unsigned DataTools bundle, not the saved checkpoint. |

The table download is labeled **Download** when no private key is loaded, or
**Download JSON** alongside **Download Signed JSON** when a key is available.
Table **Import** and **Import Session** are separate actions. Legacy graph JSON
and graph-only sessions are unsupported by the current app.

### Experimental Signatures

Signed JSON exchange is **for testing only**. Its format and verification behavior
may change; future versions may not verify or import files downloaded today.
Keep an unsigned JSON copy and do not rely on signed files as your only archive.

Signing uses the configured private key. Import requires verification with the
public key from the same key pair. Keys load independently; this does not make
local/server keypairs match. Public keys are not (yet) distributed, and private
keys will never be shared. Signed-download visibility is based on private-key
availability, not user-ID checks. Invalid or unverifiable signatures are rejected.
Signatures do not yet identify authors/downloaders.

## Guides and Reference

- [User guide](https://pandora-isomemo.github.io/TraceR/articles/using-tracer.html): editing, validation, Save/Reset, JSON, signatures, and session workflows.
- [Developer guide](https://pandora-isomemo.github.io/TraceR/articles/architecture.html): module/state ownership, JSON/signature contracts, deployment, tests, and extension boundaries.
- [API reference](https://pandora-isomemo.github.io/TraceR/reference/index.html).
- Guide sources: [vignettes/using-tracer.Rmd](https://github.com/Pandora-IsoMemo/TraceR/blob/main/vignettes/using-tracer.Rmd) and [vignettes/architecture.Rmd](https://github.com/Pandora-IsoMemo/TraceR/blob/main/vignettes/architecture.Rmd).
- [Release notes](https://pandora-isomemo.github.io/TraceR/news/index.html) describe changes over time, not the complete current feature set.

The guides are also standard package vignettes: use `browseVignettes("TraceR")`
when they are included in the installed package. Hierarchical capsules, permanent
storage, templates, revision history, authenticated authorship, and configurable
graph appearance remain future work. Capsules currently have no nested network.

## Development

The application server owns one working `tracer_table` and a separate saved
checkpoint. Network is derived from the working table; the sidebar coordinates
actions and downloads, while the Table module handles editing. See the developer
guide before changing persistence or introducing new network state.

From the checkout, run:

```r
devtools::document()
devtools::build()
devtools::test()
devtools::check()
pkgdown::build_site()
```

The documentation site is built and published through the existing GitHub Actions
workflow. Keep examples independent of production keys, running apps, and remote
data. Treat [inst/scripts/tasks.md](https://github.com/Pandora-IsoMemo/TraceR/blob/main/inst/scripts/tasks.md) as a historical roadmap,
not current app documentation.

For local Docker testing, rebuild after source/dependency changes:

```bash
docker build -t tracer:latest .
docker run --rm -p 3838:3838 tracer:latest
```

Open `http://localhost:3838/`. Do not distribute production private keys in local
images or package artifacts.
