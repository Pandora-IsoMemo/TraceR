# TraceR

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/pkgdown.yaml)
[![docker-publish](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/Pandora-IsoMemo/TraceR/actions/workflows/docker-publish.yml)
<!-- badges: end -->

TraceR is a Shiny application and R package for describing a directed network
as editable tables and visualizing them with DiagrammeR. A network can contain
hierarchically nested capsules. The app displays and edits one selected level
at a time; each level contains node, edge, and capsule rows, and its graph is
derived from the same table.

## Get Started

For installation instructions, see the
[TraceR page in the Pandora applications guide](https://pandora-isomemo.github.io/docs/apps.html#tracer).

From a development checkout, use `devtools::load_all()` followed by
`startApplication(launch.browser = TRUE)`, as in
[inst/main.R](https://github.com/Pandora-IsoMemo/TraceR/blob/main/inst/main.R).

1. Click **Load Example** to load the research workflow.
2. Edit a cell in **Network Table** by double-clicking it and leaving the cell
   to commit the change. Add/delete rows or rename optional columns as needed.
3. Use the **Network Tree** to select a capsule and view its table or graph.
   The sidebar also lets you add, rename, delete, or collapse the selected
   capsule.
4. Open **Network Graph** to inspect the graph derived from the selected table.
5. Use **Download Network** to retain the complete hierarchy as a ZIP archive.

Validation feedback preserves incomplete edits. IDs must be unique within each
level, Type must be `node`, `edge`, or `capsule`, and edge endpoints refer to
node/capsule IDs in that same level. Errors block Save and table JSON exchange.
Unresolved references are warnings: they permit Save and table JSON exchange but
prevent graph rendering until resolved. Capsule collapsing performs additional
preflight checks and reports blocking issues without partially changing the
network.

## Save and Exchange Data

**Save is a session-local checkpoint, not permanent storage or a download.**
**Reset** restores that checkpoint after confirmation. Save and Reset apply to
the selected level and its structural changes, including capsule operations.
Loading or importing a replacement network asks before discarding unsaved work.
Closing or restarting the Shiny session loses its checkpoint.

| Option | What it retains |
|--------|-----------------|
| Network ZIP (`network.zip`) | The complete working network, including all capsule tables and hierarchy. |
| Unsigned table download (`data.json`) | The currently selected table, including unsaved edits; it does not include descendant tables. |
| Signed table download (`data-signed.json`) | The same selected-table payload plus an experimental RSA-SHA256 signature. |
| Download Session (`.tracer`) | The complete working network, selected capsule path, and application inputs in an unsigned DataTools bundle; it does not include the saved checkpoint. |

The Network ZIP controls are in the sidebar. **Import Network** replaces the
working hierarchy after confirmation when necessary; **Import ZIP as capsule**
adds an imported hierarchy beneath a new capsule at the selected level.

The table download is labeled **Download** when no private key is loaded, or
**Download JSON** alongside **Download Signed JSON** when a key is available.
Table **Import** and **Import Session** are separate actions. Table JSON imports
restore only that table as the root level, with empty child tables for any
capsule rows. Session imports restore the full working hierarchy and
application inputs; they restore the selected path when it has a checkpoint
mapping, otherwise they select the nearest checkpointable ancestor (normally
Root). The saved checkpoint is unchanged. Legacy graph JSON, graph-only
sessions, and older table-only session bundles are unsupported.

### Capsule operations

The tree navigates the network hierarchy. **Add into current**, **Rename
current**, and **Delete current capsule** manage the selected capsule. Deleting
a capsule removes its subtree but does not rewrite parent-level edges that refer
to it; those references become validation warnings.

**Collapse current capsule** promotes its immediate contents one level, prefixes
promoted IDs with the capsule ID, updates local edge references and descendant
paths, then removes the capsule. It does not recursively collapse nested
capsules. Parent-level connections are redirected to internal starting or end
elements; empty capsules are treated as pass-through elements. The capsule row
and its metadata are removed. A persistent report summarizes the changes and
lists every generated, updated, or removed edge; the report is capped at 320px
and longer reports can be scrolled.
Blocked operations leave the network unchanged. The resulting changes remain
unsaved until Save.

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

- [User guide](https://pandora-isomemo.github.io/TraceR/articles/using-tracer.html): editing, capsule navigation/collapse, validation, Save/Reset, ZIP, JSON, signatures, and session workflows.
- [Developer guide](https://pandora-isomemo.github.io/TraceR/articles/architecture.html): network/state ownership, capsule/ZIP operations, JSON/signature contracts, deployment, tests, and extension boundaries.
- [API reference](https://pandora-isomemo.github.io/TraceR/reference/index.html).
- Guide sources: [vignettes/using-tracer.Rmd](https://github.com/Pandora-IsoMemo/TraceR/blob/main/vignettes/using-tracer.Rmd) and [vignettes/architecture.Rmd](https://github.com/Pandora-IsoMemo/TraceR/blob/main/vignettes/architecture.Rmd).
- [Release notes](https://pandora-isomemo.github.io/TraceR/news/index.html) describe changes over time, not the complete current feature set.

The guides are also standard package vignettes: use `browseVignettes("TraceR")`
when they are included in the installed package. Hierarchical edge references,
cross-level edges, stable internal IDs independent of user IDs, permanent
storage, templates, revision history, authenticated authorship, and configurable
graph appearance remain future work. Networks are directed but are not required
to be acyclic.

## Development

The application server owns working and saved `tracer_network` values, a
working-to-saved capsule path map, the selected path, and the selected
`tracer_table`. The Table module edits that selected table; the Network module
derives its graph; and the sidebar coordinates navigation and capsule actions.
See the developer guide before changing persistence or network state.

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
