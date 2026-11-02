# TraceR (development version)

# TraceR 26.11.1

## New Features
- Added versioned network-table JSON import/export with RSA-SHA256 signatures and session-local Save/Reset (#41).
- Legacy graph JSON download controls are removed from the app; exported graph helpers remain for compatibility.

# TraceR 26.11.0

## New Features
- Added an editable Table tab backed by one `tracer_table` state (#40).
- Network graphs are now derived from the shared table, with validation feedback for invalid intermediate edits.
- Added a deterministic research workflow example and table-based session persistence.
- Added an Empty Network action.
- Graph JSON downloads remain available below the Network visualization; legacy graph JSON import is temporarily unavailable in the app.

# TraceR 26.10.2

## New Features
- Added conversion from TraceR tables to DiagrammeR-compatible node and edge tables for graph visualization. (#38)

# TraceR 26.10.1

## New Features
- Added a list-based `tracer_table` model and constructor for single-level networks, with five renameable optional columns and structured validation that supports incomplete editing states. (#39)

# TraceR 26.10.0

## Updates
- Extracted Network UI and server logic into a Shiny module to make future extensions easier (#37)
- Updated packaging, Git, and Docker exclusions.

# TraceR 26.08.0

## Updates
- Updated to most recent DataTools version

# TraceR 24.11.0

## Bug Fixes

- fix issue with zoom (#17)

# TraceR 24.09.0

## New Features

- check enviroment for download/import enviroment

# TraceR 24.09.0

## New Features

- option to sign the graph json file with a key pair
- option to download & import signed json
- deployment adjustments for shinyproxy

# TraceR 24.09.0

## Bug Fixes

- fix issue with zoom (#17)
- add logo as icon for the browser tab

# TraceR 24.07.0

## New Features

- option to import a graph from a json file from url, Pandora or file (#16)
- option to download all user inputs together with the graph as zip file (`.tracer`) (#16)
- option to import user inputs together with the graph from a zip file (`.tracer`) (#16)

# TraceR 24.06.1

## New Features

- adds possibility to download and upload graphs as json

# TraceR 24.04.1

## New Features

- add NEWS file
- add Pkgdown documentation workflow
- add R-cmd check workflow


# TraceR 23.11.1

## New Features

- first draft of TraceR

