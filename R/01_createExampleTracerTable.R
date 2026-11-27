#' Create a deterministic example TraceR table
#'
#' Creates a small research workflow with five ordinary nodes, one capsule,
#' and five directed edges. This function returns only the root table.
#' Use [createExampleTracerNetwork()] for the example including the capsule's
#' nested analysis workflow.
#'
#' @return A valid `tracer_table`.
#' @export
#' @examples
#' table <- createExampleTracerTable()
#' validate_tracer_table(table)
createExampleTracerTable <- function() {
  data <- data.frame(
    ID = c("N1", "N2", "N3", "C1", "N4", "N5", "E1", "E2", "E3", "E4", "E5"),
    Label = c(
      "Research question", "Collect data", "Clean data", "Analyze data",
      "Review results", "Publish results", "defines", "prepares",
      "input for", "produces", "approved for"
    ),
    Type = c(rep("node", 3), "capsule", rep("node", 2), rep("edge", 5)),
    From = c(rep("", 6), "N1", "N2", "N3", "C1", "N4"),
    To = c(rep("", 6), "N2", "N3", "C1", "N4", "N5"),
    `Unique identifier` = rep("", 11),
    Author = c(rep("Research team", 6), rep("", 5)),
    Status = c(
      "Completed", "Completed", "In progress", "In progress", "Planned",
      "Planned", rep("", 5)
    ),
    Description = c(
      "Define the study objective", "Gather the required observations",
      "Prepare data for analysis", "Analyze the collected data",
      "Check and approve the findings", "Share the final results",
      rep("", 5)
    ),
    `Optional 4` = rep("", 11),
    `Optional 5` = rep("", 11),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  new_tracer_table(data)
}

#' Create a deterministic example TraceR network
#'
#' Extends [createExampleTracerTable()] with a small workflow inside the
#' Analyze data capsule (`C1`): summarize data, fit a model, and evaluate it.
#' IDs and edge references are local to each table.
#'
#' @return A valid `tracer_network`.
#' @export
#' @examples
#' network <- createExampleTracerNetwork()
#' get_tracer_table(network, "C1")
createExampleTracerNetwork <- function() {
  data <- data.frame(
    ID = c("N1", "N2", "N3", "E1", "E2"),
    Label = c("Summarize data", "Fit model", "Evaluate model", "informs", "assessed by"),
    Type = c(rep("node", 3), rep("edge", 2)),
    From = c(rep("", 3), "N1", "N2"),
    To = c(rep("", 3), "N2", "N3"),
    `Unique identifier` = rep("", 5),
    Author = c(rep("Research team", 3), rep("", 2)),
    Status = c("Completed", "In progress", "Planned", "", ""),
    Description = c(
      "Explore distributions and summarize observations",
      "Estimate model parameters from the prepared data",
      "Check model fit and interpret the results",
      "", ""
    ),
    `Optional 4` = rep("", 5),
    `Optional 5` = rep("", 5),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  new_tracer_network(
    createExampleTracerTable(),
    tables = list(C1 = new_tracer_table(data))
  )
}