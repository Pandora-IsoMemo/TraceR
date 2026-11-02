#' Create a deterministic example TraceR table
#'
#' Creates a small research workflow with five ordinary nodes, one capsule,
#' and five directed edges. The capsule is a single row and has no nested
#' network.
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