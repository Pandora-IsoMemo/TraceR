#' Config
#'
#' @export
#' @return (list) configuration parameters for import of data and models
config <- function() {
  config_path <- system.file("config.yaml", package = "TraceR")
  read_yaml(config_path)
}
