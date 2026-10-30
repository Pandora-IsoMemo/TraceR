library(TraceR)

shinyServer(function(input, output, session) {
  # Adjust Path to environment
  if (!is.na(Sys.getenv("RUNNING_IN_DOCKER", unset = NA))) {
    path_private <- "/app/inst/app/private_key.pem"
    path_public <- "/app/inst/app/public_key.pem"
  } else {
    path_private <- system.file("app/private_key.pem", package = "TraceR")
    path_public <- system.file("app/public_key.pem", package = "TraceR")
  }

  # Check if key files exists
  if (file.exists(path_private) && file.exists(path_public)) {
    private_key <- openssl::read_key(path_private)
    public_key <- openssl::read_pubkey(path_public)
  } else {
    private_key <- NULL
    public_key <- NULL
  }

  mod_network_server(
    "network",
    private_key = private_key,
    public_key = public_key
  )
})
