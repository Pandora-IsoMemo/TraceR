# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(TraceR)

test_check("TraceR")

# Note when working with vscode:
# If running the tests localy fails (the interactive R terminal gets stuck), try running them in
# a fresh, non-interactive R process (bash terminal):
# Rscript -e 'devtools::test()'
