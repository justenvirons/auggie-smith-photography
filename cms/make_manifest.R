# Run this script once from the RStudio console (with working directory set to
# the project root) to generate cms/manifest.json for Posit Connect deployment.
#
#   source("cms/make_manifest.R")

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

rsconnect::writeManifest(
  appDir     = "cms",
  appPrimary = "app.R"
)

message("cms/manifest.json written. Commit it and push before deploying.")
