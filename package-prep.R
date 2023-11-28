# List of required packages
required_packages <- c("rethinking", "ggplot2", "viridis", "dplyr", "readr", "lubridate", "tidyr", "mosaic", "scales", "GGally", "lsr", "corrplot", "GGally", "gridExtra", "reshape2", "MASS")

# Function to install missing packages
install_missing_packages <- function(packages) {
  installed <- rownames(installed.packages())
  for (package in packages) {
    if (!package %in% installed) {
      install.packages(package)
    }
  }
}

# Install any missing packages
install_missing_packages(required_packages)

# Load the packages into the environment
for (package in required_packages) {
  library(package, character.only = TRUE, quietly = TRUE)
}