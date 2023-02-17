# Install necessary packages
r <- getOption("repos")
r["CRAN"] <- "https://cran.rstudio.com/"
options(repos = r)

# List of packages
packages <- c("devtools", "knitr", "rmarkdown", "tidyverse")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

# Install and load Statamarkdown
devtools::install_github("Hemken/Statamarkdown")
library(Statamarkdown)

# Load data
library(haven)
df <- read_dta("replication.data.beheadings.march.15.dta")
