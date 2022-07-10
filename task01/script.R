r <- getOption("repos")
r["CRAN"] <- "https://cran.rstudio.com/"
options(repos = r)

# List of packages
packages <- c("devtools", "eha",
              "knitr", "rmarkdown", "stargazer", "SurvRegCensCov", "tidyverse")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

# Install and load Statamarkdown
devtools::install_github("Hemken/Statamarkdown")
library(Statamarkdown)

df <- haven::read_dta("./df.dta")

m1 <- WeibullReg(Surv(as.numeric((curyear-origyear)/30.41667), warend) ~ factor(technologyrebellion), data = df)

m1



sort id year
gen warend=0
replace warend=1 if year==yrend
