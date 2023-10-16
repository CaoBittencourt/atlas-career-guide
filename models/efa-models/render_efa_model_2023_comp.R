# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'rstudioapi' # Get current file path
  # , 'rmarkdown' # Rmarkdown
  # , 'flexdashboard', 'shiny', 'DT' # Flex Shiny Dashboards
  # , 'bslib', 'shinythemes' # CSS Bootstrap themes
)

# filevate / install CRAN packages
lapply(
  chr_pkg,
  function(pkg){
    if (!require(pkg, character.only = T)) {
      install.packages(pkg)
    }
    
    require(pkg, character.only = T)
  }
)

# - Working directory -----------------------------------------------------
# Set working directory to current file path
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Work space image -------------------------------------------------------------
tryCatch(
  load("./image_file.RData"),
  error = function(e){source("./setup_file.R")}
)

# - Packages ----------------------------------------------------------------
# Activate / install CRAN packages
lapply(
  chr_pkg,
  function(pkg) {
    if (!require(pkg, character.only = T)) {
      install.packages(pkg)
    }
    
    require(pkg, character.only = T)
  }
)

# Activate / install Git packages
Map(
  function(git, profile) {
    if (!require(git, character.only = T)) {
      install_github(
        paste0(profile, "/", git),
        upgrade = F,
        force = T
      )
    }
    
    require(git, character.only = T)
  },
  git = chr_git,
  profile = names(chr_git)
)

rm(chr_pkg, chr_git)

# # [SHINY] -----------------------------------------------------------------
# # - Render Shiny app ------------------------------------------------------
# rmarkdown::run("./shiny_file.Rmd")
