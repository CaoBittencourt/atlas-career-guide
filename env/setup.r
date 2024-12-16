# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# install modular if not installed
if (!any(utils::installed.packages()[, 1] == "modular")) {
  devtools::install_github("CaoBittencourt/modular")
}

library(modular)

# objective project root
setwd(file.path(getwd(), ".."))

project.options(
  project.name = "atlas",
  relative.paths = list(
    atlas.src = "src",
    box.path = "src",
    atlas.mod = "src/mod",
    atlas.data = "data",
    atlas.occupations = "data/occupations/df_occupations_2022.csv"
  ),
  root.name = ".atlas",
  start.path = ".",
  end.path = "."
)
