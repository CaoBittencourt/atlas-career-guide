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
project.options(
  project.name = "atlas",
  relative.paths = list(
    # source code
    atlas.src = "src",
    box.path = "src",
    atlas.mod = "src/mod",
    # database
    atlas.db = "database",
    atlas.data = "database/data",
    atlas.oldata = "database/data/old/occupations/df_occupations_2022.csv",
    # rds
    atlas.rds = "database/data/output/rds",
    atlas.skills_mtx = "database/output/rds/skill_set_mtx.rds",
    atlas.skills = "database/output/rds/skill_sets.rds",
    atlas.labor = "database/output/rds/labor.rds",
    atlas.education = "database/output/rds/education.rds"
  ),
  root.name = ".atlas",
  start.path = ".",
  end.path = "."
)

