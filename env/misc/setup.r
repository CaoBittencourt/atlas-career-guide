# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# install modular if not installed
if (!any(utils::installed.packages()[, 1] == "modular")) {
  devtools::install_github("CaoBittencourt/modular")
}

library(modular)
library(stringr)

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
    # questionnaires
    atlas.quest = "database/quest",
    # rds
    atlas.rds = "database/data/output/rds",
    atlas.skills_mtx = "database/output/rds/skill_set_mtx.rds",
    atlas.skills = "database/output/rds/skill_sets.rds",
    atlas.labor = "database/output/rds/labor.rds",
    atlas.education = "database/output/rds/education.rds",
    atlas.cao = "database/quest/rds/cao.rds"
  ),
  root.name = ".atlas",
  start.path = ".",
  end.path = "."
)

# # environment vars
# if(list.files(pattern = '*.env$') |> length()){
#   list.files(pattern = '*.env$') |>
#     lapply(function(file){
#       file |>
#     })
#     Sys.setenv()
# }

# list.files(pattern = '*.env$') |>
#   readLines() |>
#   str_subset('^#', T) |>
#   str_subset('^$', T) |>
#   str_replace_all(' = ', '=') |>
#   str_split('=', n = 2, T) ->
# vars

# do.call(
#   Sys.getenv,
#   args = list(
#     vars[,2] |> stats::setNames(vars[,1])
#   )
# )

#   Sys.getenv('ATLAS.SRC')
