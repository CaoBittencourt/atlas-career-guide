# region: modules
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
    atlas.src = "src",
    atlas.mod = "src/mod",
    box.path = "src",
    atlas.data = "data",
    atlas.occupations = "/data/occupations/df_occupations_2022.csv"
  ),
  root.name = ".atlas"
)

# endregion
# region: imports
box::use(
  ast = mod / describe,
  util = mod / utils
)

# endregion
# region: test attribute equivalence
# equivalent attributes form a valid skill set
runif(1) |>
  rep(120) |>
  ast$eq$aeq() |>
  util$assert$valid_skill_set() |>
  try() |>
  class() !=
  "try-error"

# invalid skill sets attribute equivalence
c(rep(0, 119), 1.1) |>
  ast$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), -0.1) |>
  ast$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), Inf) |>
  ast$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), "dsds") |>
  ast$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

# endregion
# region: test generality
# 1 generality
runif(1) |> rep(120) -> skills
all(
  ast$gn$gene(skills) == 1,
  ast$gn$gene(skills) |> length() == 1
)
rm(skills)

# # 0 generality
# all(
#   ast$gn$gene(c(rep(0, 120), 1)) == 0,
#   ast$gn$gene(rep(1, 120)) |> length() == 1
# )

# NA generality
all(
  ast$gn$gene(rep(0, 120)) |> is.na(),
  ast$gn$gene(rep(0, 120)) |> length() == 1
)

# invalid skill sets generality
c(rep(0, 119), 1.1) |>
  ast$gn$gene() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), -0.1) |>
  ast$gn$gene() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), Inf) |>
  ast$gn$gene() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), "dsds") |>
  ast$gn$gene() |>
  try() |>
  class() ==
  "try-error"

# endregion
# region: test competence
# whereas generality of repeated skill set is 1, competence is not
runif(1) |> rep(120) -> skills
all(
  ast$cp$comp(skills) != 1,
  ast$cp$comp(skills) |> length() == 1
)
rm(skills)

# 1 competence
all(
  ast$cp$comp(rep(1, 120)) == 1,
  ast$cp$comp(rep(1, 120)) |> length() == 1
)

# 0 competence
all(
  ast$cp$comp(c(rep(0, 120))) == 0,
  ast$cp$comp(rep(1, 120)) |> length() == 1
)

# invalid skill sets competence
c(rep(0, 119), 1.1) |>
  ast$cp$comp() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), -0.1) |>
  ast$cp$comp() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), Inf) |>
  ast$cp$comp() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), "dsds") |>
  ast$cp$comp() |>
  try() |>
  class() ==
  "try-error"

# endregion
