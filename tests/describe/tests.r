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
  desc = mod / describe,
  util = mod / utils
)

# endregion
# region: test attribute equivalence
# equivalent attributes form a valid skill set
runif(120) |>
  desc$eq$aeq() |>
  util$assert$valid_skill_set() |>
  try() |>
  class() !=
  "try-error"

# invalid skill sets attribute equivalence
c(rep(0, 119), 1.1) |>
  desc$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), -0.1) |>
  desc$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), Inf) |>
  desc$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), "dsds") |>
  desc$eq$aeq() |>
  try() |>
  class() ==
  "try-error"

# unit vector attribute equivalence
all(
  rep(1, 120) |> desc$eq$aeq() == 1,
  rep(1, 120) |> desc$eq$aeq() |> length() == 120
)

# null vector attribute equivalence
all(
  rep(0, 120) |> desc$eq$aeq() == 1,
  rep(0, 120) |> desc$eq$aeq() |> length() == 120
)

# hyper-generalist attribute equivalence
runif(1) |> rep(120) -> skill_set
all(
  skill_set |> desc$eq$aeq() == 1,
  skill_set |> desc$eq$aeq() |> length() == 120
)
rm(skill_set)

# hyper-specialist attribute equivalence
runif(1) |> c(rep(0, 119)) -> skill_set
all(
  skill_set |> desc$eq$aeq() == c(1, rep(0, 119)),
  skill_set |> desc$eq$aeq() |> length() == 120
)
rm(skill_set)

# endregion
# region: test generality
# 1 generality
runif(1) |> rep(120) -> skills
all(
  desc$gn$gene(skills) == 1,
  desc$gn$gene(skills) |> length() == 1
)
rm(skills)

# 0 generality
all(
  desc$gn$gene(c(rep(0, 120), 1)) == 0,
  desc$gn$gene(rep(1, 120)) |> length() == 1
)

# null vector generality
all(
  desc$gn$gene(rep(0, 120)) == 1,
  desc$gn$gene(rep(0, 120)) |> length() == 1
)

# invalid skill sets generality
c(rep(0, 119), 1.1) |>
  desc$gn$gene() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), -0.1) |>
  desc$gn$gene() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), Inf) |>
  desc$gn$gene() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), "dsds") |>
  desc$gn$gene() |>
  try() |>
  class() ==
  "try-error"

# endregion
# region: test competence
# whereas generality of repeated skill set is 1, competence is not
runif(1) |> rep(120) -> skills
all(
  desc$cp$comp(skills) != 1,
  desc$cp$comp(skills) |> length() == 1
)
rm(skills)

# 1 competence
all(
  desc$cp$comp(rep(1, 120)) == 1,
  desc$cp$comp(rep(1, 120)) |> length() == 1
)

# 0 competence
all(
  desc$cp$comp(c(rep(0, 120))) == 0,
  desc$cp$comp(rep(1, 120)) |> length() == 1
)

# invalid skill sets competence
c(rep(0, 119), 1.1) |>
  desc$cp$comp() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), -0.1) |>
  desc$cp$comp() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), Inf) |>
  desc$cp$comp() |>
  try() |>
  class() ==
  "try-error"

c(rep(0, 119), "dsds") |>
  desc$cp$comp() |>
  try() |>
  class() ==
  "try-error"

# endregion
