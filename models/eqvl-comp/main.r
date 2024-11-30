# setup
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
  assert = mod / utils / assert,
  gn = mod / stats / gene,
  dplyr[...],
  tidyr[...]
)

# endregion
# region: data
getOption("atlas.occupations") |>
  read.csv() |>
  as_tibble() ->
df_occupations

# endregion
# models
# region: attribute equivalence 1
aeq <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # remove generality from attribute equivalence?
  return(skill_set / max(skill_set))
}

# endregion
# region: attribute equivalence 2
aeq2 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  fun_eqvl_logistic <- function(x, a = 0, k = 1, c = 1, q = 1, m = 0, b = 1, nu = 1) {
    # Arguments validated in main functions

    # Generalized logistic function
    y <- a + (k - a) / ((c + q * exp(-b * (x - m)))^(1 / nu))

    # output
    return(y)
  }

  # define variable and midpoint
  skill_set -> x
  gn$gene(skill_set) -> m

  rm(skill_set)

  # calculate attribute equivalence
  # with generalized logistic function
  fun_eqvl_logistic(
    x = x,
    m = m,
    a = 0,
    k = x,
    c = 1,
    q = m * (1 - x),
    nu = x / m,
    b = 1 / (1 - m)
  ) -> dbl_attribute_eqvl

  rm(x, m)

  return(dbl_attribute_eqvl)
}

# endregion
# region: skill set competence 1
comp <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set |> eq$aeq()
    )
  )
}

# endregion
# region: skill set competence 2
comp2 <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # return skill set competence
  return(
    weighted.mean(
      x = skill_set,
      w = skill_set |> eq$aeq2()
    )
  )
}

# endregion
# comments
