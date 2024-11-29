# region: modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# install modular if not installed
if (!any(utils::installed.packages()[, 1] == "modular")) {
  devtools::install_github("CaoBittencourt/modular")
}

# objective project root
modular::project.root(root.name = "src.root")

# box module search path
options(box.path = getwd())

# endregion
# region: imports
box::use(
  assert = mod / utils / assert,
  gn = mod / stats / gene
)

# endregion
# region: attribute equivalence
aeq <- function(skill_set, generality = NULL) {
  # assert args
  assert$valid_skill_set(skill_set)

  # remove generality from attribute equivalence?
  return(skill_set / max(skill_set))
}

# endregion
# region: attribute equivalence
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
# region: workforce equivalence

# endregion
# region: equivalent similarity

# endregion
# region: equivalence generic

# endregion
# region: exports
box::export(
  aeq, aeq2
)

# endregion
