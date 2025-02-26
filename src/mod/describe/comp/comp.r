# region: imports
box::use(
  gn = mod / describe / gene,
  eq = mod / describe / aeq,
  assert = mod / utils / assert,
  kflex = mod / micro / kflex,
  dplyr[...],
  stats[weighted.mean]
)

# endregion
# # region: imports
# box::use(
#   gn = mod / describe / gene,
#   eq = mod / describe / aeq,
#   assert = mod / utils / assert,
#   stats[weighted.mean]
# )

# # endregion
# region: weighted penalized mlv (i.e. mean) method
comp.mean <- function(skill_set, ä) {
  # assert args in main function
  # skill set competence as aeq-weighted mean
  return(
    weighted.mean(
      x = skill_set,
      w = ä
    )
  )
}

# endregion
# region: cobb-douglas method
comp.cobb_douglas <- function(skill_set, ä) {
  # assert args in main function
  # skill set competence as cobb-douglas production function
  return(prod(skill_set^(ä / sum(ä))))
}

# endregion
# # region: skill set competence generic
# comp <- function(skill_set, comp_method = c("mean", "cobb-douglas")[[1]], ...) {
#   # assert args
#   assert$valid_skill_set(skill_set)

#   stopifnot(
#     "'comp_method' must be one of the following methods: 'mean', 'cobb-douglas'." = any(
#       comp_method == c("mean", "cobb-douglas")
#     )
#   )

#   # estimate attribute equivalence
#   eq$aeq |> do.call(args = c(list(skill_set), list(...))) -> ä

#   # multiple dispatch
#   comp_method[[1]] |>
#     as.character() |>
#     switch(
#       # "mean" = return(comp.mean(skill_set, 1 + ä)),
#       # "cobb-douglas" = return(comp.cobb_douglas(skill_set, 1 + ä))
#       "mean" = return(comp.mean(skill_set, ä)),
#       "cobb-douglas" = return(comp.cobb_douglas(skill_set, ä))
#     )
# }

# # endregion
# region: skill set competence generic
getOption("atlas.skills_mtx") |>
  readRDS() -> dsds

kflex$macroflex(
  dsds,
  weights =
    getOption("atlas.labor") |>
      readRDS() |>
      pull(
        employment_variants
      ),
  skill.names = dsds$item
) -> PHI

comp <- function(skill_set, comp_method = c("mean", "cobb-douglas")[[1]], macroflex_weight = F, ...) {
  # assert args
  assert$valid_skill_set(skill_set)

  stopifnot(
    "'comp_method' must be one of the following methods: 'mean', 'cobb-douglas'." = any(
      comp_method == c("mean", "cobb-douglas")
    )
  )

  stopifnot(
    "'macroflex_weight' must be either TRUE or FALSE." = all(
      macroflex_weight[[1]] |> is.logical(),
      macroflex_weight[[1]] |> is.na() |> isFALSE()
    )
  )

  # estimate attribute equivalence
  eq$aeq |> do.call(args = c(list(skill_set), list(...))) -> ä

  # multiple dispatch
  comp_method[[1]] |>
    as.character() |>
    switch(
      # "mean" = return(comp.mean(skill_set, 1 + ä)),
      # "cobb-douglas" = return(comp.cobb_douglas(skill_set, 1 + ä))
      "mean" = return(comp.mean(pmax(skill_set - PHI, 0), ä)),
      "cobb-douglas" = return(comp.cobb_douglas(skill_set, ä * ifelse(macroflex_weight, 1 - PHI, 1)))
    )
}

# endregion
# region: exports
box::export(comp)

# endregion
