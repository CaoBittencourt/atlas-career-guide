modular::project.options("atlas")
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / utils / egmap[...],
  eq = mod / describe / aeq,
  mod / compare / match / methods / bvls[...],
  mod / compare / match / methods / pearson[...],
  stats[setNames]
)

# endregion
# region: dispatch function
s <- function(ak, aq, äq, match_method = c("euclidean", "bvls", "logit", "probit", "cobb-douglas", "gmme", "pearson")[[1]], aeq_method, ...) {
  # assert args in main function

  # multiple dispatch
  match_method[[1]] |>
    switch(
      "euclidean" = s.euclidean(ak, aq, äq),
      "bvls" = s.bvls(ak, aq, äq),
      "logit" = s.logit(ak, aq, äq),
      "probit" = s.probit(ak, aq, äq),
      "cobb-douglas" = s.cobb_douglas(ak, aq, äq),
      "gmme" = s.gmme(ak, aq, äq),
      "pearson" = s.pearson(ak, aq, äq)
    )
}

# endregion
# region: egmap function
similarity <- function(skill_set, skill_mtx, match_method = c("euclidean", "bvls", "logit", "probit", "cobb-douglas", "gmme", "pearson")[[1]], ...) {
  # assert args
  assert$as.skill_mtx(skill_set) -> Ak
  assert$as.skill_mtx(skill_mtx) -> Aq

  stopifnot(
    "'match_method' must be one of the following methods: 'euclidean', 'bvls', 'logit', 'probit', 'cobb-douglas', 'gmme', 'pearson'." = all(
      match_method |> vapply(
        function(method) {
          all(any(method == c("euclidean", "bvls", "logit", "probit", "cobb-douglas", "gmme", "pearson")))
        },
        logical(1)
      )
    )
  )

  # egmap similarity
  # note: vectorize aeq
  return(list(from = Ak, to = list(to = Aq, aeq = Aq |> vapply(eq$aeq, numeric(120))), match_method = match_method |> setNames(match_method) |> rbind()) |> egmap(s, ...))
}

# endregion
# # region: exports
# box::export(similarity)

# # endregion
# region: test
(getOption("atlas.skills_mtx") |> readRDS())[-1] -> dsds

similarity(dsds[1:4], dsds[1:4], match_method = c("bvls", "pearson")) -> lalala
lalala

# endregion
