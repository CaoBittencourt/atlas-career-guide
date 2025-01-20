# region: imports
box::use(
  assert = mod / utils / assert,
  mod / utils / egmap[...],
  mod / utils / vmap[...],
  eq = mod / describe / aeq,
  bin = mod / compare / match / methods / bin,
  vec = mod / compare / match / methods / vec,
  stats[setNames],
  mod / utils / conform[...],
  dplyr[bind_rows],
  mod / utils / cbindmap[...]
)

# endregion
# region: egmap dispatch function
s.bin <- function(ak, aq, äq, match_method = c("euclidean", "bvls", "logit", "probit", "cobb-douglas", "gmme", "pearson")[[1]], ...) {
  # assert args in main function

  # multiple dispatch
  match_method[[1]] |>
    switch(
      "euclidean" = bin$euclidean(ak, aq, äq),
      "bvls" = bin$bvls(ak, aq, äq),
      "logit" = bin$logit(ak, aq, äq, link = "logit"),
      "probit" = bin$logit(ak, aq, äq, link = "probit"),
      "cobb-douglas" = bin$cobb_douglas(ak, aq, äq, ...),
      "gmme" = bin$gmme(ak, aq, äq, ...),
      "pearson" = bin$pearson(ak, aq, äq)
    )
}

# endregion
# region: vector dispatch function
s.vec <- function(ak, A, Ä, match_method = c("euclidean", "bvls", "logit", "probit", "cobb-douglas", "gmme", "pearson")[[1]], bind = T, ...) {
  # assert args in main function
  # conform skill sets to skill set matrix
  ak |> conform(A) -> Ak

  # multiple dispatch
  match_method |>
    setNames(
      match_method
    ) |>
    lapply(
      switch,
      "euclidean" = Ak |> cbindmap(vec$euclidean, names(A), A, Ä),
      # "bvls" = vec$bvls(ak, aq, äq),
      # "logit" = vec$logit(ak, aq, äq, link = "logit"),
      # "probit" = vec$logit(ak, aq, äq, link = "probit"),
      # "cobb-douglas" = vec$cobb_douglas(ak, aq, äq, ...),
      # "gmme" = vec$gmme(ak, aq, äq, ...),
      "pearson" = Ak |> cbindmap(vec$pearson, names(A), A, Ä)
    ) ->
  match.results

  # bind results into data frame
  if (bind) {
    match.results |>
      bind_rows(
        .id = "match_method"
      ) ->
    match.results
  }

  return(match.results)
}

# endregion
# region: vmap dispatch function

# endregion
# region: generic function
similarity <- function(skill_set, skill_mtx, match_method = c("euclidean", "bvls", "logit", "probit", "cobb-douglas", "gmme", "pearson")[[1]], mode = c("vector", "egmap")[[1]], bind = T, ...) {
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

  stopifnot(
    "'mode' must be either 'vector' or 'egmap'." = any(
      mode[[1]] == c("vector", "vmap", "egmap")
    )
  )

  # multiple dispatch on mode
  mode[[1]] |>
    switch(
      "vector" = return(
        skill_set |>
          s.vec(
            skill_mtx,
            skill_mtx |> vapply(eq$aeq, numeric(120)) |> as.data.frame(),
            match_method,
            bind
          )
      ),
      "vmap" = return("vmap mode"),
      "egmap" = return(
        list(
          from = Ak,
          to = list(
            to = Aq,
            # note: vectorize aeq
            aeq = Aq |> vapply(eq$aeq, numeric(120))
            # aeq = Aq |> lapply(eq$aeq)
          ),
          match_method =
            match_method |>
              setNames(match_method) |>
              rbind()
        ) |> egmap(s.bin, ...)
      ),
    )
}

# endregion
# region: exports
box::export(similarity)

# endregion
