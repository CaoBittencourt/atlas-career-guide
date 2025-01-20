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
# region: cbmap dispatch function
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
      "cobb-douglas" = Ak |> cbindmap(vec$cobb_douglas, names(A), A, Ä),
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
  assert$as.skill_mtx(skill_mtx) -> A

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
        Ak |>
          s.vec(
            A,
            A |> vapply(eq$aeq, numeric(nrow(A))) |> as.data.frame(),
            match_method,
            bind
          )
      ),
      "vmap" = return("vmap mode"),
      "egmap" = return(
        list(
          from = Ak,
          to = list(
            to = A,
            # note: vectorize aeq
            aeq = A |> vapply(eq$aeq, numeric(nrow(A)))
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
