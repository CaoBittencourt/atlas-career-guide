# region: imports
box::use(
  assert = mod / utils / assert,
  mod / compare / similarity[similarity.methods],
  mod / utils / egmap[...],
  mod / utils / vmap[...],
  eq = mod / describe / aeq,
  bin = mod / compare / match / methods / bin,
  vec = mod / compare / match / methods / vec,
  stats[setNames],
  mod / utils / bernoulli[...],
  mod / utils / cbindmap[...],
  mod / utils / conform[...],
  dplyr[bind_rows],
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

  if (
    any(match_method %in% c("logit", "probit"))
  ) {
    Ak |>
      lapply(
        as.bernoulli,
        ub = 100,
        lb = 0
      ) |>
      lapply(as.data.frame) ->
    bernoulliAk

    A |>
      as.bernoulli(
        ub = 100,
        lb = 0
      ) |>
      as.data.frame() ->
    bernoulliA

    Ä[
      Ä |>
        nrow() |>
        seq_len() |>
        rep(each = 100 - 0),
    ] |> as.data.frame() ->
    repsÄ
  }

  # multiple dispatch
  match_method |>
    setNames(
      match_method
    ) |>
    lapply(
      switch,
      "euclidean" = Ak |> cbindmap(vec$euclidean, names(A), A, Ä),
      "bvls" = Ak |> cbindmap(vec$bvls, names(A), A, sqrt(Ä)),
      "logit" = bernoulliAk |> cbindmap(vec$logit, names(A), bernoulliA, repsÄ, link = "logit"),
      "probit" = bernoulliAk |> cbindmap(vec$logit, names(A), bernoulliA, repsÄ, link = "probit"),
      "cobb-douglas" = Ak |> cbindmap(vec$cobb_douglas, names(A), A, Ä),
      "gmme" = Ak |> cbindmap(vec$gmme, names(A), A, Ä),
      "pearson" = Ak |> cbindmap(vec$pearson, names(A), A, Ä)
    ) ->
  match.results

  # bind results into data frame
  if (bind) {
    match.results |>
      bind_rows(
        .id = "method"
      ) ->
    match.results
  }

  return(match.results)
}

# endregion
# region: generic function
similarity <- function(skill_set, skill_mtx, match_method = c("euclidean", "bvls", "logit", "probit", "cobb-douglas", "gmme", "pearson")[[1]], mode = c("cbmap", "egmap")[[1]], bind = T, ...) {
  # assert args
  assert$base$validate.method(match_method, "match_method", similarity.methods)
  assert$models$as.skill.set.matrix(skill_set, arg.name = "skill_set") -> Ak
  assert$models$as.skill.set.matrix(skill_mtx, arg.name = "skill_mtx") -> A

  stopifnot(
    "'mode' must be either 'cbmap' or 'egmap'." = any(
      mode[[1]] == c("cbmap", "egmap")
    )
  )

  # multiple dispatch on mode
  mode[[1]] |>
    switch(
      "cbmap" = return(
        Ak |>
          s.vec(
            A,
            A |> vapply(eq$aeq, numeric(nrow(A))) |> as.data.frame(),
            match_method,
            bind
          )
      ),
      "egmap" = return(
        list(
          from = Ak,
          to = list(
            to = A,
            # note: vectorize aeq
            aeq = A |> vapply(eq$aeq, numeric(nrow(A)))
          ),
          method =
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
