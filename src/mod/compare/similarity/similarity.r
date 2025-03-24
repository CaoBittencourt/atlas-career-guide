# setup
# region: imports
box::use(
  assert = mod / utils / assert,
  mod / utils / data[cbindmap, conform],
  mod / utils / math[as.bernoulli],
  eq = mod / describe / aeq,
  stats[setNames],
  dplyr[bind_rows, as_tibble],
)

# endregion
# methods
# region: similarity methods
box::use(
  mod / compare / similarity / methods[...]
)

# endregion
# region: list of methods
list(
  "bvls" = "bvls",
  "cobb_douglas" = "cobb-douglas",
  "euclidean" = "euclidean",
  "gmme" = "gmme",
  "logit" = "logit",
  "pearson" = "pearson",
  "probit" = "probit"
) -> similarity.methods

# endregion
# dispatch
# region: similarity vectorized dispatch helper function
similarity.dispatch <- function(ak, A, Ä, similarity_method, bind, ...) {
  # assert args in main function
  # conform skill sets to skill set matrix
  ak |> conform(A) -> Ak

  if (any(similarity_method %in% similarity.methods[c("logit", "probit")])) {
    Ak |>
      lapply(
        as.bernoulli,
        ub = 1,
        lb = 0,
        scaling = 100
      ) |>
      lapply(as.data.frame) ->
    bernoulliAk

    A |>
      as.bernoulli(
        ub = 1,
        lb = 0,
        scaling = 100
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
  similarity_method |>
    setNames(
      similarity_method
    ) |>
    lapply(
      switch,
      "bvls" = Ak |> cbindmap(similarity.bvls, names(A), A, sqrt(Ä)),
      "cobb-douglas" = Ak |> cbindmap(similarity.cobb_douglas, names(A), A, Ä),
      "euclidean" = Ak |> cbindmap(similarity.euclidean, names(A), A, Ä),
      "gmme" = Ak |> cbindmap(similarity.gmme, names(A), A, Ä),
      "logit" = bernoulliAk |> cbindmap(similarity.logit, names(A), bernoulliA, repsÄ, link = "logit"),
      "pearson" = Ak |> cbindmap(similarity.pearson, names(A), A, Ä),
      "probit" = bernoulliAk |> cbindmap(similarity.logit, names(A), bernoulliA, repsÄ, link = "probit")
    ) ->
  similarity.results

  # bind results into data frame
  if (bind) {
    similarity.results |>
      lapply(
        as_tibble,
        rownames = "to"
      ) |>
      bind_rows(
        .id = "method"
      ) ->
    similarity.results
  }

  return(similarity.results)
}

# endregion
# region: similarity generic function
similarity <- function(skill_set, skill_mtx, similarity_method = similarity.methods$euclidean, bind = T, ...) {
  # assert args
  assert$base$validate.method(similarity_method, "similarity_method", similarity.methods)
  assert$models$as.skill.set.matrix(skill_set, arg.name = "skill_set") -> Ak
  assert$models$as.skill.set.matrix(skill_mtx, arg.name = "skill_mtx") -> A

  # multiple dispatch
  return(
    similarity.dispatch(
      Ak,
      A,
      A |> vapply(eq$aeq, numeric(nrow(A))) |> as.data.frame(),
      similarity_method,
      bind
    )
  )
}

# endregion
# exports
# region: exports
box::export(similarity, similarity.methods)

# endregion
