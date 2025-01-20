# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  s = mod / compare / match,
  mod / utils / conform[...]
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
df_occupations[-1] -> df_occupations

# endregion
# model
# region: egmap mode
df_occupations[1:3] |>
  s$similarity(
    df_occupations[1:3],
    mode = "egmap",
    match_method = c(
      "euclidean",
      "bvls",
      "logit",
      "probit",
      "cobb-douglas",
      "gmme",
      "pearson"
    )
  ) ->
eg_similarity

eg_similarity |> print(n = nrow(eg_similarity))

# endregion
# region: vector mode
# note: is from/to inverted?
df_occupations[1] |>
  s$similarity(
    df_occupations,
    mode = "vector",
    bind = F,
    match_method = c(
      "euclidean",
      # ,
      # # "bvls",
      # # "logit",
      # # "probit",
      # # "cobb-douglas",
      # # "gmme",
      "pearson"
    )
  ) ->
vec_similarity

vec_similarity |> bind_rows(.id = "method")

df_occupations[1:2] |>
  conform(df_occupations) -> Ak

box::use(mod / utils / cbindmap[...], weights[wtd.cors])

Ak |> cbindmap(pearson, A = df_occupations, Ä = df_occupations)


box::use(
  mod / compare / match / methods / vec / euclidean[...]
)

s.vec <- function() {
  ak |> conform(A) -> Ak

  match_method |>
    setNames(
      match_method
    ) |>
    lapply(
      switch,
      "euclidean" = Ak |> vec.apply(vec$euclidean, A, Ä)
    )
}

library(purrr)

cbindmap <- function(list, fn, to = NULL, ...) {
  return(
    list |>
      lapply(fn, ...) |>
      bind_cols() |>
      mutate(
        .before = 1,
        to = to
      )
  )
}

df_occupations[1:2] |>
  conform(df_occupations) |>
  map_dfc(
    euclidean,
    df_occupations,
    df_occupations
  ) |>
  mutate(
    .before = 1,
    to = NULL
  )

df_occupations[1:2] |>
  conform(df_occupations) |>
  lapply(
    euclidean,
    df_occupations,
    df_occupations
  ) |>
  bind_cols() |>
  mutate(
    .before = 1,
    to = NULL
  )

dsds
vec_similarity |> print(n = nrow(vec_similarity))

# endregion
# region: vmap mode
df_occupations[1:3] |>
  s$similarity(
    df_occupations[1:3],
    mode = "vmap",
    match_method = c(
      "euclidean",
      "bvls",
      "logit",
      "probit",
      "cobb-douglas",
      "gmme",
      "pearson"
    )
  ) ->
vm_similarity

vm_similarity |> print(n = nrow(vm_similarity))

# endregion
