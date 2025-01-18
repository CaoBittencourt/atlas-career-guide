# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  s = mod / compare / match
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
df_occupations[-1] -> df_occupations

# endregion
# model
# region: egmap all matching methods
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
# region: vmap all matching methods

# endregion
