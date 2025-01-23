# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  tt = mod / compare / prod
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# endregion
# model
# region: productivity
df_occupations[1:20] |>
  tt$productivity(
    df_occupations[1:20],
    prod_method = c(
      "cobb-douglas",
      "gmme"
    ),
    bind = F
  ) -> productivity

# endregion
