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
    tt$productivity.methods,
    bind = T
  )

df_occupations[1:20] |>
  tt$productivity(
    df_occupations[1:20],
    tt$productivity.methods,
    bind = F
  )

df_occupations[1:20] |>
  tt$productivity(
    df_occupations[1:20],
    tt$productivity.methods$cobb_douglas,
    bind = F
  )

df_occupations[1:20] |>
  tt$productivity(
    df_occupations[1:20],
    tt$productivity.methods$gmme,
    bind = T
  )

# endregion
