# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  stats[...],
  dplyr[...],
  tidyr[...],
  micro / kflex,
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# labor statistics
(getOption("atlas.labor") |> readRDS())$employment_variants -> employment

# endregion
# model
# region: human capital macroflexibility (uppercase Phi)
df_occupations |>
  kflex$macroflex(
    employment,
    df_occupations$item
  ) ->
Phi

Phi |> sort(T)

# endregion
# region: versatility (skill set aggregate macroflexibility)
df_occupations |>
  kflex$versatility(Phi) |>
  sort(T)

# endregion
