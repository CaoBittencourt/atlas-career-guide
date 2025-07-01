# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  eee = compare / eeq,
  dplyr[...],
  tidyr[...],
)

library(atlas.plot)

# endregion
# region: data
getOption("atlas.education") |> readRDS() -> df_education

df_education$education_years |>
  unique() |>
  sort() -> min_years
min_years |> setNames(min_years) -> min_years

seq(0, 2 * max(min_years), length.out = 1000) -> years

# endregion
# model
# region: education and experience equivalence
expand.grid(
  req_years = min_years,
  method = c("linear-logistic","logistic", "binary", "linear")
) |>
  mutate(
    model =
      paste0(
        "Minimum ",
        req_years,
        " years of education and experience (",
        method,
        ")"
      )
  ) -> df_params

eee$eeq |>
  mapply(
    years = df_params |> nrow() |> replicate(years, simplify = F),
    min_years = df_params$req_years,
    eeq_method = df_params$method
  ) |>
  as.data.frame() |>
  setNames(
    df_params$model
  ) |>
  mutate(
    .before = 1,
    years = years
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "model",
    values_to = "eeq"
  ) |>
  inner_join(
    df_params
  ) -> df_eeq

# endregion
# plots
# region: eeq line plot
df_eeq |>
  fun_plot.line(aes(
    x = years,
    y = eeq,
    ), 
  .sym_facets = c(method, req_years),
  # .dbl_limits.x = c(0,30),
  ) + geom_vline(aes(
      xintercept = req_years
    ),
    linetype = 'dashed'
  )

# endregion
