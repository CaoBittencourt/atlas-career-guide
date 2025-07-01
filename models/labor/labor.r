# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  labor = labor,
  tt = compare / prod,
  dplyr[...]
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
getOption("atlas.labor") |> readRDS() -> df_labor

# endregion
# wrang
# region: sample data
df_occupations |> select(2) -> df_from
df_occupations |> select(2:19) -> df_to
df_labor |> filter(occupation %in% names(df_to)) -> df_labor

# endregion
# model
# region: employability
df_from |>
  tt$productivity(df_to) |>
  pull(names(df_from)) |>
  labor$employability(
    hk = 1,
    ttc = exp,
    w = df_labor$employment_norm,
    agg = F
  )

# endregion
# region: competitiveness
df_to |>
  as.matrix() |>
  tt$productivity(df_from) |>
  select(!1:2) |>
  as.numeric() |>
  labor$competitiveness(
    h_q = 1,
    u_qk = 1,
    u_qq = 1,
    ttc = exp,
    w = df_labor$employment_norm,
    agg = F
  )

df_to |>
  as.matrix() |>
  tt$productivity(df_from) |>
  select(!1:2) |>
  as.numeric() |>
  labor$competitiveness(
    h_q = 1,
    u_qk = 1,
    u_qq = 1,
    ttc = exp,
    w = df_labor$employment_variants,
    agg = T
  )

# endregion
# region: vulnerability
df_to |>
  as.matrix() |>
  tt$productivity(df_from) |>
  select(!1:2) |>
  as.numeric() |>
  labor$vulnerability(
    h_q = 1,
    ttc = exp,
    w = df_labor$employment_norm,
    agg = F
  )

df_to |>
  as.matrix() |>
  tt$productivity(df_from) |>
  select(!1:2) |>
  as.numeric() |>
  labor$vulnerability(
    h_q = 1,
    ttc = exp,
    w = df_labor$employment_variants,
    agg = T
  )

# endregion
