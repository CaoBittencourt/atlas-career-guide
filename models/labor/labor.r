# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  labor = mod / labor,
  tt = mod / compare / prod
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

getOption("atlas.labor") |> readRDS() -> df_labor

# endregion
# model
# region: employability
df_occupations[1:2] |> tt$productivity(df_occupations[1:19]) -> ttilde

box::use(
  mod / labor / employability / misc / pec[...]
)

labor$employability(
  hk = 1,
  Tk = ttilde$`Accountants and Auditors`,
  ttc = exp,
  w = df_labor$employment_norm[seq_along(ttilde$`Accountants and Auditors`)],
  # p = Inf,
  p = df_labor$employment_norm[seq_along(ttilde$`Accountants and Auditors`)],
  agg = F
)

# endregion
# region: competitiveness

# endregion
# region: vulnerability

# endregion
