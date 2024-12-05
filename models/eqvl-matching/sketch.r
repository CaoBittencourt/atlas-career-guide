# setup
# region: modules
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# install modular if not installed
if (!any(utils::installed.packages()[, 1] == "modular")) {
  devtools::install_github("CaoBittencourt/modular")
}

library(modular)

# objective project root
project.options(
  project.name = "atlas",
  relative.paths = list(
    atlas.src = "src",
    atlas.mod = "src/mod",
    box.path = "src",
    atlas.data = "data"
  ),
  root.name = ".atlas"
)

# endregion
# region: imports
box::use(
  match = mod / compare / match / cobb_douglas
)

library(dplyr)
library(atlas.aeq)

# endregion
# region: data
# onet occupations data frame
getOption("atlas.occupations") |>
  read.csv() |>
  as_tibble() ->
df_occupations

# my preference-adjusted skill set
getOption("atlas.data") |>
  file.path(
    "questionnaires",
    "questionnaire_Cao.csv"
  ) |>
  read.csv() |>
  as_tibble() ->
df_skill_set

# sample occupations
c(
  "Mechanical Engineers",
  "Physicists",
  "Credit Analysts",
  "Dishwashers",
  "Registered Nurses",
  "Hospitalists",
  "Philosophy and Religion Teachers, Postsecondary"
) -> chr_sample

# df_occupations$
#   occupation ->
# chr_sample

# Sample occupations data frame
df_occupations %>%
  filter(
    occupation %in%
      chr_sample
  ) %>%
  mutate(
    occupation = factor(
      occupation,
      levels =
        chr_sample
    )
  ) %>%
  arrange(
    occupation
  ) -> df_sample

# Select only occupations and attributes
df_sample %>%
  select(
    occupation,
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) -> df_sample

# endregion
# model
# region: runif cobb-douglas matching model
runif(120) -> ss

replicate(
  n = 873,
  runif(120)
) -> mtx_ss

mtx_ss |> apply(2, fun_aeq_aequivalence) -> mtx_aeq

mtx_ss |> dim()
mtx_aeq |> dim()
(mtx_ss^mtx_aeq) |> dim()
ss |> length()

match$cobb_douglas(ss, mtx_ss, mtx_aeq)
match$cobb_douglas(ss, cbind(ss), cbind(ss))

# endregion
# region: cobb-douglas matching model
df_occupations |>
  slice_head(n = 1) |>
  select(
    occupation,
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) -> df_skill_set

df_skill_set[-1] ->
df_skill_set

df_skill_set |>
  as.numeric() -> ss

df_occupations |>
  select(
    names(df_skill_set)
  ) |>
  as.matrix() |>
  t() -> ss_mtx

df_occupations$
  occupation ->
colnames(ss_mtx)

ss_mtx |>
  apply(
    2, fun_aeq_aequivalence
  ) -> aeq_mtx

ss_mtx |> dim()
aeq_mtx |> dim()
(ss_mtx^aeq_mtx) |> dim()
ss |> length()

match$cobb_douglas(
  skill_set = ss,
  skill_mtx = ss_mtx,
  weights = aeq_mtx
)

# endregion
