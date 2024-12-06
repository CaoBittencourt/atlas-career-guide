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
library(dplyr)
library(tidyr)

box::use(
  desc = mod / describe
)

# endregion
# region: data
# onet occupations data frame
getOption("atlas.occupations") |>
  read.csv() |>
  as_tibble() ->
df_occupations

df_occupations |>
  mutate(
    across(
      .cols = c(
        starts_with("skl_"),
        starts_with("abl_"),
        starts_with("knw_")
      ),
      .fns = ~ .x / 100
    )
  ) -> df_occupations

# my preference-adjusted skill set
getOption("atlas.data") |>
  file.path(
    "questionnaires",
    "questionnaire_Cao.csv"
  ) |>
  read.csv() |>
  as_tibble() ->
df_skill_set

df_skill_set |>
  mutate(
    across(
      .cols = c(
        starts_with("skl_"),
        starts_with("abl_"),
        starts_with("knw_")
      ),
      .fns = ~ .x / 100
    )
  ) -> df_skill_set

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
# region: competence
df_skill_set |>
  pivot_longer(
    cols = -1,
    names_to = "item",
    values_to = "item_score",
  ) |>
  group_by(across(
    -starts_with("item")
  )) |>
  reframe(
    generality = desc$gn$gene(item_score),
    comp_mean_specialty_root = desc$cp$comp(item_score, comp_method = "mean", aeq_method = "specialty-root"),
    comp_cobb_douglas_specialty_root = desc$cp$comp(item_score, comp_method = "cobb-douglas", aeq_method = "specialty-root"),
    comp_mean_default = desc$cp$comp(item_score, comp_method = "mean"),
    comp_cobb_douglas_default = desc$cp$comp(item_score, comp_method = "cobb-douglas")
  )

# endregion
