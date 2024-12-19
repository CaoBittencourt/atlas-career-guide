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
    box.path = "src",
    atlas.mod = "src/mod",
    atlas.db = "database",
    atlas.data = "database/data",
    atlas.oldata = "database/data/old/occupations/df_occupations_2022.csv"
  ),
  root.name = ".atlas"
)

# endregion
# region: imports
box::use(
  dplyr[...],
  tidyr[...],
  stringr[str_sub, str_remove_all],
  readr[write_csv]
)

# endregion
# region: working directory
setwd(getOption("atlas.data"))

# endregion
# region: data
getOption("atlas.oldata") |>
  read.csv() |>
  as_tibble() ->
df_occupations

# endregion
# wrang
# region: occupations
df_occupations |>
  arrange(occupation) |>
  mutate(
    .before = 1,
    pk_occupation_id = row_number()
  ) -> df_occupations

df_occupations |>
  mutate(
    soc_2int = str_sub(id_soc_code, 1, 2),
    soc_4int = str_sub(id_soc_code, 4, -1)
  ) |>
  select(
    pk_occupation_id,
    occupation,
    soc_2int,
    soc_4int
  ) -> occupations

# endregion
# region: labor
df_occupations |>
  select(
    pk_occupation_id,
    employment,
    wage
  ) -> labor

# endregion
# region: education
df_occupations |>
  select(
    pk_occupation_id,
    fk_education_id = id_education
  ) -> required_education

df_occupations |>
  select(
    pk_education_id = id_education,
    education,
    education_years
  ) |>
  unique() |>
  arrange(pk_education_id) ->
education

# endregion
# region: attributes
df_occupations |>
  select(
    pk_occupation_id,
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "item",
    values_to = "item_score"
  ) |>
  mutate(
    item_category =
      str_sub(item, 1, 4) |>
        str_remove_all("_"),
    item = str_sub(item, 5),
    item_score = item_score / 100
  ) |>
  relocate(
    -item_score
  ) -> competencies

df_occupations |>
  select(
    pk_occupation_id,
    starts_with("stl_")
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "item",
    values_to = "item_score"
  ) |>
  mutate(
    item_category =
      str_sub(item, 1, 4) |>
        str_remove_all("_"),
    item = str_sub(item, 5),
    item_score = item_score / 100
  ) |>
  relocate(
    -item_score
  ) -> styles

df_occupations |>
  select(
    pk_occupation_id,
    starts_with("ctx_")
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "item",
    values_to = "item_score"
  ) |>
  mutate(
    item_category =
      str_sub(item, 1, 4) |>
        str_remove_all("_"),
    item = str_sub(item, 5),
    item_score = item_score / 100
  ) |>
  relocate(
    -item_score
  ) -> contexts

df_occupations |>
  select(
    pk_occupation_id,
    starts_with("act_")
  ) |>
  pivot_longer(
    cols = -1,
    names_to = "item",
    values_to = "item_score"
  ) |>
  mutate(
    item_category =
      str_sub(item, 1, 4) |>
        str_remove_all("_"),
    item = str_sub(item, 5),
    item_score = item_score / 100
  ) |>
  relocate(
    -item_score
  ) -> activities

list(
  competencies,
  styles,
  contexts,
  activities
) |>
  lapply(
    function(df) {
      df |>
        select(
          item,
          item_category
        ) |>
        unique()
    }
  ) |>
  bind_rows() |>
  mutate(
    .before = 1,
    pk_item_id = row_number()
  ) -> attributes

activities |>
  inner_join(attributes) |>
  select(-item, -item_category) |>
  relocate(-item_score) ->
activities

competencies |>
  inner_join(attributes) |>
  select(-item, -item_category) |>
  relocate(-item_score) ->
competencies

contexts |>
  inner_join(attributes) |>
  select(-item, -item_category) |>
  relocate(-item_score) ->
contexts

styles |>
  inner_join(attributes) |>
  select(-item, -item_category) |>
  relocate(-item_score) ->
styles

# endregion
# write
# region: export data
print("Writing attributes data.")
attributes |> write_csv("attributes.csv")

print("Writing activities data.")
activities |> write_csv("activities.csv")

print("Writing competencies data.")
competencies |> write_csv("competencies.csv")

print("Writing contexts data.")
contexts |> write_csv("contexts.csv")

print("Writing styles data.")
styles |> write_csv("styles.csv")

print("Writing education data.")
education |> write_csv("education.csv")

print("Writing labor data.")
labor |> write_csv("labor.csv")

print("Writing occupations data.")
occupations |> write_csv("occupations.csv")

print("Writing required education data.")
required_education |> write_csv("required_education.csv")

# endregion
