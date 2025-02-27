# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
chr_pkg <- c(
  "devtools" # github packages
  , "readr" # read and write data
  , "tidyr", "dplyr", "stringr", "scales" # data wrangling
)

# git packages
chr_git <- c(
  "CaoBittencourt" = "atlas.match"
)

# activate / install cran packages
lapply(
  chr_pkg,
  function(pkg) {
    if (!require(pkg, character.only = T)) {
      install.packages(pkg, dependencies = T)
    }

    require(pkg, character.only = T)
  }
)

# activate / install git packages
Map(
  function(git, profile) {
    if (!require(git, character.only = T)) {
      install_github(
        paste0(profile, "/", git),
        dependencies = T,
        upgrade = F,
        force = T
      )
    }

    require(git, character.only = T)
  },
  git = chr_git,
  profile = names(chr_git)
)

# endregion
# region: data
# set wd
getwd() |>
  file.path(
    "articles",
    "1.introduction-matching"
  ) |>
  setwd()

# skill set matrix
getwd() |>
  file.path(
    "data",
    "df_occupations_2022.csv"
  ) |>
  read_csv() ->
df_occupations

# attribute names
df_attribute_names <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vStiX7PF7ltnjqVTCLJchWtaAW_BhVCwUM1hRCXGolCOLCS8mCygyde6FfhcvhZiAvke-tujkTBpXoA/pub?gid=0&single=true&output=csv")

# endregion
# model
# region: matching data frame
df_occupations %>%
  select(
    occupation,
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) -> df_matching

# endregion
# region: euclidean matching model
fun_match_similarity(
  df_data_rows = df_matching,
  df_query_rows = df_matching,
  chr_method = "euclidean",
  chr_weights = "linear",
  dbl_scale_ub = 100,
  dbl_scale_lb = 0,
  chr_id_col = "occupation",
  lgc_sort = T
) -> list_matching

# endregion
# results
# region: select occupations to showcase results
# selected occupations
c(
  "Mechanical Engineers",
  "Physicists",
  "Credit Analysts",
  "Dishwashers"
) -> chr_occupations

# similarity matrix
list_matching$
  mtx_similarity[
  chr_occupations,
  chr_occupations
] -> mtx_similarity


# endregion
# region: top 10 matches
# select top 10 matches
list_matching$
  list_similarity[
  chr_occupations
] %>%
  map(head, 11) %>%
  map(
    as_tibble,
    rownames =
      "Comparison Occupation"
  ) %>%
  map(
    rename,
    Similarity = 2
  ) -> list_df_top10

# arrange by similarity
list_df_top10 %>%
  map(
    arrange,
    -Similarity
  ) -> list_df_top10

# endregion
# region: general descriptive stats
# descriptive statistics for all clusters
df_occupations %>%
  mutate(
    market_value =
      sum(
        employment_variants *
          wage
      )
  ) %>%
  group_by(
    career_cluster
  ) %>%
  reframe(
    N = n(),
    Employment = sum(
      employment_variants
    ),
    Wage = weighted.mean(
      wage,
      employment_variants
    ),
    `Market Share` = sum(
      wage * employment_variants
    ) / first(market_value)
  ) %>%
  arrange(desc(
    `Market Share`
  )) -> df_clusters

# format table
df_clusters %>%
  rename(
    Cluster = career_cluster
  ) %>%
  mutate(
    Employment =
      Employment %>%
        ceiling() %>%
        number(
          big.mark = ",",
          accuracy = 1
        ),
    Wage = dollar(
      Wage
    ),
    `Market Share` =
      percent(
        `Market Share`,
        .01
      )
  ) -> df_clusters

# endregion
# region: selected occupations table
# descriptive table data
df_occupations %>%
  filter(
    occupation %in%
      chr_occupations
  ) %>%
  select(
    id_soc_code,
    occupation,
    career_cluster,
    employment,
    wage
  ) -> df_desc

# format descriptive table
df_desc %>%
  arrange(desc(
    wage
  )) %>%
  mutate(
    employment =
      number(employment),
    wage = dollar(wage, .01)
  ) -> df_desc

c(
  "SOC",
  "Occupation",
  "Cluster",
  "Employment",
  "Wage (2021)"
) -> names(df_desc)

# endregion
# region: attributes table
# descriptive table data
df_matching %>%
  filter(
    occupation %in%
      chr_occupations
  ) %>%
  left_join(
    chr_occupations %>%
      as_tibble() %>%
      rename(
        occupation = 1
      )
  ) %>%
  pivot_longer(
    cols = -1,
    names_to = "attribute"
  ) %>%
  pivot_wider(
    values_from = "value",
    names_from = "occupation"
  ) %>%
  arrange(desc(across(
    .cols = chr_occupations
  ))) -> df_attributes

# competency names
df_attributes %>%
  right_join(
    df_attribute_names
  ) %>%
  select(
    Competency,
    any_of(
      chr_occupations
    )
  ) -> df_attributes

# endregion
# export
# region: rds
# matching results
write_rds(
  x = list_matching,
  file = getwd() |> file.path("output", "list_matching.rds")
)

# endregion
# region: csv
# csv
list_matching$
  mtx_similarity |>
  as_tibble(
    rownames = "comparison_occupation"
  ) |>
  write_csv(
    file = getwd() |> file.path("output", "similarity_matrix.csv")
  )


# endregion
