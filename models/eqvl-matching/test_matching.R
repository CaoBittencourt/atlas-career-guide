# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
# CRAN packages
chr_pkg <- c(
  "devtools" # GitHub packages
  , "readr" # Read data
  , "weights" # Fast/weighted correlation method
  , "dplyr", "tidyr", "stringr" # Data wrangling
)

# Git packages
chr_git <- c(
  "CaoBittencourt" = "atlas.match",
  "CaoBittencourt" = "atlas.comp",
  "CaoBittencourt" = "atlas.gene",
  "CaoBittencourt" = "atlas.aeq",
  "CaoBittencourt" = "atlas.intc",
  "CaoBittencourt" = "atlas.plot",
  # 'CaoBittencourt' = 'atlas.notiq',
  "CaoBittencourt" = "atlas.class"
)

# Activate / install CRAN packages
lapply(
  chr_pkg,
  function(pkg) {
    if (!require(pkg, character.only = T)) {
      install.packages(pkg)
    }

    require(pkg, character.only = T)
  }
)

# Activate / install Git packages
Map(
  function(git, profile) {
    if (!require(git, character.only = T)) {
      install_github(
        paste0(profile, "/", git),
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
# region: matching methods
c(
  "bvls",
  "logit",
  "probit",
  "pearson",
  "euclidean"
) -> chr_methods

# endregion
# region: matching weights
c(
  "linear",
  "quadratic",
  "speciality-root",
  "attribute-eqvl"
) -> chr_weights

# endregion
# region: matching combinations
crossing(
  method = chr_methods,
  weight = chr_weights
) -> df_methods

df_methods %>%
  mutate(
    .before = 1,
    model = paste0(
      method, "_", weight
    ),
    model = str_replace_all(
      model, "-", "_"
    )
  ) -> df_methods

# endregion
# region: baseline model
"euclidean_linear" -> chr_baseline

# endregion
# results
# region: my matching results
Map(
  function(model, method, weight, over_sub) {
    return(
      fun_match_similarity(
        df_data_rows = df_occupations,
        df_query_rows = df_skill_set,
        chr_method = method,
        chr_weights = weight,
        dbl_scale_ub = 100,
        dbl_scale_lb = 0,
        chr_id_col = "occupation",
        lgc_sort = T,
        lgc_overqualification_sub = F
      )
    )
  },
  model = df_methods$model,
  method = df_methods$method,
  weight = df_methods$weight
) |>
  map(
    function(x) {
      x$
        mtx_similarity |>
        as_tibble(
          rownames = "occupation"
        )
    }
  ) |>
  bind_rows(
    .id = "model"
  ) -> df_matches_mine

# endregion
df_matches_mine


list_matches_mine

list_matches_mine %>%
  map(
    ~ .x$mtx_similarity %>%
      as_tibble(
        rownames =
          "comparison_occupation"
      ) %>%
      rename(
        similarity = 2
      )
  ) %>%
  bind_rows(
    .id = "model"
  ) %>%
  left_join(
    df_methods
  ) %>%
  relocate(names(
    df_methods
  )) %>%
  group_by(
    model
  ) %>%
  arrange(desc(
    similarity
  ), .by_group = T) %>%
  ungroup() ->
df_matches_mine

df_matches_mine %>%
  group_by(across(
    names(df_methods)
  )) %>%
  reframe(
    max = max(similarity),
    min = min(similarity),
    range = max - min,
    mean = mean(similarity),
    sd = sd(similarity)
  ) %>%
  mutate(
    range_vs_baseline =
      filter(
        ., model ==
          chr_baseline
      ) %>% pull(range),
    range_vs_baseline =
      range /
        range_vs_baseline
  ) %>%
  left_join(
    df_matches_mine %>%
      select(
        model,
        comparison_occupation
      ) %>%
      rename(
        best_match =
          comparison_occupation
      ) %>%
      group_by(
        model
      ) %>%
      slice(1)
  ) %>%
  left_join(
    df_matches_mine %>%
      select(
        model,
        comparison_occupation
      ) %>%
      rename(
        worst_match =
          comparison_occupation
      ) %>%
      group_by(
        model
      ) %>%
      slice(n())
  ) %>%
  arrange(desc(
    range
  )) -> df_models_mine

# df_models_mine %>% filter(!str_detect(model, 'bvls')) %>% print(n = Inf)
df_models_mine %>%
  select(-model) %>%
  print(n = Inf)

# # [MODELS] ----------------------------------------------------------------
# # - Midpoints ----------------------------------------------
# # Use skill set generality as midpoint for attribute equivalence
# # Use skill set competence as midpoint for interchangeability
# df_sample %>%
#   pivot_longer(
#     cols = -1,
#     names_to = "item",
#     values_to = "item_score"
#   ) %>%
#   group_by(
#     occupation
#   ) %>%
#   reframe(
#     generality =
#       fun_gene_generality(
#         item_score
#       ),
#     competence =
#       fun_comp_competence(
#         dbl_profile = item_score,
#         dbl_scale_ub = 100,
#         dbl_scale_lb = 0,
#         dbl_generality = generality
#       )
#   ) -> df_midpoint

# df_midpoint %>%
#   arrange(desc(
#     competence
#   )) %>%
#   print(
#     n = Inf
#   )

# # - Matching methods ------------------------------------------------------
# # Matching methods to apply
# c(
#   "bvls",
#   "logit",
#   "probit",
#   "pearson",
#   "euclidean"
# ) -> chr_methods

# # - Weighting methods ------------------------------------------------------
# # Weighting methods to apply
# c(
#   "linear",
#   "quadratic",
#   "speciality-root",
#   "attribute-eqvl"
# ) -> chr_weights

# # - Overqualification substitution ----------------------------------------
# # Whether or not to apply overqualification substitution
# c(T, F) -> lgc_over_sub

# # - Matching-weights-sub combinations -----------------------------------------
# # Generate all model matching-weights-sub combinations
# crossing(
#   method = chr_methods,
#   weight = chr_weights,
#   sub = lgc_over_sub
# ) -> df_methods

# df_methods %>%
#   mutate(
#     .before = 1,
#     model = paste0(
#       method, "_", weight, if_else(
#         sub, "_sub", ""
#       )
#     ),
#     model = str_replace_all(
#       model, "-", "_"
#     )
#   ) -> df_methods

# # - Define baseline model -------------------------------------------------
# # Most basic matching method
# "euclidean_linear" -> chr_baseline

# [MATCHING] --------------------------------------------------------------
# - My matches ------------------------------------------------------------
# My career matches
Map(
  function(model, method, weight, over_sub) {
    # Apply matching function
    return(
      fun_match_similarity(
        df_data_rows = df_occupations,
        df_query_rows = df_profile_adjusted,
        chr_method = method,
        chr_weights = weight,
        dbl_scale_ub = 100,
        dbl_scale_lb = 0,
        chr_id_col = "occupation",
        lgc_sort = T,
        lgc_overqualification_sub = over_sub
      )
    )
  },
  model = df_methods$model,
  method = df_methods$method,
  weight = df_methods$weight,
  over_sub = df_methods$sub
) -> list_matches_mine

# - Sample occupations' matches ------------------------------------------------------------
# My career matches
Map(
  function(model, method, weight, over_sub) {
    # Apply matching function
    return(
      fun_match_similarity(
        df_data_rows = df_occupations,
        df_query_rows = df_sample,
        chr_method = method,
        chr_weights = weight,
        dbl_scale_ub = 100,
        dbl_scale_lb = 0,
        chr_id_col = "occupation",
        lgc_sort = T,
        lgc_overqualification_sub = over_sub
      )
    )
  },
  model = df_methods$model,
  method = df_methods$method,
  weight = df_methods$weight,
  over_sub = df_methods$sub
) -> list_matches_sample

# [RESULTS] --------------------------------------------------------
# - My models' summary --------------------------------------------------------
# My matches
list_matches_mine %>%
  map(
    ~ .x$mtx_similarity %>%
      as_tibble(
        rownames =
          "comparison_occupation"
      ) %>%
      rename(
        similarity = 2
      )
  ) %>%
  bind_rows(
    .id = "model"
  ) %>%
  left_join(
    df_methods
  ) %>%
  relocate(names(
    df_methods
  )) %>%
  group_by(
    model
  ) %>%
  arrange(desc(
    similarity
  ), .by_group = T) %>%
  ungroup() ->
df_matches_mine

df_matches_mine %>%
  group_by(across(
    names(df_methods)
  )) %>%
  reframe(
    max = max(similarity),
    min = min(similarity),
    range = max - min,
    mean = mean(similarity),
    sd = sd(similarity)
  ) %>%
  mutate(
    range_vs_baseline =
      filter(
        ., model ==
          chr_baseline
      ) %>% pull(range),
    range_vs_baseline =
      range /
        range_vs_baseline
  ) %>%
  left_join(
    df_matches_mine %>%
      select(
        model,
        comparison_occupation
      ) %>%
      rename(
        best_match =
          comparison_occupation
      ) %>%
      group_by(
        model
      ) %>%
      slice(1)
  ) %>%
  left_join(
    df_matches_mine %>%
      select(
        model,
        comparison_occupation
      ) %>%
      rename(
        worst_match =
          comparison_occupation
      ) %>%
      group_by(
        model
      ) %>%
      slice(n())
  ) %>%
  arrange(desc(
    range
  )) -> df_models_mine

# df_models_mine %>% filter(!str_detect(model, 'bvls')) %>% print(n = Inf)
df_models_mine %>%
  select(-model) %>%
  print(n = Inf)

# - Sample occupations models' summary --------------------------------------------------------
# Sample occupations matches
list_matches_sample %>%
  map(
    ~ .x$mtx_similarity %>%
      as_tibble(
        rownames =
          "comparison_occupation"
      )
  ) %>%
  bind_rows(
    .id = "model"
  ) %>%
  pivot_longer(
    cols = any_of(chr_sample),
    names_to = "occupation",
    values_to = "similarity"
  ) %>%
  left_join(
    df_methods
  ) %>%
  relocate(names(
    df_methods
  )) %>%
  mutate(
    occupation = factor(
      occupation,
      levels =
        chr_sample
    )
  ) %>%
  arrange(
    occupation
  ) %>%
  group_by(
    model,
    occupation
  ) %>%
  arrange(desc(
    similarity
  ), .by_group = T) %>%
  ungroup() ->
df_matches_sample

# - Sample occupations' top 10 matches ------------------------------------
# Sample occupations' 10 (11) matches
df_matches_sample %>%
  group_by(
    model,
    occupation
  ) %>%
  slice(1:11) %>%
  ungroup() ->
df_matches_sample_top10

# - Sample occupation' similarity matrix ----------------------------------
# Sample occupations vs sample occupations
df_matches_sample %>%
  select(
    model,
    comparison_occupation,
    occupation,
    similarity
  ) %>%
  filter(if_all(
    .cols = c(
      comparison_occupation,
      occupation
    ),
    .fns = ~ .x %in%
      chr_sample
  )) %>%
  mutate(
    comparison_occupation =
      factor(
        comparison_occupation,
        levels = chr_sample
      )
  ) %>%
  arrange(
    comparison_occupation
  ) -> df_mtx_similarity

df_mtx_similarity %>%
  split(.$model) %>%
  map(
    ~ .x %>%
      select(-model) %>%
      mutate(
        similarity = round(
          similarity,
          2
        )
      ) %>%
      pivot_wider(
        names_from = "occupation",
        values_from = "similarity"
      )
  ) -> list_mtx_similarity

# - Minimize low vs. high level similarity --------------------------------
# Arrange models by how much they minimize
# "low level" occupations' similarity scores vs.
# "high level" occupations
df_midpoint %>%
  rename_with(
    ~ paste0(
      "comparison_",
      .x
    )
  ) %>%
  right_join(
    df_mtx_similarity
  ) %>%
  left_join(
    df_midpoint
  ) %>%
  relocate(any_of(
    names(df_matches_sample)
  )) -> df_matches_sample_midpoint

# Minimize "low level" vs "high level" matching
# <=> maximize correlation between
# competence diff and similarity
df_matches_sample_midpoint %>%
  mutate(
    competence_diff =
      competence -
        comparison_competence
  ) %>%
  group_by(
    model
  ) %>%
  reframe(
    correlation =
      as.numeric(
        wtd.cors(
          competence_diff,
          similarity
        )
      )
  ) %>%
  arrange(desc(
    correlation
  )) -> df_matches_sample_correlation

df_matches_sample_correlation %>% print(n = Inf)

# [PLOTTING] --------------------------------------------------------------
# - My models' plots ------------------------------------------------------
# Similarity density plot
df_matches_mine %>%
  mutate(
    sub = str_detect(
      model, "_sub"
    ),
    model =
      str_remove_all(
        model,
        "_sub"
      )
  ) %>%
  fun_plot.ridges(
    aes(
      x = similarity,
      y = model,
      fill = sub
    ),
    .list_axis.x.args = list(
      limits = c(-0, 1),
      breaks = seq(0, 1, .25)
    ),
    .fun_format.x = percent,
    .list_labs = list(
      title = "Matching Models Comparison",
      subtitle = "For Cao Bittencourt's Career Profile",
      fill = "Overqualification Substitution"
    )
  ) -> plt_density_mine

# Ranges dumbbell plot
df_models_mine %>%
  pivot_longer(
    cols = c(max, min)
    # cols = c(max, mean, min)
    , names_to = "match",
    values_to = "similarity"
  ) %>%
  fun_plot.dumbbell(
    aes(
      x = similarity,
      y = fct_reorder(
        model,
        range
      ),
      color = match
    ),
    .list_axis.x.args = list(
      limits = c(-0, 1),
      breaks = seq(0, 1, .25)
    ),
    .chr_manual.pal = c("blue", "red")
    # , .chr_manual.pal = c('blue', 'yellow', 'red')
    , .reorder_fct = F,
    .fun_format.x = percent,
    .list_labs = list(
      title = "Matching Models Comparison",
      subtitle = "For Cao Bittencourt's Career Profile",
      x = "Similarity",
      color = NULL
    )
  ) -> plt_dumbbell_mine

plt_density_mine

plt_dumbbell_mine

# - Sample occupations models' plots --------------------------------------
# - Career matching (s, ß) -------------------------------------------------------
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations,
  df_query_rows =
    df_profile_adjusted
  # df_sample %>%
  # select(
  #   occupation,
  #   career_cluster,
  #   starts_with('skl_'),
  #   starts_with('abl_'),
  #   starts_with('knw_')
  # ) %>%
  # slice(1:2)
  , chr_method = "euclidean"
  # , chr_method = 'pearson'
  # , chr_method = 'bvls'
  # , chr_method = 'logit'
  # , chr_method = 'probit'
  # , chr_weights = 'linear'
  # , chr_weights = 'quadratic'
  # , chr_weights = ''speciality-root''
  , chr_weights = "attribute-eqvl",
  dbl_scale_ub = 100,
  dbl_scale_lb = 0,
  chr_id_col = "occupation",
  lgc_sort = T,
  lgc_overqualification_sub = F
) -> list_matches

# list_matches$matches %>% map(length)
#
# list_matches
#
# df_profile_adjusted
#
# list_matches$query %>% tail()
# list_matches$data %>% tail()
#
# list_matches$query %>% head(874) %>% tail()
# list_matches$data %>% head(874) %>% tail()

# 0 = shit, 1 = ok, 2 = good, 3 = great, 4 = awesome
# euclidean linear 1
# euclidean quadratic 3
# euclidean 'speciality-root' 2
# euclidean attribute-eqvl 3.5

# pearson linear 2.5
# pearson quadratic 1.5
# pearson 'speciality-root' 2
# pearson attribute-eqvl 1

# bvls linear 0
# bvls quadratic 0
# bvls 'speciality-root' 0
# bvls attribute-eqvl 0.5

# logit/probit linear 2
# logit/probit quadratic 2
# logit/probit 'speciality-root' 2
# logit/probit attribute-eqvl 3

list_matches$
  mtx_similarity %>%
  as_tibble(
    rownames = "comparison_occupation"
  ) %>%
  pivot_longer(
    cols = -1,
    names_to = "occupation",
    values_to = "similarity"
  ) %>%
  left_join(
    df_sample %>%
      select(
        occupation,
        education_years
      ) %>%
      rename(
        years_min = education_years
      ),
    by = c(
      "comparison_occupation" =
        "occupation"
    )
  ) %>%
  left_join(
    df_sample %>%
      select(
        occupation,
        education_years
      ) %>%
      rename(
        years = education_years
      )
  ) %>%
  left_join(
    df_midpoint,
    by = c(
      "comparison_occupation" =
        "occupation"
    )
  ) -> df_match

df_match$similarity[df_match$similarity < 0]

df_match %>%
  mutate(
    years = if_else(
      occupation == "Cao",
      22,
      years
    ),
    ß = fun_intc_ss(
      dbl_similarity = similarity,
      dbl_competence = competence,
      dbl_years = years,
      dbl_years_min = years_min
    ),
    hireability =
      fun_eqvl_bin(ß)
  ) %>%
  select(
    comparison_occupation,
    occupation,
    comp_class,
    gene_class,
    similarity,
    ß,
    hireability
  ) -> df_match

df_match %>%
  split(.$occupation) ->
list_match

list_match %>%
  map(
    arrange,
    similarity
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

list_match %>%
  map(
    arrange,
    -similarity
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

list_match %>%
  map(
    arrange,
    ß
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

list_match %>%
  map(
    arrange,
    -ß
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

df_match %>%
  left_join(
    df_sample %>%
      select(
        occupation,
        employment_variants
      ),
    by = c(
      "comparison_occupation" =
        "occupation"
    )
  ) %>%
  group_by(
    occupation
  ) %>%
  reframe(
    employability = sum(
      weighted.mean(
        # x = hireability,
        # x = ß,
        x = hireability * ß,
        w = employment_variants
      )
    )
  )
