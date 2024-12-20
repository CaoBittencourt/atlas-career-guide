# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  cbdg = mod / compare / match / methods / cobb_douglas,
  gmme = mod / compare / match / methods / gmme,
  stringr[str_remove_all]
)

library(dplyr)
library(atlas.aeq)
library(atlas.plot)

# endregion
# region: data
# onet occupations data frame
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# my preference-adjusted skill set
getOption("atlas.data") |>
  file.path(
    "old",
    "questionnaires",
    "questionnaire_Cao.csv"
  ) |>
  read.csv() |>
  as_tibble() |>
  mutate(
    across(
      .cols = c(
        starts_with("skl_"),
        starts_with("abl_"),
        starts_with("knw_")
      ),
      .fns = ~ .x / 100
    )
  ) |>
  rename_with(
    .fn = function(x) {
      x |>
        str_remove_all("^skl_") |>
        str_remove_all("^abl_") |>
        str_remove_all("^knw_")
    }
  ) -> df_skill_set

# endregion
# model
# region: cobb-douglas matching model
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

cbdg$cobb_douglas(
  skill_set = ss,
  skill_mtx = ss_mtx,
  weights = aeq_mtx
) |>
  as_tibble(
    rownames = "occupation"
  ) |>
  rename(
    similarity = 2
  ) |>
  fun_plot.histogram(
    aes(
      x = similarity
    )
  )

cbdg$cobb_douglas(
  skill_set = ss,
  skill_mtx = ss_mtx,
  weights = aeq_mtx,
) |>
  as_tibble(
    rownames = "occupation"
  ) |>
  rename(
    similarity = 2
  ) |>
  fun_plot.density(
    aes(
      x = similarity
    ),
    .list_geom.param = list(
      bw = .1,
      fill = "#290396",
      color = "#212121",
      size = 1.23,
      alpha = 0.77
    ),
    .list_axis.x.args = list(
      limits = c(-.25, 1.25),
      breaks = seq(0, 1, length.out = 7)
    ),
    .fun_format.x = percent,
    .list_labs = list(
      y = ""
    )
  )

cbdg$cobb_douglas(
  skill_set = ss,
  skill_mtx = ss_mtx,
  weights = aeq_mtx,
) |>
  as_tibble(
    rownames = "occupation"
  ) |>
  arrange(-value) |>
  print(n = 25)

# endregion
# region: geometric mean matching model
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

gmme$gmme(
  skill_set = ss,
  skill_mtx = ss_mtx,
  weights = aeq_mtx
) |>
  as_tibble(
    rownames = "occupation"
  ) |>
  rename(
    similarity = 2
  ) |>
  fun_plot.histogram(
    aes(
      x = similarity
    )
  )

gmme$gmme(
  skill_set = ss,
  skill_mtx = ss_mtx,
  weights = aeq_mtx
) |>
  as_tibble(
    rownames = "occupation"
  ) |>
  rename(
    similarity = 2
  ) |>
  fun_plot.density(
    aes(
      x = similarity
    ),
    .list_geom.param = list(
      bw = .1,
      fill = "#290396",
      color = "#212121",
      size = 1.23,
      alpha = 0.77
    ),
    .list_axis.x.args = list(
      limits = c(-.25, 1.25),
      breaks = seq(0, 1, length.out = 7)
    ),
    .fun_format.x = percent,
    .list_labs = list(
      y = ""
    )
  )

gmme$gmme(
  skill_set = ss,
  skill_mtx = ss_mtx,
  weights = aeq_mtx
) |>
  as_tibble(
    rownames = "occupation"
  ) |>
  arrange(-value) |>
  print(n = 25)

# endregion
