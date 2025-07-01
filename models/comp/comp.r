# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
library(dplyr)
library(tidyr)
library(atlas.plot)

box::use(
  desc = describe,
  weights[wtd.cors]
)

# endregion
# region: data
# onet occupations data frame
# getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
getOption("atlas.skills") |> readRDS() -> df_occupations

# labor statistics
getOption("atlas.labor") |> readRDS() -> df_labor

# my preference-adjusted skill set
getOption("atlas.data") |>
  file.path(
    "old",
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

# endregion
# model
# region: estimation methods
# note: comp_method == "mean" with aeq_method == "linear-logistic" makes the most sense
aeq_methods <- c("linear-logistic", "gene-root", "linear")
comp_methods <- c("mean", "cobb-douglas")

expand.grid(
  aeq_method = aeq_methods,
  comp_method = comp_methods
) -> df_models

# endregion
# region: my competence and generality
df_skill_set[-1] |>
  as.numeric() ->
dbl_skill_set

df_models |>
  mutate(
    comp_method = df_models$comp_method,
    aeq_method = df_models$aeq_method,
    comp =
      mapply(
        function(cpm, aem) {
          desc$cp$comp(
            skill_set = dbl_skill_set,
            comp_method = cpm,
            aeq_method = aem
          )
        },
        cpm = comp_method,
        aem = aeq_method
      )
  ) |>
  arrange(-comp) ->
df_comp_mine

df_comp_mine

# endregion
# region: occupations' competence and generality
df_occupations |>
  group_by(occupation) |>
  reframe(
    comp_method = df_models$comp_method,
    aeq_method = df_models$aeq_method,
    gene = desc$gn$gene(item_score),
    comp = mapply(
      function(cpm, aem) {
        desc$cp$comp(
          skill_set = item_score,
          comp_method = cpm,
          aeq_method = aem
        )
      },
      cpm = comp_method,
      aem = aeq_method
    )
  ) |>
  inner_join(df_labor) |>
  arrange(-comp) ->
df_comp_occupations

df_comp_occupations

# endregion
# region: competence vs generality correlation
df_comp_occupations |>
  group_by(comp_method, aeq_method) |>
  reframe(
    gene_comp_corr = wtd.cors(
      gene, comp, employment_norm
    ) |> as.numeric(),
    wage_comp_corr = wtd.cors(
      wage, comp, employment_norm
    ) |> as.numeric(),
    gene_wage_corr = wtd.cors(
      gene, wage, employment_norm
    ) |> as.numeric(),
  ) |>
  arrange(gene_comp_corr) ->
df_corr

df_corr

# endregion
# plots
# region: methods vs competence distribution
df_comp_occupations |>
  fun_plot.density(
    aes(
      x = comp,
      weights = employment_norm
    ),
    .sym_facets = c(aeq_method, comp_method),
    .list_axis.x.args = list(
      limits = c(-.25, 1.25),
      breaks = seq(0, 1, length.out = 7)
    ),
    .fun_format.x = percent,
    .reorder_desc = T
  )

# endregion
