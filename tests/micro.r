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
    atlas.data = "data",
    atlas.occupations = "/data/occupations/df_occupations_2022.csv"
  ),
  root.name = ".atlas"
)

# endregion
# region: imports
box::use(
  micro = mod / micro,
  util = mod / utils,
)

library(dplyr)

# endregion
# region: data
# skill set matrix
getOption("atlas.occupations") |>
  read.csv() |>
  as_tibble() ->
df_occupations

df_occupations |>
  select(
    starts_with("skl_"),
    starts_with("abl_"),
    starts_with("knw_")
  ) |>
  mutate(across(
    .cols = everything(),
    .fns = ~ .x / 100
  )) ->
df_skill_mtx

# endregion
# tests
# region: macroflex submodule
test_Phi <- function() {
  list(
    "invalid skill set matrices" = list(
      "non-numeric skill sets" =
        micro$kflex$Phi(df_occupations) |>
          util$test$is.error(),
      "skill sets outside the unit interval" =
        df_occupations |>
          select(names(df_skill_mtx)) |>
          micro$kflex$Phi() |>
          util$test$is.error(),
      "valid skill set data frame" =
        micro$kflex$Phi(df_skill_mtx) |>
          util$test$is.error() |>
          isFALSE(),
      "valid skill set matrix" =
        df_skill_mtx |>
          as.matrix() |>
          micro$kflex$Phi() |>
          util$test$is.error() |>
          isFALSE()
    ),
    "invalid weights" = list(
      "null weights" =
        df_skill_mtx |>
          micro$kflex$Phi(
            weights = NULL
          ) |>
          util$test$is.error() |>
          isFALSE(),
      "invalid weight length" = list(
        df_skill_mtx |>
          micro$kflex$Phi(
            weights = rep(1, 1000)
          ) |>
          util$test$is.error(),
        df_skill_mtx |>
          micro$kflex$Phi(
            weights = rep(1, 1)
          ) |>
          util$test$is.error()
      )
    ),
    "output is in the unit interval" = list(
      all(
        df_skill_mtx |> micro$kflex$Phi() >= 0,
        df_skill_mtx |> micro$kflex$Phi() <= 1
      ),
      all(
        df_skill_mtx |> as.matrix() |> micro$kflex$Phi() >= 0,
        df_skill_mtx |> as.matrix() |> micro$kflex$Phi() <= 1
      ),
      all(
        df_skill_mtx |> micro$kflex$Phi(weights = df_occupations$employment_norm) >= 0,
        df_skill_mtx |> micro$kflex$Phi(weights = df_occupations$employment_norm) <= 1
      ),
      all(
        df_skill_mtx |> as.matrix() |> micro$kflex$Phi(weights = df_occupations$employment_norm) >= 0,
        df_skill_mtx |> as.matrix() |> micro$kflex$Phi(weights = df_occupations$employment_norm) <= 1
      )
    ),
    "output is the same length as the number of attributes in the skill set matrix" = list(
      df_skill_mtx |> micro$kflex$Phi() |> length() == ncol(df_skill_mtx),
      df_skill_mtx |> as.matrix() |> micro$kflex$Phi() |> length() == ncol(df_skill_mtx),
      df_skill_mtx |> micro$kflex$Phi(weights = df_occupations$employment_norm) |> length() == ncol(df_skill_mtx),
      df_skill_mtx |> as.matrix() |> micro$kflex$Phi(weights = df_occupations$employment_norm) |> length() == ncol(df_skill_mtx)
    )
  )
}

# endregion
# region: run all tests
list(
  macroflex = test_Phi()
  # , microflex = test_phi(),
) -> tests

tests |>
  lapply(
    function(submodule) {
      submodule |>
        unlist() |>
        all()
    }
  )

# endregion
