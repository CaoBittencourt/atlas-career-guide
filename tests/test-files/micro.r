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
  micro = micro,
  util = utils,
  dplyr[...]
)

# library(dplyr)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_skill_mtx

# labor statistics
getOption("atlas.labor") |> readRDS() -> df_labor

# non-numeric data
df_skill_mtx |>
  select(occupation) |>
  inner_join(
    Sys.getenv("ATLAS_OLD_DATA") |>
      read.csv() |>
      as_tibble()
  ) -> df_occupations

# check if ordered
all(df_skill_mtx$occupation == df_occupations$occupation)
all(df_skill_mtx$occupation == df_labor$occupation)

df_skill_mtx[-1] -> df_skill_mtx

# endregion
# tests
# region: macroflex submodule
test_macroflex <- function() {
  list(
    "invalid skill set matrices" = list(
      "non-numeric skill sets" =
        micro$kflex$macroflex(df_occupations) |>
          util$test$is.error(),
      "skill sets outside the unit interval" =
        df_occupations |>
          select(names(df_skill_mtx)) |>
          micro$kflex$macroflex() |>
          util$test$is.error(),
      "valid skill set data frame" =
        micro$kflex$macroflex(df_skill_mtx) |>
          util$test$is.error() |>
          isFALSE(),
      "valid skill set matrix" =
        df_skill_mtx |>
          as.matrix() |>
          micro$kflex$macroflex() |>
          util$test$is.error() |>
          isFALSE()
    ),
    "invalid weights" = list(
      "null weights" =
        df_skill_mtx |>
          micro$kflex$macroflex(
            weights = NULL
          ) |>
          util$test$is.error() |>
          isFALSE(),
      "invalid weight length" = list(
        df_skill_mtx |>
          micro$kflex$macroflex(
            weights = rep(1, 1000)
          ) |>
          util$test$is.error(),
        df_skill_mtx |>
          micro$kflex$macroflex(
            weights = rep(1, 1)
          ) |>
          util$test$is.error()
      )
    ),
    "output is in the unit interval" = list(
      all(
        df_skill_mtx |> micro$kflex$macroflex() >= 0,
        df_skill_mtx |> micro$kflex$macroflex() <= 1
      ),
      all(
        df_skill_mtx |> as.matrix() |> micro$kflex$macroflex() >= 0,
        df_skill_mtx |> as.matrix() |> micro$kflex$macroflex() <= 1
      ),
      all(
        df_skill_mtx |> micro$kflex$macroflex(weights = df_labor$employment_norm) >= 0,
        df_skill_mtx |> micro$kflex$macroflex(weights = df_labor$employment_norm) <= 1
      ),
      all(
        df_skill_mtx |> as.matrix() |> micro$kflex$macroflex(weights = df_labor$employment_norm) >= 0,
        df_skill_mtx |> as.matrix() |> micro$kflex$macroflex(weights = df_labor$employment_norm) <= 1
      )
    ),
    "output is the same length as the number of attributes in the skill set matrix" = list(
      df_skill_mtx |> micro$kflex$macroflex() |> length() == ncol(df_skill_mtx),
      df_skill_mtx |> as.matrix() |> micro$kflex$macroflex() |> length() == ncol(df_skill_mtx),
      df_skill_mtx |> micro$kflex$macroflex(weights = df_labor$employment_norm) |> length() == ncol(df_skill_mtx),
      df_skill_mtx |> as.matrix() |> micro$kflex$macroflex(weights = df_labor$employment_norm) |> length() == ncol(df_skill_mtx)
    )
  )
}

# endregion
# region: microflex submodule
test_microflex <- function() {
  list(
    "invalid skill set matrices" = list(
      "non-numeric skill sets" =
        micro$kflex$microflex(df_occupations) |>
          util$test$is.error(),
      "skill sets outside the unit interval" =
        df_occupations |>
          select(names(df_skill_mtx)) |>
          micro$kflex$microflex() |>
          util$test$is.error(),
      "valid skill set data frame" =
        micro$kflex$microflex(df_skill_mtx) |>
          util$test$is.error() |>
          isFALSE(),
      "valid skill set matrix" =
        df_skill_mtx |>
          as.matrix() |>
          micro$kflex$microflex() |>
          util$test$is.error() |>
          isFALSE()
    ),
    "invalid weights" = list(
      "null weights" =
        df_skill_mtx |>
          micro$kflex$microflex(
            weights = NULL
          ) |>
          util$test$is.error() |>
          isFALSE(),
      "invalid weight length" = list(
        df_skill_mtx |>
          micro$kflex$microflex(
            weights = rep(1, 1000)
          ) |>
          util$test$is.error(),
        df_skill_mtx |>
          micro$kflex$microflex(
            weights = rep(1, 1)
          ) |>
          util$test$is.error()
      )
    ),
    "output is in the unit interval" = list(
      all(
        df_skill_mtx |> micro$kflex$microflex() >= 0,
        df_skill_mtx |> micro$kflex$microflex() <= 1
      ),
      all(
        df_skill_mtx |> as.matrix() |> micro$kflex$microflex() >= 0,
        df_skill_mtx |> as.matrix() |> micro$kflex$microflex() <= 1
      ),
      all(
        df_skill_mtx |> micro$kflex$microflex(weights = df_labor$employment_norm) >= 0,
        df_skill_mtx |> micro$kflex$microflex(weights = df_labor$employment_norm) <= 1
      ),
      all(
        df_skill_mtx |> as.matrix() |> micro$kflex$microflex(weights = df_labor$employment_norm) >= 0,
        df_skill_mtx |> as.matrix() |> micro$kflex$microflex(weights = df_labor$employment_norm) <= 1
      )
    ),
    "output is the same length as the number of attributes in the skill set matrix" = list(
      df_skill_mtx |> micro$kflex$microflex() |> dim() == ncol(df_skill_mtx),
      df_skill_mtx |> as.matrix() |> micro$kflex$microflex() |> dim() == ncol(df_skill_mtx),
      df_skill_mtx |> micro$kflex$microflex(weights = df_labor$employment_norm) |> dim() == ncol(df_skill_mtx),
      df_skill_mtx |> as.matrix() |> micro$kflex$microflex(weights = df_labor$employment_norm) |> dim() == ncol(df_skill_mtx)
    ),
    "attributes' microflexibility with respect to themselves is 1." = list(
      df_skill_mtx |> micro$kflex$microflex() |> diag() == 1,
      df_skill_mtx |> as.matrix() |> micro$kflex$microflex() |> diag() == 1,
      df_skill_mtx |> micro$kflex$microflex(weights = df_labor$employment_norm) |> diag() == 1,
      df_skill_mtx |> as.matrix() |> micro$kflex$microflex(weights = df_labor$employment_norm) |> diag() == 1
    )
  )
}

# endregion
# region: run all tests
util$test$tests.run(list(
  macroflex = test_macroflex,
  microflex = test_microflex
))

# endregion
