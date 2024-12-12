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

# endregion
# tests
# region: macroflex submodule
test_Phi <- function() {
  list(
    "invalid skill set matrices" = list(
      micro$kflex$Phi(df_occupations) |>
        util$test$is.error()
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
