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
  desc = mod / describe,
  util = mod / utils
)

# endregion
# region: data
# hyper-generalist skill set
runif(1) |> rep(120) -> hyper_generalist

# hyper-specialist skill set
runif(1) |> c(rep(0, 119)) -> hyper_specialist

# endregion
# tests
# region: attribute equivalence submodule
test_aeq <- function() {
  return(
    list(
      "equivalent attributes form a valid skill set" =
        runif(120) |>
          desc$eq$aeq() |>
          util$assert$valid_skill_set() |>
          util$test$is.error() |>
          isFALSE(),
      "invalid skill sets attribute equivalence" = list(
        "attribute > 1" =
          c(rep(0, 119), 1.1) |>
            desc$eq$aeq() |>
            util$test$is.error(),
        "attribute < 0" =
          c(rep(0, 119), -0.1) |>
            desc$eq$aeq() |>
            util$test$is.error(),
        "attribute == Inf" =
          c(rep(0, 119), Inf) |>
            desc$eq$aeq() |>
            util$test$is.error(),
        "attribute == NA" =
          c(rep(0, 119), NA) |>
            desc$eq$aeq() |>
            util$test$is.error(),
        "attribute not numeric" =
          c(rep(0, 119), "dsds") |>
            desc$eq$aeq() |>
            util$test$is.error()
      ),
      "unit vector attribute equivalence" =
        all(
          rep(1, 120) |> desc$eq$aeq() == 1,
          rep(1, 120) |> desc$eq$aeq() |> length() == 120
        ),
      "null vector attribute equivalence" =
        all(
          rep(0, 120) |> desc$eq$aeq() == 1,
          rep(0, 120) |> desc$eq$aeq() |> length() == 120
        ),
      "hyper-generalist attribute equivalence" =
        all(
          hyper_generalist |> desc$eq$aeq() == 1,
          hyper_generalist |> desc$eq$aeq() |> length() == 120
        ),
      "hyper-specialist attribute equivalence" =
        all(
          hyper_specialist |> desc$eq$aeq() == c(1, rep(0, 119)),
          hyper_specialist |> desc$eq$aeq() |> length() == 120
        )
    )
  )
}

# endregion
# region: generality submodule
test_gene <- function() {
  return(
    list(
      "invalid skill sets generality" = list(
        "attribute > 1" =
          c(rep(0, 119), 1.1) |>
            desc$gn$gene() |>
            util$test$is.error(),
        "attribute < 0" =
          c(rep(0, 119), -0.1) |>
            desc$gn$gene() |>
            util$test$is.error(),
        "attribute == Inf" =
          c(rep(0, 119), Inf) |>
            desc$gn$gene() |>
            util$test$is.error(),
        "attribute == NA" =
          c(rep(0, 119), NA) |>
            desc$gn$gene() |>
            util$test$is.error(),
        "attribute not numeric" =
          c(rep(0, 119), "dsds") |>
            desc$gn$gene() |>
            util$test$is.error()
      ),
      "hyper-generalist generality" =
        all(
          hyper_generalist |> desc$gn$gene() == 1,
          hyper_generalist |> desc$gn$gene() |> length() == 1
        ),
      "hyper-specialist generality" =
        all(
          hyper_specialist |> desc$gn$gene() == 0,
          hyper_specialist |> desc$gn$gene() |> length() == 1
        ),
      "unit vector generality" =
        all(
          desc$gn$gene(rep(1, 120)) == 1,
          desc$gn$gene(rep(1, 120)) |> length() == 1
        ),
      "null vector generality" =
        all(
          desc$gn$gene(rep(0, 120)) == 1,
          desc$gn$gene(rep(0, 120)) |> length() == 1
        )
    )
  )
}

# endregion
# region: competence submodule
test_comp <- function() {
  return(
    list(
      "invalid skill sets competence" = list(
        "attribute > 1" =
          c(rep(0, 119), 1.1) |>
            desc$cp$comp() |>
            util$test$is.error(),
        "attribute < 0" =
          c(rep(0, 119), -0.1) |>
            desc$cp$comp() |>
            util$test$is.error(),
        "attribute == Inf" =
          c(rep(0, 119), Inf) |>
            desc$cp$comp() |>
            util$test$is.error(),
        "attribute == NA" =
          c(rep(0, 119), NA) |>
            desc$cp$comp() |>
            util$test$is.error(),
        "attribute not numeric" =
          c(rep(0, 119), "dsds") |>
            desc$cp$comp() |>
            util$test$is.error()
      ),
      "hyper-generalist competence" =
        all(
          hyper_generalist |> desc$cp$comp() == hyper_generalist[[1]],
          hyper_generalist |> desc$cp$comp() |> length() == 1
        ),
      "hyper-specialist competence" =
        all(
          hyper_specialist |> desc$cp$comp() == max(hyper_specialist),
          hyper_specialist |> desc$cp$comp() |> length() == 1
        ),
      "unit vector competence" =
        all(
          desc$cp$comp(rep(1, 120)) == 1,
          desc$cp$comp(rep(1, 120)) |> length() == 1
        ),
      "null vector competence" =
        all(
          desc$cp$comp(rep(0, 120)) == 0,
          desc$cp$comp(rep(0, 120)) |> length() == 1
        )
    )
  )
}

# endregion
# region: run all tests
list(
  aeq = test_aeq(),
  gene = test_gene(),
  comp = test_comp()
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