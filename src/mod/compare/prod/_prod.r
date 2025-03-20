# region: imports
box::use(
  s = mod / compare / match,
  assert = mod / utils / assert
)

# endregion
# region: productivity function
productivity <- function(skill_set, skill_mtx, prod_method = c("cobb-douglas", "gmme")[[1]], bind = T, ...) {
  # assert args
  assert$as.skill_mtx(skill_set) -> Ak
  assert$as.skill_mtx(skill_mtx) -> A

  stopifnot(
    "'prod_method' must be one of the following methods: 'cobb-douglas', 'gmme'." = all(
      prod_method |> vapply(
        function(method) {
          all(any(method == c("cobb-douglas", "gmme")))
        },
        logical(1)
      )
    )
  )

  # multiple dispatch
  if (
    any(prod_method %in% c("cobb-douglas", "gmme"))
  ) {
    return(
      skill_set |>
        s$similarity(
          skill_mtx,
          prod_method,
          bind = bind,
          ...
        )
    )
  }
}

# endregion
# region: exports
box::export(productivity)

# endregion
