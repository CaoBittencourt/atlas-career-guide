# conform-bind map
# method |>
#   setNames(method) |>
#   lapply(dispatch) ->
# output
#
# # bind results into data frame
# if (bind) {
#   output |>
#     bind_rows(
#       .id = "match_method"
#     ) ->
#   match.results
# }
#
# return(match.results)

smap <- function(methods, dispatch = seq_along(methods), bind = T, ...) {
  if (!length(names(methods))) {
    "method" |>
      paste0(
        seq_along(methods)
      ) ->
    names(methods)
  }

  methods[dispatch] |>
    lapply(do.call, ...) ->
  output

  if (bind) {
    output |>
      bind_rows(
        .id = names(output)
      ) ->
    output
  }

  return(output)

  # box::use(
  #   dplyr[bind_cols, mutate]
  # )

  # cbindmap <- function(list, fn, to = NULL, ...) {
  #   return(
  #     list |>
  #       lapply(fn, ...) |>
  #       bind_cols() |>
  #       mutate(
  #         .before = 1,
  #         to = to
  #       )
  #   )
  # }
}

box::use(
  mod / compare / match / methods / vec / euclidean[...],
  mod / compare / match / methods / vec / pearson[...],
)



list(
  "euclidean" = euclidean
  # ,
  # "pearson" = pearson
) |>
  smap(
    dispatch = 1,
    bind = T,
    Ak = df_occupations[[1]],
    A = df_occupations[1:3],
    Ä = df_occupations[1:3]
  )
