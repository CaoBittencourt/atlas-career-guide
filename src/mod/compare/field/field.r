# region: imports
box::use(
  eq = mod / describe / aeq,
  assert = mod / utils / assert,
  cs = mod / utils / cosine[...],
  mod / utils / vmap[...]
)

# endregion
# region: field similarity
field <- function(aeq_set, aeq_mtx) {
  # assert args
  assert$as.skill_mtx(aeq_set) -> aeq_set
  assert$as.skill_mtx(aeq_mtx) -> aeq_mtx

  # vmap cosine similarity
  return(aeq_set |> vmap(aeq_mtx, cs$cosine.similarity))
}

# field <- function(skill_set, skill_mtx, aeq_method = NULL) {
#   # assert args
#   # assert args
#   assert$valid_skill_set(skill_set)
#   assert$as.skill_mtx(skill_mtx) -> skill_mtx

#   # if attribute equivalence method is provided,
#   # estimate attribute equivalence
#   if (length(aeq_method)) {
#     skill_set |>
#       rbind() |>
#       as.data.frame() |>
#       lapply(eq$aeq, aeq_method = aeq_method) |>
#       bind_rows() ->
#     skill_set

#     skill_mtx |>
#       lapply(eq$aeq, aeq_method = aeq_method) |>
#       bind_rows() ->
#     skill_mtx
#   }

#   # calculate cosine similarity
#   return(cs$cosine.similarity(skill_set, skill_mtx))
# }

# endregion
# region: exports
box::export(field)

# endregion