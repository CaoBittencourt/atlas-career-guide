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

# endregion
# region: exports
box::export(field)

# endregion