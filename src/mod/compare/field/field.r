modular::project.options("atlas")
# region: imports
box::use(
  eq = mod / describe / aeq,
  assert = mod / utils / assert,
  # cs = mod / utils / cosine[...],
  dplyr[...],
  # dplyr[bind_rows],
)

# endregion
# region: field similarity
field <- function(skill_set, skill_mtx, aeq_method = NULL) {
  # assert args
  # assert args
  assert$valid_skill_set(skill_set)
  assert$as.skill_mtx(skill_mtx) -> skill_mtx

  # if attribute equivalence method is provided,
  # estimate attribute equivalence
  if (length(aeq_method)) {
    skill_set |>
      rbind() |> 
      as.data.frame() |> 
      lapply(eq$aeq, aeq_method = aeq_method) |>
      bind_rows() ->
    skill_set

    skill_mtx |>
      lapply(eq$aeq, aeq_method = aeq_method) |>
      bind_rows() ->
    skill_mtx
  }

  # calculate cosine similarity
  return(cs$cosine.similarity(skill_set, skill_mtx))
}

# endregion
# # region: exports
# box::export(field)

# # endregion
# region: test
(getOption("atlas.skills_mtx") |> readRDS())[-1] -> dsds

# dsds[[1]] |> crossprod(as.matrix(dsds))
# dsds |> sapply(function(col){col * dsds[[1]]}) |> dim()

sum(dsds[1:2]^2)
sum(dsds[[1]] * dsds[[2]])
crossprod()
diag(t(crossprod())) / prod(sqrt(diag(crossprod())))
dsds |> as.matrix() |> crossprod(dsds[1:2] |> as.matrix()) |> View()
dsds |> as.matrix() |> crossprod(dsds[1:2] |> as.matrix()) |> diag() |> sqrt() #denominator

crossprod(dsds |> as.matrix()) |> diag() |> sqrt() |> length()

dsds[1:2] |> as.matrix() |> crossprod(dsds |> as.matrix()) |> diag() |> sqrt() |> length()
dsds[1:2] |> as.matrix() |> crossprod(dsds |> as.matrix())

diag(t(crossprod(as.matrix(dsds) |> crossprod()))) / sqrt(diag(as.matrix(dsds) |> crossprod()))
dsds[1:2] |> as.matrix() |> crossprod()
dsds[[1]] |> crossprod(dsds[[1]])
dsds[[1]] |> field(dsds[[1]])
# |> round(2) |> sum()
# dsds[[1]] |> field(dsds, aeq_method = 'linear') |> round(2)

# endregion
