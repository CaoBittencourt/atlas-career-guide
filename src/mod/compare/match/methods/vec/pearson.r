modular::project.options("atlas")
# region: imports
box::use(
  weights[wtd.cors]
)

# endregion
# region: pearson correlation matching method
pearson <- function(Ak, A, Ä = rep(1, length(aq))) {
  # assert args in main function
  return((1 + wtd.cors(ak, aq, äq)) / 2)
}
box::use(mod / utils / conform[...])
(getOption("atlas.skills_mtx") |> readRDS())[-1] -> dsds
dsds[2] |> conform(dsds)

mapply(wtd.cors, ak, )

wtd.cors(dsds[[1]], as.matrix(dsds), weight = as.matrix(dsds))
wtd.cors(dsds[[1]], as.matrix(dsds), weight = as.matrix(dsds))

?weights::wtd.cors()
# endregion
# region: exports
box::export(pearson)

# endregion
