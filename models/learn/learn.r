# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use()

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# endregion
# model
retirement.age <- 65
starting.age <- 14

# learn <- function(ak, aq, years) {

# }

# t = time iterator
# isEducation = 1 - isWork
edu <- function(t, ttotal, sfield_kq, isEducation) {
  return((1:t) * isEducation * sfield_kq * (sfield_kq > 0.5))
}

xp <- function(t, ttotal, sfield_kq, isEducation) {
  return(((ttotal - t):ttotal) * (1 - isEducation) * sfield_kq * (sfield_kq > 0.5))
}

1:3 |> xp(3, 1, 0)

ak <- 19
aq <- 0

ak + (aq - ak) * (0:19 / 19)
