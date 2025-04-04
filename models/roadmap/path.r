modular::project.options("atlas")
box::use(
  mod / utils / data[last]
)

morph <- function(akz, A, Bkz, fn = mean, ...) {
  # morph a skill set into another skill set
  # akz is the skill set of person k at time z
  # A is the skill set (column vector) matrix
  # Bkz is the "point B" to which k is morphing
  # fn is the morphing function with (...) args
  return(fn |> mapply(akz, A[, Bkz], ...))
}

learn <- function(ẍkz, ẗkz, S, S.eq.geq, Bkz, is.education) {
  # increment experience and education vectors
  # ẍkz is the equivalent experience of person k at time z
  # ẗkz is the equivalent education of person k at time z
  # S is a (column vector) similarity matrix (e.g. field similarity)
  # Bkz is the "point B" to which k is morphing
  # is.education indicates whether each q is a kind of job or education

  if (is.education) {
    return(
      list(
        "xp" = ẍkz,
        "edu" = S[, Bkz] + ẗkz
        # "edu" = S.eq.geq[, Bkz] * S[, Bkz] + ẗkz
      )
    )
  }

  return(
    list(
      "xp" = S[, Bkz] + ẍkz,
      # "xp" = S.eq.geq[, Bkz] * S[, Bkz] + ẍkz,
      "edu" = ẗkz
    )
  )
  # return(
  #   list(
  #     "xp" = S[, Bkz] + ẍkz,
  #     # "xp" = S.eq.geq[, Bkz] * S[, Bkz] + ẍkz,
  #     "edu" = ẗkz
  #   )
  # )
}

path.optimize <- function(ẍkz, ẗkz, uk, S, S.eq.geq, x, t, Bk.star, Bk, is.education) {
  # choose next "point B" in the career progression
  # ẍkz is the equivalent experience of person k at time z
  # ẗkz is the equivalent education of person k at time z
  # uk is the vector of person k's evaluated utility for each q
  # S is a (column vector) similarity matrix (e.g. field similarity)
  # S.eq.geq is a (column vector) evaluated equivalent similarity matrix (e.g. field similarity)
  # x is the vector of experience requirements for all q in 1:2n
  # t is the vector of educational requirements for all q in 1:2n
  # Bk is the vector of all previous "point B's" (i.e. the career path)
  # Bk.star is the optimal or stationary "point B" (i.e. the career goal)
  # is.education indicates whether each q is a job or a kind of education

  # if person k has reached their career goal, stay at the current job
  last(Bk) -> Bkz

  if (!is.null(Bkz)) {
    if (Bkz == Bk.star) {
      return(Bk.star)
    }
  }

  # assess person k's attainment
  (ẍkz[Bk.star] >= x[Bk.star]) -> attained.xp
  (ẗkz[Bk.star] >= t[Bk.star]) -> attained.edu

  print(paste("attained.xp:", attained.xp))
  print(paste("attained.edu:", attained.edu))

  # k is ready for their career goal
  if (all(attained.xp, attained.edu)) {
    return(Bk.star)
  }

  # k is not ready for their career goal
  print("argmax:")
  print(
    (
      (ẍkz >= x) * (ẗkz >= t) * (
        !attained.xp * !is.education +
          !attained.edu * is.education
      ) * S[Bk.star, ] * uk
    )
  )

  (ẍkz >= x) * (ẗkz >= t) * S[Bk.star, ] * uk -> utility

  print("utility:")
  print(utility)


  # (ẍkz >= x) * (ẗkz >= t) * (
  #   !attained.xp * !is.education +
  #     !attained.edu * is.education
  # ) * S[Bk.star, ] * uk -> utility

  # (ẍkz >= x) * (ẗkz >= t) * (
  #   !attained.xp * !is.education +
  #     !attained.edu * is.education
  # ) * S[Bk.star, ] * uk -> utility

  if (all(utility == 0)) {
    return(S[Bk.star, ] * uk)
  }

  return(which.max(utility))

  # return(
  #   which.max(
  #     (ẍkz >= x) * (ẗkz >= t) * (
  #       !attained.xp * !is.education +
  #         !attained.edu * is.education
  #     ) * S[Bk.star, ] * uk
  #   )
  # )

  # (x[Bk.star] > 0)

  # (x[Bk.star] > 0)
  # S[Bk.star, ]


  # (x[Bk.star] > 0)
  # S[Bk.star, ] == 0

  # missing xp => go get xp
  #   cannot get xp if majored in the wrong thing
  # missing edu & xp not required => go get edu => go to labor market
  # missing edu & xp required => go get edu which maximizes xp carryover

  x[Bk.star] > 0
  # (S[Bk.star, ] * !is.education) * (seq_along(is.education) != Bk.star)
  # (1) persue Bk.star, then segway into Bk.star from another role
  # (2) persue other education, then segway into Bk.star from another role

  S[Bk.star, ] * (seq_along(is.education) != Bk.star)
  S[, Bk.star] * (seq_along(is.education) != Bk.star)




  # 1   2   3   4   5   6   (( 7))  8   9   10  11  12  13 (14) # id
  # .21 .23 .45 .66 .11 .11 (( 1)) .21 .23 .45 .66 .11 .11 ( 1) # S[, Bk.star]
  # .14 .65 .66 .44 .62 .55 (( 1)) .14 .65 .66 .44 .62 .55 ( 1) # S[Bk.star, ]
  # 0   0   0   7   0   2   (( 7)) 0   0   0   0   0   0   ( 0) # x
  # 19  28  26  14  18  23  ((26)) 0   0   0   0   0   0   ( 0) # t

  S
  # S[, Bk.star]
  S[Bk.star, ]
  x
  t
  ((t - ẗkz)) * ((x == 0)^(x[Bk.star] > 0)) / S[, Bk.star]
  ((t - ẗkz)) * ((x == 0)^(x[Bk.star] > 0)) / S[Bk.star, ]

  S[Bk.star, ] * ((t - ẗkz)) * ((x == 0)^(x[Bk.star] > 0)) / S[, Bk.star]
  S[Bk.star, ] * ((t - ẗkz)) * ((x == 0)^(x[Bk.star] > 0)) / S[Bk.star, ]

  ((x - ẍkz) + (t - ẗkz)) * ((x == 0)^(x[Bk.star] > 0)) / S[Bk.star, ]
  ((x - ẍkz) + (t - ẗkz)) * ((x == 0)^(x[Bk.star] > 0)) / S[Bk.star, ]
  ((x - ẍkz) + (t - ẗkz)) * ((x == 0)^(x[Bk.star] > 0)) / S[Bk.star, ]
  ((x - ẍkz) + (t - ẗkz)) * ((seq_along(is.education) != Bk.star)^(x[Bk.star] > 0)) / S[Bk.star, ]
  (x - ẍkz) / S

  (x[Bk.star] > 0) * S[Bk.star, ] * (seq_along(is.education) != Bk.star)
  ((x - ẍkz) + (t - ẗkz)) / S[Bk.star, ]


  # (x - ẍkz) / S[,Bk.star]
  # (t - ẗkz) / S[,Bk.star]

  (x - ẍkz) / S[Bk.star, ]
  (t - ẗkz) / S[Bk.star, ]

  (x - ẍkz) / rowMeans(S)
  (t - ẗkz) / rowMeans(S)

  (x - ẍkz) / colMeans(S)
  (t - ẗkz) / colMeans(S)

  # ẍkz - x
  # ẗkz - t
  x - ẍkz
  t - ẗkz

  # alternative (stupid):
  #   if majored in the wrong thing, go back to school


  # return(
  #   which.max(
  #     (ẍkz >= x) * (ẗkz >= t) * (
  #       !attained.xp * !is.education +
  #         !attained.edu * is.education
  #     ) * S[Bk.star, ] * uk
  #   )
  # )
}

path.recursive <- function(ẍkz, ẗkz, uk, S, S.eq.geq, x, t, Bk.star, Bk, is.education, zmax, z) {
  # assert args in main function
  # recursively optimize career path
  if (z < zmax) {
    path.optimize(
      ẍkz = ẍkz,
      ẗkz = ẗkz,
      uk = uk,
      S = S,
      S.eq.geq = S.eq.geq,
      x = x,
      t = t,
      Bk.star = Bk.star,
      Bk = Bk,
      is.education = is.education
    ) -> Bkz

    print(paste("z:", z))
    print(
      list(
        "xp" = ẍkz,
        "edu" = ẗkz,
        "goal" = Bk.star,
        "path" = Bk
      )
    )
    print(
      paste(
        ifelse(
          is.education[Bkz],
          "studying",
          "working"
        ),
        Bkz
      )
    )

    learn(
      ẍkz = ẍkz,
      ẗkz = ẗkz,
      S = S,
      S.eq.geq = S.eq.geq,
      Bkz = Bkz,
      is.education = is.education[Bkz]
    ) -> ẍẗkz.next

    return(
      path.recursive(
        ẍkz = ẍẗkz.next[[1]],
        ẗkz = ẍẗkz.next[[2]],
        uk = uk,
        S = S,
        S.eq.geq = S.eq.geq,
        x = x,
        t = t,
        Bk.star = Bk.star,
        Bk = c(Bk, Bkz),
        is.education = is.education,
        zmax = zmax,
        z = z + 1
      )
    )
  }

  # return career path when the model has reached the maximum number of iterations (years)
  return(
    list(
      "xp" = ẍkz,
      "edu" = ẗkz,
      "goal" = Bk.star,
      "path" = Bk
    )
  )
}

replicate(7, runif(7)) -> S
diag(S) <- 1
rep(F, ncol(S)) -> is.education
c(is.education, !is.education) -> is.education
S |> cbind(S) -> S
S |> rbind(S) -> S

(S^2) > 0.5 -> S.eq.geq

x <- c(runif(7, -5, 10) |> round(), rep(0, 7)) |> pmax(0)
t <- c(runif(7, 14, 28) |> round(), rep(0, 7))

ẍkz <- rep(0, 7 + 7)
ẗkz <- rep(0, 7 + 7)

uk <- rep(1:7 * runif(7), 2)
Bk.star <- 7
Bk <- c()

zmax <- 65
z <- 0

path.recursive(
  ẍkz = ẍkz,
  ẗkz = ẗkz,
  uk = rep(1, 14),
  # uk = uk,
  S = S,
  S.eq.geq = S.eq.geq,
  x = x,
  t = t,
  Bk.star = Bk.star,
  Bk = Bk,
  is.education = is.education,
  zmax = zmax,
  z = z
) -> dsds

dsds
paste("sum(dsds$path == 14):", sum(dsds$path == 14))
x
t
1 * S.eq.geq[Bk.star, ]
S[Bk.star, ]

# path <- function() {
#   # assert args

#   # calculate is.education
#   # calculate S.eq.geq
#   # (S >= 0.5) -> S.eq.geq
# }
