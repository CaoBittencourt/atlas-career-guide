# region: imports
box::use(
  mod / labor / employability / misc / Omega[...],
  # mod / labor / employability / misc / pec[...],
  mod / labor / employability / misc / pec_recursive[...],
  stats[weighted.mean]
)

# endregion
# region: discretized productivity (Tkappa)
Tkappa <- function(Tk) {
  return(Tk)
}

# endregion
# region: employability
employability <- function(hk, Tk, ttc, w, p = w, agg = T) {
  # assert args
  stopifnot(
    "'hk' must be a hireability vector in the unit interval the same length as 'Tk', 'w' and 'p'." = all(
      hk |> is.numeric(),
      hk >= 0, hk <= 1
      # ,
      # length(hk) == length(Tk),
      # length(hk) == length(w),
      # length(hk) == length(p)
    )
  )

  stopifnot(
    "'Tk' must be a productivity vector in the unit interval the same length as 'hk', 'w' and 'p'." = all(
      Tk |> is.numeric(),
      Tk >= 0, Tk <= 1
      # ,
      # length(Tk) == length(hk),
      # length(Tk) == length(w),
      # length(Tk) == length(p)
    )
  )

  stopifnot(
    "'ttc' must be a function or a list of functions." = any(
      is.function(ttc),
      all(
        is.list(ttc),
        ttc |> sapply(is.function) |> all()
      )
    )
  )

  stopifnot(
    "'w' must be a vector of employment levels the same length as 'hk', 'Tk' and 'p'." = all(
      w |> is.numeric(),
      w > 0
      # ,
      # length(w) == length(hk),
      # length(w) == length(Tk),
      # length(w) == length(p)
    )
  )

  stopifnot(
    "'p' must be a vector of number of positions (job subtypes) the same length as 'hk', 'Tk' and 'p'." = all(
      p |> is.numeric(),
      any(
        all(p >= 1, p <= w),
        all(p >= 1, is.infinite(p))
      )
      # ,
      # length(p) == length(hk),
      # length(p) == length(Tk),
      # length(p) == length(w)
    )
  )

  stopifnot(
    "'agg' must be either TRUE or FALSE." = all(
      is.logical(agg),
      !is.na(agg)
    )
  )

  if (is.function(ttc)) {
    list(ttc) -> ttc
  }

  # employability in infinitely stratified labor market
  # employability in maximally stratified labor markets
  Tk[p != Inf] <- Tkappa(Tk)
  hk * Omega(0, Tk, ttc) -> employability

  # aggregate employability
  if (agg) {
    weighted.mean(
      x = employability,
      w = w
    ) -> employability
  }

  # return employability
  return(employability)
}

# endregion
# region: exports
box::export(employability)

# endregion
