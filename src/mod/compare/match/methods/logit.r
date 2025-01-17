modular::project.options("atlas")
# region: imports
box::use(
  mod / utils / bernoulli[...],
  fastglm[fastglmPure],
  stats[binomial, coef],
  mod / utils / prob[...]
)

# endregion
# region: method name
s.logit <- function(ak, aq, äq = rep(1, length(aq)), link = c("logit", "probit")[[1]]) {
  # assert args
  stopifnot(
    "'link' must be either 'logit' or 'probit'." = all(
      any(link == c("logit", "probit"))
    )
  )

  # convert to bernoulli
  ak |> as.bernoulli(ub = 100, lb = 0) -> ak
  aq |>
    as.bernoulli(ub = 100, lb = 0) |>
    as.matrix() -> aq
  äq |> rep(each = 100 - 0) -> äq

  # run weighted logit or probit regression
  return(
    fastglmPure(
      x = aq,
      y = ak,
      family = binomial(link),
      weights = äq
    )
  )
}

# endregion
# region: generic function

# endregion
# region: exports
box::export()

# endregion
getOption("atlas.skills_mtx") |> readRDS() -> dsds

dsds[-1] -> dsds

dsds[[1]] |> as.bernoulli()
box::use(eq = mod / describe / aeq)
s.logit(dsds[[1]], dsds[[2]], eq$aeq(dsds[[2]]), link = "logit")

dsds[[1]] |> as.bernoulli(ub = 100, lb = 0) -> ak
dsds[[2]] |>
  as.bernoulli(ub = 100, lb = 0) |>
  cbind() -> aq
eq$aeq(dsds[[2]]) |> rep(each = 100 - 0) -> äq

ak |> length()
aq |> nrow()
äq |> length()

fastglmPure(
  x = aq,
  y = ak,
  family = binomial('probit'),
  weights = äq
)

matrix(1, 100, 120) |> c()

fastglm
coef(fastglmPure(
  x = aq,
  y = ak,
  family = binomial(link),
  weights = äq
))

dsds[[1]] -> ak
digits <- 2

(ub * ak) |>
  sapply(
    function(a) {
      rep(
        c(1, 0),
        times = ceiling(c(
          a, (ub - 0) - a
        ))
      )
    }
  )
library(purrr)
all(
  list_c(map(
    .x = as.integer(100 * ak),
    ~ rep(
      c(1, 0),
      times = ceiling(c(
        .x, (100 - 0) - .x
      ))
    )
  )) == as.bernoulli(ak)
)

rm(ak)

# Convert data to a Bernoulli variable
map(
  .x = df_data_cols,
  ~
    as.matrix(list_c(map(
      .x = as.integer(.x),
      ~ rep(
        c(1, 0),
        times = ceiling(c(
          .x, (dbl_scale_ub - dbl_scale_lb) - .x
        ))
      )
    )))
) -> list_data_bernoulli

rm(df_data_cols)

# Logistic regression
if (!length(df_weights)) {
  # Run logistic regression matching without weights
  map_dbl(
    .x = list_data_bernoulli,
    ~ coef(fastglmPure(
      x = .x,
      y = int_query_bernoulli * (
        .x^as.numeric(
          lgc_overqualification_sub
        )
      ),
      family = binomial(
        link = chr_method
      )
    ))
  ) -> dbl_similarity
} else {
  # Repeat df_weights' rows
  df_weights[rep(
    1:nrow(df_weights),
    each =
      dbl_scale_ub -
        dbl_scale_lb
  ), ] -> df_weights

  # Run logistic regression matching with weights
  map2_dbl(
    .x = list_data_bernoulli,
    .y = df_weights,
    ~ coef(fastglmPure(
      x = .x,
      y = int_query_bernoulli * (
        .x^as.numeric(
          lgc_overqualification_sub
        )
      ),
      family = binomial(
        link = chr_method
      ), weights = .y
    ))
  ) -> dbl_similarity
}

# Extract probability
exp(dbl_similarity) /
  (1 + exp(dbl_similarity)) ->
dbl_similarity
