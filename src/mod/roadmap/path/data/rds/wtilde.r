options(box.path = Sys.getenv("ATLAS_MOD"))

box::use(
  emp = labor / employability,
  req = roadmap / path / data / req,
  lab = roadmap / path / data / labor,
  bin = utils / bin,
  gg = ggplot2,
  ddd = plot3D,
  veg = VGAM,
  weights[wtd.cors],
  stats[...],
  dplyr[...],
  tidyr[...],
  str = stringr,
  plt = plotly,
  br = betareg,
  st = rstanarm,
  bayesplot[...],
  np = np
)

# br$betareg.fit()
# br$betareg()
req$education |>
  as.numeric() |>
  max() -> tmax

req$education |>
  as.numeric() |>
  min() -> tmin

scale.t <- function(t) {
  return(
    (t - tmin) / (tmax - tmin)
  )
}

rescale.t <- function(t) {
  return(
    t * (tmax - tmin) + tmin
  )
}


req$career.req |>
  filter(
    # occupation |> str$str_like("accountants%")
    # occupation |> str$str_like("econ%")
    occupation |> str$str_like("cashier%")
  ) |>
  slice(1) |>
  pull(id) ->
dsds

req$onet.bin$x |> filter(id == dsds) -> x
req$onet.bin$t |> filter(id == dsds) -> t

# conditional probability
# Pr[A,B] = Pr[A|B] * P[B]
# Bayes: Pr[A|B] = Pr[B|A] * Pr[A] / Pr[B]

# Pr[x,t] = Pr[x|t] * P[t] = Pr[t,x] = Pr[t|x] * P[x]
# joint conditional probability of having experience level x AND education level l
# Pr[x|t] # probability of having experience level x, given one has education level l
# Pr[t|x] # probability of having education level l, given one has experience level x

# it makes more sense to model Pr[t|x]
# let us assume Pr[t|x] to be beta-distributed on the [0,tmax] interval as a function of x

# note experience and education and moderately correlated
inner_join(
  req$onet.bin$x |> group_by(id) |> reframe(xmean = bin$as.kde(from, pct, 0) |> bin$mean.kde()),
  req$onet.bin$t |> group_by(id) |> reframe(tmean = bin$as.kde(from, pct, 0) |> bin$mean.kde())
  # req$onet.bin$x |> group_by(id) |> reframe(xmean = weighted.mean(from, pct)),
  # req$onet.bin$t |> group_by(id) |> reframe(tmean = weighted.mean(from, pct))
) |>
  inner_join(
    lab$labor
  ) ->
df_xt.mean

df_xt.mean |>
  reframe(
    xt.corr = wtd.cors(
      xmean, tmean, w
    ) |>
      as.numeric()
  )

# education for its own sake is economically worthless
# thus, experience is the most important metric
# in this sense, education only serve as a certificate for employers and hiring managers
# furthermore, as education (usually) doesn't yield a wage, and often events costs a lot
# in theory, then, workers will try to enter the workforce with as little education as possible
df_xt.mean |>
  gg$ggplot(
    gg$aes(
      x = xmean,
      y = tmean,
      size = w,
      weight = w,
      color = wage
    )
  ) +
  gg$geom_point() +
  gg$scale_color_viridis_c() +
  gg$geom_smooth()

t$from |> bin$as.kde(t$pct, 0) -> kde.t
kde.t |>
  bin$sample.kde() |>
  scale.t() ->
tnorm

vmu <- function(mu) {
  return(mu * (1 - mu))
}

phi <- function(t, mu) {
  return((vmu(mu) / var(t)) - 1)
}

kde.t |> plot()

x$from |>
  bin$as.kde(x$pct, 0) |>
  bin$mean.kde() |>
  scale.t() ->
x.mu

t.kde |>
  scale.t() |>
  phi()

beta.t <- function(t) {
  beta.a <- function(mu, va) {
    return(
      mu * (mu * (1 - mu) / va - 1)
    )
  }

  beta.b <- function(mu, va) {
    return(
      (1 - mu) * (mu * (1 - mu) / va - 1)
    )
  }

  t |> mean() -> mu
  t |> var() -> va
  return(
    beta(
      beta.a(mu, va),
      beta.b(mu, va)
    )
  )
}

t$from |>
  bin$as.kde(t$pct, 0) |>
  bin$sample.kde() ->
t.kde

t.kde |> var()
(t.kde |> mean()) * (1 - (t.kde |> mean()))

t

# np$npregbw(
#   df_xt.mean$tmean ~ df_xt.mean$xmean,
#   weights = df_xt.mean$w
# ) |>
#   np$npreg() ->
# np.model

# loess(
#   tmean ~ xmean,
#   df_xt.mean,
#   weights = w
# ) ->
# loess.model

# lm(
#   tmean ~ log(xmean) + wage,
#   df_xt.mean,
#   weights = w
# ) |>
#   summary()

# lm(
#   tmean ~ xmean + wage,
#   df_xt.mean,
#   weights = w
# ) |>
#   summary()

# lm(
#   tmean ~ log(xmean),
#   df_xt.mean,
#   weights = w
# ) |>
#   summary()

# lm(
#   tmean ~ xmean,
#   df_xt.mean,
#   weights = w
# ) |>
#   summary()

# lm.wfit(
#   df_xt.mean |> mutate(log.xmean = log(xmean)) |> pull(log.xmean) |> cbind(),
#   df_xt.mean$tmean,
#   df_xt.mean$w
# ) |>
#   fitted.values()

# fitted values cannot be negative
# thefore, the tobit regression seems to be the most appropriate model
veg$vglm(
  tmean ~ xmean,
  veg$tobit(),
  df_xt.mean,
  weights = w
) |>
  veg$fitted.values() > 0

veg$vglm(
  tmean ~ xmean,
  veg$tobit(),
  df_xt.mean,
  weights = w
) ->
model_xt.tobit

model_xt.tobit |> veg$coef()
model_xt.tobit |>
  veg$predict(
    # list(xmean = 5)
  ) |>
  as.list() |>
  setNames(
    c("mu", "sd")
  ) ->
lalala


mu.t <- function(x, tmean, ...) {
  return(tmean + 0.6 * x)
}

phi <- function(kde.t, ...) {
  return(kde.t |> bin$sd.kde(...))
}

t$from |>
  bin$as.kde(t$pct, 0, tmax) ->
kde.t

x$from |>
  bin$as.kde(x$pct, 0) ->
kde.x

kde.t |> bin$mean.kde()
kde.t |> bin$sd.kde()


df_xt.mean$tmean[1] + df_xt.mean$xmean[1] * .6


seq(0, 1, .01) |>
  br$dbetar(
    mu = mu.t()
  )

# (
#   seq(0, 1, .01) |>
#     br$dbetar(
#       df_xt.mean$tmean[1] |> scale.t(),
#     )
# ) |>
#   plot()

# lalala$mu |> br$BetaR(lalala$sd)
# seq(0, req$education |> as.numeric() |> max(), .01) |> dnorm(lalala$mu, lalala$sd) |> plot()

# model_xt.tobit |> veg$residuals() |> plot()
# ((model_xt.tobit |> veg$fitted.values()) - df_xt.mean$tmean) |> plot()
x$from |> bin$as.kde(x$pct, 0) -> kde.x
t$from |> bin$as.kde(t$pct, 0) -> kde.t

# nls(
#   # tmean ~ b0 + function(b1) br$BetaR(xmean / 20, b1),
#   # tmean ~ b0 + br$BetaR(xmean / 20, b1),
#   tmean ~ b0 + xmean^b1,
#   data = df_xt.mean,
#   weights = w,
#   lower = 0
# ) ->
# nls.model

st$stan_glm(
  tmean ~ xmean,
  data = df_xt.mean,
  weights = w
) ->
bayes.model

# bayes.model |> as_tibble() |> pull(xmean) |> density() |> plot()

bayes.model |> print()
bayes.model |> mcmc_dens("xmean")
bayes.model |> mcmc_areas("xmean", prob = .9)
bayes.model |> st$posterior_predict()

prob.t_x <- function(x, kde.t) {
  x |>
    mu.t(
      tmean = kde.t |> bin$mean.kde()
    ) ->
  tmu

  kde.t |> phi() -> tphi

  return(
    100 |>
      br$rbetar(
        mu = tmu |> scale.t(),
        phi = tphi |> scale.t()
      )
  )
}

# x$from |> prob.t_x(kde.t) |> rescale.t() |> hist()

# bin$as.kde(x$from, x$pct, 0, x$to |> max(na.rm = T)) |> plot()
# bin$as.kde(t$from, x$pct, 0, t$to |> max(na.rm = T)) |> plot()
x$from |> bin$as.kde(x$pct, 0, 20)
x$from |>
  bin$as.kde(x$pct, 0, 20) |>
  bin$as.pdf() ->
pdf.x
t$from |>
  bin$as.kde(t$pct, 0, 20) |>
  bin$as.pdf() ->
pdf.t

expand.grid(
  x = seq(0, 20, length.out = 100),
  t = seq(0, 20, length.out = 100)
) |>
  # filter(x <= t) |>
  mutate(
    px = x |> pdf.x(),
    pt = t |> pdf.t(),
    px_t = px * pt
  ) |>
  plt$plot_ly(
    x = ~x,
    y = ~t,
    z = ~px_t,
    intensity = ~px_t,
    type = "mesh3d"
  )

# # establish a x vs t trade-off function
# # assess from the data the likehood of each (x,t) pair, given the cost or trade-off
# # z = inverse of prob o landing a job requiring experience x while having education t and experience x (i.e. cost in years)


# # observation: the probability of workers being employed at a certain level of experience is proportional to the inverse cost in years of not having the required education for this same experience level (i.e. trade-off between education and experience)
# # in other words, the most likely workers are those that meet both experience and educational requirements
# # therefore,
# # resoanable axiom: assume de probability of workers being employed at experience level x, given education level t, is exactly the (normalized) inverse of the additional cost function.
# # Pr[x|t] := 1 / xt.cost(x,t),
# # which is in the unit interval
# # if xt.cost = 0 for all (x,t), assume uniform distribution?
# xt.base <- function(t) {
#   return(t)
# }

# xt.add <- function(x, t) {
#   return(0)
# }

# xt.cost <- function(x, t) {
#   return(xt.base(t) + xt.add(x, t))
# }

# xt.pdf <- function(x, t, x.pct, t.pct) {
#   probx.t <- 1 / xt.cost(x, t)
#   return(probx.t)
# }

# xt.pdf(x$from[1], t$from)

# # bayes <- function(pba, pa, pb) {
# #   return(pba * (pa / pb))
# # }

# # x |>
# #   nrow() |>
# #   runif()
# #   bayes(
# #     x$pct[1],
# #     t$pct[1]
# #   )
