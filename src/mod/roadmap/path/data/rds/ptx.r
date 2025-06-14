modular::project.options("atlas")


box::use(
  req = mod / roadmap / path / data / req,
  lab = mod / roadmap / path / data / labor,
  mod / roadmap / path / data / vertices[...],
  bin = mod / utils / bin,
  pro = mod / utils / probs,
  weights[wtd.cors],
  gg = ggplot2,
  plt = plotly,
  # veg = VGAM, # tobit regression
  stats[...],
  dplyr[...],
  tidyr[...],
  # str = stringr,
  # br = betareg, # reparametrized beta regression
  # st = rstanarm, # bayesian regression
  # bayesplot[...], # bayesian regression
  # np = np, # non parametric (kernel) regression
)

req$experience$senior -> xmax
req$experience$junior -> xmin

req$education$doctorate -> tmax
req$education$high.school -> tmin

req$onet.bin$x -> x
req$onet.bin$t -> t

sample.id <- 1

x |> filter(id == sample.id) -> x.sample
t |> filter(id == sample.id) -> t.sample

x.sample$from |> bin$as.kde(x.sample$pct, 0) -> x.kde
t.sample$from |> bin$as.kde(t.sample$pct, 0) -> t.kde

# x.kde |> plot(xlim = c(0, 20))
# t.kde |> plot(xlim = c(0, 20))

x.kde |> bin$sample.kde() -> x.kde.sample
t.kde |> bin$sample.kde() -> t.kde.sample

# # notice experience and education are not uncorrelated:
# inner_join(
#   x |> group_by(id) |> reframe(xmean = from |> bin$as.kde(pct, 0) |> bin$mean.kde()),
#   t |> group_by(id) |> reframe(tmean = from |> bin$as.kde(pct, 0) |> bin$mean.kde())
# ) |>
#   inner_join(
#     lab$labor
#   ) |>
#   reframe(
#     xt.corr =
#       wtd.cors(
#         xmean,
#         tmean,
#         weight = w
#       ) |>
#         as.numeric()
#   )

# # therefore, we shouldn't assume the distributions are independent
# # i.e. P(t|x) = P(t), P(x|t) = P(x) => P(x,t) = P(t|x) * P(x) = P(t) * P(x)
# # thus, we need a theoretical model of how education relates to experience

# # if distributions were independent, this is what they would look like
# x$from |>
#   bin$as.kde(x$pct, 0, 20) |>
#   bin$as.pdf() ->
# pdf.x

# t$from |>
#   bin$as.kde(t$pct, 0, 20) |>
#   bin$as.pdf() ->
# pdf.t

# expand.grid(
#   x.id = x.sample$binId,
#   t.id = t.sample$binId
# ) |>
#   inner_join(
#     x.sample |>
#       rename_with(
#         ~ paste0("x.", .x)
#       )
#   ) |>
#   inner_join(
#     t.sample |>
#       rename_with(
#         ~ paste0("t.", .x)
#       )
#   ) |>
#   group_by(row_number()) |>
#   mutate(
#     prob.x = (pdf.x |> integrate(x.from, x.to |> replace_na(Inf)))[[1]],
#     prob.t = (pdf.t |> integrate(t.from, t.to |> replace_na(Inf)))[[1]],
#     pxt = prob.x * prob.t
#   ) |>
#   rename(
#     experience = x.from,
#     education = t.from
#   ) |>
#   ungroup() |>
#   plt$plot_ly(
#     x = ~experience,
#     y = ~education,
#     z = ~pxt,
#     type = "mesh3d",
#     intensity = ~pxt
#   )

seq(tmin, tmax, .01) -> tseq

tidy <- function(d, xseq) {
  return(
    d |>
      as_tibble() |>
      rename(density = 1) |>
      mutate(value = xseq)
  )
}

plot.kde <- function(df) {
  return(
    df |>
      gg$ggplot(
        gg$aes(
          x = value,
          y = density
        )
      ) +
      gg$geom_area(
        alpha = 0.7,
        fill = "red",
        color = "black"
      )
  )
}

# (x.sample$from + .01) |>
#   setNames(
#     x.sample$type
#   ) |>
#   lapply(
#     function(xmean) {
#       tseq |>
#         dlnorm(
#           xmean |> log(),
#           # (0.5 * xmean) |> log(),
#           t.kde.sample |>
#             sd() |>
#             log()
#         ) |>
#         tidy(tseq) |>
#         plot.kde()
#     }
#   ) ->
# plots

# plots$intern
# plots$junior
# plots$associate
# plots$mid.level
# plots$senior





# sum(
#   (x.kde |> bin$as.pdf() |> integrate(req$experience$intern, req$experience$junior))[[1]],
#   (x.kde |> bin$as.pdf() |> integrate(req$experience$junior, req$experience$associate))[[1]],
#   (x.kde |> bin$as.pdf() |> integrate(req$experience$associate, req$experience$mid.level))[[1]],
#   (x.kde |> bin$as.pdf() |> integrate(req$experience$mid.level, req$experience$senior))[[1]],
#   (x.kde |> bin$as.pdf() |> integrate(req$experience$senior, Inf))[[1]]
# )


# x.kde |>
#   bin$as.pdf() |>
#   pro$prob.xy(
#     function(x, t) dnorm(t, x),
#     # function(x, y) mean(c(x, y)),
#     x.from = 0,
#     x.to = 5,
#     y.from = 0,
#     y.to = 5
#   )

# pdf.t_x <- function(t, xmean, tsd) {
#   return(
#     t |> dlnorm(xmean, tsd)
#   )
# }

vertices |>
  filter(
    occupation == sample.id
  ) |>
  inner_join(
    t.sample |> select(t = from, t.to = to)
  ) |>
  inner_join(
    x.sample |> select(x = from, x.to = to)
  ) |>
  mutate(
    t.to = replace_na(t.to, 45),
    x.to = replace_na(x.to, 45)
  ) ->
vertices.sample

vertices.sample

pdf.t_x <- function(x, t) {
  return(dnorm(t, x))
}

vertices.sample |>
  group_by(occupation) |>
  mutate(
    prob =
      pro$prob.y_x(
        pdf.t_x,
        x, x.to,
        t, t.to
      ) / pro$norm.const(
        pdf.t_x,
        min(x), max(x.to),
        min(t), max(t.to)
      )
  ) |>
  reframe(
    prob = sum(prob)
  )


# t.pct(t.lb, t.ub, x) = \int_{t.lb}^{t.ub} pdf(t|x) dt \forall x
# t.pct(t.lb, t.ub) = \int_{-inf}^{+inf} (\int_{t.lb}^{t.ub} pdf(t|x) dt)dx #\forall x

vertices.sample |>
  group_by(x) |>
  # group_by(vertex) |>
  mutate(
    prob =
    # joint probability dist
    # P(x,t) = P(t|x) * P(x)
      x.pct * (
        pdf.t_x |>
          prob.pdf(
            t, t.to,
            xmean = x + .01,
            tsd = sd(t.kde.sample)
          )
      )
  ) |>
  ungroup()
vertices.sample

vertices.sample$prob |>
  sum() |>
  round(4)

# vertices |>
#   filter(
#     occupation == sample.id
#   ) |>
#   inner_join(
#     t.sample |> select(t = from, t.to = to)
#   ) |>
#   mutate(
#     t.to = replace_na(t.to, Inf),
#     prob = prob.pdf.vec(pdf.t_x, t, t.to, xmean = x, tsd = t.kde.sample |> sd())
#     # prob = prob.pdf(pdf.t_x, t, t.to, xmean = x, tsd = t.kde.sample |> sd())
#   )
