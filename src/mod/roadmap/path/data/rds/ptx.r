modular::project.options("atlas")

box::use(
  req = mod / roadmap / path / data / req,
  lab = mod / roadmap / path / data / labor,
  mod / roadmap / path / data / vertices[...],
  bin = mod / utils / bin,
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

prob.y_x <- function(pdf.y_x, x.from, x.to, y.from, y.to, ...) {
  return(
    integrate(
      function(x) {
        integrate(
          pdf.y_x,
          y.from,
          y.to,
          ...
        )[[1]]
      },
      x.from,
      x.to
    )[[1]]
  )
}

prob.xy <- function(px, pdf.y_x, x.from, x.to, y.from, y.to, ...) {
  return(
    prob.y_x(
      pdf.y_x,
      x.from,
      x.to,
      y.from,
      y.to,
      ...
    ) * px
  )
}

pdf.t_x <- function(t, xmean, tsd) {
  return(
    t |> dlnorm(xmean, tsd)
  )
}
pdf.t_x(5, 5, 2)
pdf.t_x(10, 10, 2)

prob.pdf <- function(pdf, lb = -Inf, ub = Inf, ...) {
  return(integrate(pdf, lb, ub, ...)[[1]])
}

prob.pdf.joint <- function(pdf, lb = -Inf, ub = Inf, ...) {
  return(integrate(pdf, lb, ub, ...)[[1]])
}


dsds <- function(x, t.from, t.to, tsd) {
  integrate(pdf.t_x, t.from, t.to, xmean = x, tsd = tsd)[[1]]
}

prob.t_x <- function(x.from, x.to, t.from, t.to, tsd) {
  integrate(
    function(x) {
      integrate(
        pdf.t_x,
        t.from,
        t.to,
        xmean = x,
        tsd = tsd
      )[[1]]
    },
    x.from,
    x.to
  )[[1]]
}

vertices.sample |> slice(1) -> lalala
integrate(
  function(x) {
    integrate(
      pdf.t_x,
      lalala$t,
      lalala$t.to,
      xmean = x,
      tsd = t.kde.sample[t.kde.sample |> between(lalala$t, lalala$t.to)] |> log() |> sd() |> replace_na(0)
    )[[1]]
  },
  0,
  1
)

vertices.sample |>
  group_by(vertex) |>
  reframe(
    tsd = t.kde.sample[t.kde.sample |> between(t, t.to)] |> log() |> sd() |> replace_na(0)
  )



prob.pdf |> Vectorize(c("lb", "ub")) -> prob.pdf

prob.pdf(pdf.t_x, c(0, 5), c(5, 10), xmean = c(5, 10), tsd = c(2.5, 5))

pdf.t_x |> prob.pdf(lb = 0, ub = Inf, xmean = 5, tsd = 1)

vertices |>
  filter(
    occupation == sample.id
  ) |>
  inner_join(
    t.sample |> select(t = from, t.to = to)
  ) |>
  mutate(
    t.to = replace_na(t.to, Inf)
  ) ->
vertices.sample

vertices.sample

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
