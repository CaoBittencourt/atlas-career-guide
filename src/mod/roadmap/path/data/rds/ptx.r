modular::project.options("atlas")

box::use(
  req = mod / roadmap / path / data / req,
  lab = mod / roadmap / path / data / labor,
  mod / roadmap / path / data / vertices[...],
  bin = mod / utils / bin,
  weights[wtd.cors],
  gg = ggplot2,
  plt = plotly,
  # veg = VGAM,
  stats[...],
  dplyr[...],
  tidyr[...],
  # str = stringr,
  # br = betareg,
  # st = rstanarm,
  # bayesplot[...],
  # np = np,
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

x.kde |> plot(xlim = c(0, 20))
t.kde |> plot(xlim = c(0, 20))

x.kde |> bin$sample.kde() -> x.kde.sample
t.kde |> bin$sample.kde() -> t.kde.sample

# notice experience and education are not uncorrelated:
inner_join(
  x |> group_by(id) |> reframe(xmean = from |> bin$as.kde(pct, 0) |> bin$mean.kde()),
  t |> group_by(id) |> reframe(tmean = from |> bin$as.kde(pct, 0) |> bin$mean.kde())
) |>
  inner_join(
    lab$labor
  ) |>
  reframe(
    xt.corr =
      wtd.cors(
        xmean,
        tmean,
        weight = w
      ) |>
        as.numeric()
  )

# therefore, we shouldn't assume the distributions are independent
# i.e. P(t|x) = P(t), P(x|t) = P(x) => P(x,t) = P(t|x) * P(x) = P(t) * P(x)
# thus, we need a theoretical model of how education relates to experience
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

(x.sample$from + .01) |>
  setNames(
    x.sample$type
  ) |>
  lapply(
    function(xmean) {
      tseq |>
        dlnorm(
          xmean |> log(),
          # (0.5 * xmean) |> log(),
          t.kde.sample |>
            sd() |>
            log()
        ) |>
        tidy(tseq) |>
        plot.kde()
    }
  ) ->
plots

plots$intern
plots$junior
plots$associate
plots$mid.level
plots$senior

pdf.t_x <- function(t, xmean, tsd) {
  return(
    t |>
      dlnorm(
        xmean |> log(),
        tsd |> log()
      )
  )
}

prob.pdf <- function(pdf, lb = -Inf, ub = Inf, ...) {
  return(integrate(pdf, lb, ub, ...)[[1]])
}

# prob.pdf |>
#   Vectorize(c("lb", "ub")) ->
# prob.pdf.vec

pdf.t_x |>
  prob.pdf(
    xmean = x.kde.sample |> mean(),
    tsd = t.kde.sample |> sd()
  )

pdf.t_x |>
  prob.pdf(
    req$education$high.school,
    req$education$associate,
    xmean = x.kde.sample |> mean(),
    tsd = t.kde.sample |> sd()
  )

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

vertices.sample |>
  group_by(vertex) |>
  mutate(
    prob =
      x.pct * (
        pdf.t_x |>
          prob.pdf(
            t, t.to,
            xmean = x + .01,
            tsd = sd(t.kde.sample)
          )

      )
  ) |>
  ungroup() ->
vertices.sample

vertices.sample$prob |> sum()

# vertices |>
#   filter(
#     occupation == sample.id
#   ) |>
#   inner_join(
#     t.sample |> select(t = from, t.to = to)
#   ) |>
#   mutate(
#     t.to = replace_na(t.to, Inf),
#     prob = prob.pdf.vec(pdf.t_x, t, t.to, xmean = x, tds = t.kde.sample |> sd())
#     # prob = prob.pdf(pdf.t_x, t, t.to, xmean = x, tds = t.kde.sample |> sd())
#   )
