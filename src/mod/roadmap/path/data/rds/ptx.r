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
req$experience$intern -> xmin

req$education$doctorate -> tmax
req$education$high.school -> tmin

req$onet.bin$x -> x
req$onet.bin$t -> t

tibble(
  type = req$experience |> names(),
  x = req$experience |> as.numeric(),
  x.to = req$experience |> as.numeric() |> lead() |> replace_na(45)
) -> x.move

tibble(
  type = req$education |> names(),
  t = req$education |> as.numeric(),
  t.to = req$education |> as.numeric() |> lead() |> replace_na(45)
) -> t.move

sample.id <- c(1, 2)

x |>
  filter(
    id == sample.id
  ) ->
x.sample

t |>
  filter(
    id == sample.id
  ) ->
t.sample

x.sample |>
  group_by(id) |>
  reframe(
    kde =
      from |>
        bin$as.kde(
          pct,
          xmin,
          xmax
        ) |>
        list()
  ) ->
x.kde

t.sample |>
  group_by(id) |>
  reframe(
    kde =
      from |>
        bin$as.kde(
          pct,
          tmin,
          tmax
        ) |>
        list()
  ) ->
t.kde

vertices |>
  filter(
    occupation == sample.id
  ) |>
  inner_join(
    x.move |> select(-type)
  ) |>
  inner_join(
    t.move |> select(-type)
  ) ->
vertices.sample

vertices.sample

pdf.t_x <- function(x, t) {
  return(dnorm(t, x))
}

vertices.sample |>
  inner_join(
    vertices.sample |>
      filter(x.pct > 0) |>
      filter(t.pct > 0) |>
      group_by(occupation) |>
      reframe(
        xmin = min(x),
        tmin = min(t),
        xmax = max(x.to),
        tmax = max(t.to)
      ) |>
      mutate(
        const =
          pdf.t_x |>
            pro$norm.const(
              xmin,
              xmax,
              tmin,
              tmax
            )
      )
  ) |>
  group_by(occupation) |>
  mutate(
    prob =
      pro$prob.y_x(
        pdf.t_x,
        x, x.to,
        t, t.to
      ) / const
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
