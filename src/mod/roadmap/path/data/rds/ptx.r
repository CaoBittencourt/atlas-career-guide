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

x |> filter(id == sample.id) -> x.sample

t |> filter(id == sample.id) -> t.sample

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

pdf.t_x <- function(x, t) {
  return(
    # t |> dnorm(mean(c(x, t)))
    t |> dnorm(x, t)
  )
}

vertices.sample |>
  mutate(
    const =
      pdf.t_x |>
        pro$norm.const(
          x, x.to,
          t, t.to
        ),
    prob =
      x.pct *
        pro$prob.y_x(
          pdf.t_x,
          x, x.to,
          t, t.to
        ) / pro$norm.const(
          x, x.to,
          t, t.to
        )
  ) |>
  select(
    occupation,
    vertex,
    x, t,
    prob
  ) ->
vertices.sample

vertices.sample |>
  group_by(occupation) |>
  reframe(
    prob = sum(prob)
  )

vertices.sample |>
  group_by(occupation) |>
  group_split() |>
  lapply(
    function(df) {
      df |>
        plt$plot_ly(
          x = ~x,
          y = ~t,
          z = ~prob,
          intensity = ~prob,
          type = "mesh3d"
        )
    }
  ) ->
plots

plots[[1]]
plots[[2]]

# x.seq <- seq(xmin, xmax, length.out = 25)
# t.seq <- seq(tmin, tmax, length.out = 25)

# tibble(
#   x = x.seq,
#   x.to = x.seq |> lead() |> replace_na(20)
# ) -> x.move

# tibble(
#   t = t.seq,
#   t.to = t.seq |> lead() |> replace_na(20)
# ) -> t.move

# expand.grid(
#   x = x.seq,
#   t = t.seq
# ) -> xt.grid

# x.kde[1, ]$kde[[1]] |> bin$as.pdf() -> x.pdf
# t.kde[1, ]$kde[[1]] |> bin$as.pdf() -> t.pdf

# # pdf.t_x <- function(x, t) {
# #   return(
# #     t |> dnorm(mean(c(x, t)))
# #     # t |> dbeta(mean(c(x, t)), x)
# #   )
# # }

# xt.grid |>
#   inner_join(x.move) |>
#   inner_join(t.move) |>
#   mutate(
#     const = pdf.t_x |>
#       pro$norm.const(
#         x, x.to,
#         t, t.to
#       ),
#     prob =
#       x.pdf |>
#         pro$prob.xy(
#           pdf.t_x,
#           x, x.to,
#           t, t.to
#         )
#   )
# # ->
# # xt.grid
# # xt.grid |>
# plt$plot_ly(
#   x = ~x,
#   y = ~t,
#   z = ~prob,
#   intensity = ~prob,
#   type = "mesh3d"
# )

# xt.grid$prob |> sum()
