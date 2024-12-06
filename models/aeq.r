library(atlas.aeq)
library(atlas.plot)

seq(0, 1, .001) |>
  as_tibble() |>
  mutate(
    aeq = fun_aeq_aequivalence(value, 1 - .9999999)
  ) |>
  fun_plot.line(
    aes(
      x = value,
      y = aeq
    ),
    .dbl_limits.x = c(0, 1),
    .dbl_limits.y = c(0, 1),
    .fun_format.x = percent,
    .fun_format.y = percent 
  )
