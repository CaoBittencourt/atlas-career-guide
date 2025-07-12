c(
  "201" = 1000 * 2,
  "202" = 1000 * 2,
  "203" = 1000 * 4,
  "204" = 1000 * 4,
  "205" = 1000 * 0,
  "206" = 1000 * 4,
  "301" = 1000 * 4,
  "302" = 1000 * (2 + 2),
  "303" = 1000 * 0,
  "304" = 1000 * 0,
  "305" = 1000 * 0,
  "306" = 1000 * 4,
  "401" = 1000 * 4,
  "402" = 1000 * 0,
  "403" = 1000 * 0,
  "404" = 1000 * 4,
  "405" = 1000 * 7,
  "406" = 1000 * 4
) -> individual

.2 -> adv

sum(individual) -> agregado

agregado * (1 + adv) -> acordo

list(
  "individual" = individual,
  "agregado" = agregado,
  "adv" = agregado * adv,
  "acordo" = acordo
)

list(
  "individual" = (60000 / acordo) * individual,
  "agregado" = (60000 / acordo) * agregado,
  "adv" = (60000 / acordo) * agregado * adv,
  "acordo" = (60000 / acordo) * acordo
) -> valores

list(
  "individual" = individual * (100000 / acordo),
  "agregado" = agregado * (100000 / acordo),
  "adv" = agregado * .3 * (100000 / acordo),
  "acordo" = agregado * 1.3 * (100000 / acordo)
) -> valores

valores$individual |>
  tibble::as_tibble(rownames = "apt") |>
  rename(indenização = value) |>
  clipr::write_clip()

apt	indenização
201	3546.09929078014
202	3546.09929078014
203	7092.19858156000028
204	7092.19858156000028
205	0
206	7092.19858156000028
301	7092.19858156000028
302 7092.19858156000028
303	0
304	0
305	0
306	7092.19858156000028
401	7092.19858156000028
402	0
403	0
404	7092.19858156000028
405	12411.3475177305
406	7092.19858156000028