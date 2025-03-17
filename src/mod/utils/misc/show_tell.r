box::use(
  clipr[...],
  dplyr[...]
)

sample(
  c(
    "Caike Ferreira",
    "Elmer Ribeiro",
    "Jamile Queiroz",
    "Lucas Chinaglia de Oliveira",
    "Milena Weidgenant e Silva",
    "Tatiana Frigoletto",
    "Uelinton Santos",
    "Aline Contente",
    "Anderson Gomes da Silva",
    "Cao Bittencourt",
    "Felipe Fittipaldi",
    "Joao Lima",
    "Leticia Scalioni"
  )
) |>
  as_tibble() |>
  rename(
    name = 1
  ) |>
  mutate(
    order = paste0(
      row_number(),
      ". ",
      name
    )
  ) |>
  reframe(
    order |>
      paste0(
        collapse = "\n"
      )
  ) |>
  write_clip()
