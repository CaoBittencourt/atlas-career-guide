# imports
box::use(tidyr[pivot_wider, as_tibble])

# pivot skill set matrix
read.csv("output/skill_sets.csv") -> skill_sets

skill_sets |> 
  pivot_wider(
    names_from = "item",
    values_from = "item_score"
  ) -> skill_set_mtx

# csv
skill_set_mtx |> write.csv("output/skill_set_mtx.csv", row.names = F)

# rds
dir.create("output/rds/", recursive = T)

skill_sets |>
  as_tibble() |>
  saveRDS("output/rds/skill_sets.rds")

skill_set_mtx |>
  as_tibble() |>
  saveRDS("output/rds/skill_set_mtx.rds")

read.csv("output/education.csv") |>
  as_tibble() |>
  saveRDS("output/rds/education.rds")

read.csv("output/labor.csv") |>
  as_tibble() |>
  saveRDS("output/rds/labor.rds")
