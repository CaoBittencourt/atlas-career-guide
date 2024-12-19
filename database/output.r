# imports
box::use(tidyr[pivot_wider])

# pivot skill set matrix
read.csv("./output/skill_sets.csv") |>
  pivot_wider(
    names_from = "item",
    values_from = "item_score"
  ) |>
  write.csv(
    "./output/skill_sets.csv",
    row.names = F
  )
