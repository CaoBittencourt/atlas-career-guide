# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  pa = roadmap / path,
  gr = igraph,
  dplyr[...],
  stats[setNames]
)

# library(atlas.plot)

# endregion
# region: data
# skill set matrix
# Sys.getenv("ATLAS_SKILLS") |>
#   readRDS() |>
#   filter(
#     occupation %in%
#       (Sys.getenv("ATLAS_EDUCATION") |>
#         readRDS() |>
#         filter(education_years <= 17) |>
#         pull(occupation))
#   ) |>
#   inner_join(
#     Sys.getenv("ATLAS_LABOR") |>
#       readRDS()
#   ) |>
#   group_by(item) |>
#   reframe(
#     item_score = stats::weighted.mean(
#       item_score,
#       employment_norm
#     )
#   ) -> basicEducation

Sys.getenv("ATLAS_SKILLS_MTX") |>
  readRDS() |>
  select(-1) |>
  names() |>
  c("Basic Education") -> occupations

occupations |>
  seq_along() |>
  as.list() |>
  setNames(
    occupations
  ) -> occupations

# endregion
# model
# # simplified model
# region: actor => musician
# occupations
occupations$Actors -> occupation.from
occupations$`Musicians and Singers` -> occupation.to

# find path
occupation.from |> pa$expected$path(occupation.to) -> epath

# career path
epath |>
  pa$expected$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$expected$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$expected$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$expected$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => art director
# occupations
occupations$Actors -> occupation.from
occupations$`Art Directors` -> occupation.to

# find path
occupation.from |> pa$expected$path(occupation.to) -> epath

# career path
epath |>
  pa$expected$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$expected$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$expected$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$expected$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => accountants
# occupations
occupations$Actors -> occupation.from
occupations$`Accountants and Auditors` -> occupation.to

# find path
occupation.from |> pa$expected$path(occupation.to) -> epath

# career path
epath |>
  pa$expected$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$expected$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$expected$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$expected$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: credit analysts => civil engineers
# occupations
occupations$`Credit Analysts` -> occupation.from
occupations$`Civil Engineers` -> occupation.to

# find path
occupation.from |> pa$expected$path(occupation.to) -> epath

# career path
epath |>
  pa$expected$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$expected$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$expected$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$expected$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: high-school => civil engineers
# occupations
occupations$`Basic Education` -> occupation.from
occupations$`Civil Engineers` -> occupation.to

# find path
occupation.from |> pa$expected$path(occupation.to) -> epath

# career path
epath |>
  pa$expected$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$expected$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$expected$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$expected$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: accountant => financial managers
# occupations
occupation.from <- occupations$`Accountants and Auditors`
occupation.to <- occupations$`Financial Managers`

# find path
occupation.from |> pa$expected$path(occupation.to) -> epath

# career path
epath |>
  pa$expected$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$expected$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$expected$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$expected$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# # detailed model
# region: actor => musician
# occupations
occupations$Actors -> occupation.from
occupations$`Musicians and Singers` -> occupation.to

# find path
occupation.from |> pa$detailed$path(occupation.to) -> epath

# career path
epath |>
  pa$detailed$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$detailed$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$detailed$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$detailed$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => art director
# occupations
occupations$Actors -> occupation.from
occupations$`Art Directors` -> occupation.to

# find path
occupation.from |> pa$detailed$path(occupation.to) -> epath

# career path
epath |>
  pa$detailed$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$detailed$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$detailed$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$detailed$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: actor => accountants
# occupations
occupations$Actors -> occupation.from
occupations$`Accountants and Auditors` -> occupation.to

# find path
occupation.from |> pa$detailed$path(occupation.to) -> epath

# career path
epath |>
  pa$detailed$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$detailed$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$detailed$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$detailed$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: credit analysts => civil engineers
# occupations
occupations$`Credit Analysts` -> occupation.from
occupations$`Civil Engineers` -> occupation.to

# find path
occupation.from |> pa$detailed$path(occupation.to) -> epath

# career path
epath |>
  pa$detailed$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$detailed$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$detailed$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$detailed$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: high-school => civil engineers
# occupations
occupations$`Basic Education` -> occupation.from
occupations$`Civil Engineers` -> occupation.to

# find path
occupation.from |> pa$detailed$path(occupation.to) -> epath

# career path
epath |>
  pa$detailed$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$detailed$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$detailed$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$detailed$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
# region: accountant => financial managers
# occupations
occupation.from <- occupations$`Accountants and Auditors`
occupation.to <- occupations$`Financial Managers`

# find path
occupation.from |> pa$detailed$path(occupation.to) -> epath

# career path
epath |>
  pa$detailed$path.timeline() |>
  mutate(
    occupation = names(
      occupations
    )[
      occupation
    ]
  )

# path cost
pa$detailed$path.cost(epath) |> sum() -> pathCost
print(paste("Path cost:", pathCost |> round(2), "years."))

# base cost
occupation.to |> pa$detailed$vertex.cost() -> baseCost
print(paste("Base cost:", baseCost |> round(2), "years."))

# path efficiency
epath |> pa$detailed$path.efficiency() -> pathEfficiency

if (pathEfficiency >= 0) {
  print(
    paste0(
      "Path is optimal and ",
      round(100 * pathEfficiency, 2),
      "% faster than starting from scratch."
    )
  )
} else {
  print(
    paste0(
      "Path is suboptimal and ",
      -round(100 * pathEfficiency, 2),
      "% slower than starting from scratch."
    )
  )
}

# endregion
