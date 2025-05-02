# setup
# region: modular
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  readr[...],
  dplyr[...],
  tidyr[...],
)

# endregion
# data
# region: required education
list(
  high.school = 17 - 17,
  associate = 19 - 17,
  bachelor = 21 - 17,
  master = 23 - 17,
  doctorate = 28 - 17
) -> education

# endregion
# region: required experience
list(
  intern = 0,
  junior = 2,
  associate = 3,
  mid.level = 5,
  senior = 10
) -> experience

# endregion
# region: career requirements
# minimum required education
getOption("atlas.education") |>
  readRDS() |>
  mutate(
    id = row_number(),
    tmin = education_years - 17,
    tmin = pmax(tmin, 0)
  ) |>
  select(
    id,
    occupation,
    tmin
  ) ->
careers.edu

# use job zone to estimate minimum required experience
getOption("atlas.oldata") |>
  read_csv() |>
  mutate(
    xmin = ifelse(id_zone < 2, 0, id_zone)
  ) |>
  select(
    occupation,
    xmin
  ) ->
careers.xp

# career requirements data frame
careers.edu |>
  inner_join(
    careers.xp,
    by = c(
      "occupation" = "occupation"
    )
  ) ->
career.req

# endregion
# exports
# region: exports
education |> saveRDS(getOption("atlas.mod") |> file.path("roadmap", "path", "data", "rds", "education.rds"))
experience |> saveRDS(getOption("atlas.mod") |> file.path("roadmap", "path", "data", "rds", "experience.rds"))
career.req |> saveRDS(getOption("atlas.mod") |> file.path("roadmap", "path", "data", "rds", "career_req.rds"))

# endregion
