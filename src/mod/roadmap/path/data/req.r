# setup
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
  high.school = 17,
  associate = 19,
  bachelor = 21,
  master = 23,
  doctorate = 28
) -> education

education |>
  lapply(
    function(t) {
      t - education$high.school
    }
  ) ->
restart

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
"atlas.education" |>
  getOption() |>
  readRDS() |>
  mutate(
    id = row_number()
  ) |>
  select(
    id,
    occupation,
    tmin = education_years
  ) ->
careers.edu

# use job zone to estimate minimum required experience
"atlas.oldata" |>
  getOption() |>
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
box::export(
  education,
  restart,
  experience,
  career.req
)

# endregion
