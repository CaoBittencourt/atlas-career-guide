# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  readr[...],
  dplyr[...],
  tidyr[...],
  stringr[...],
  stats[...],
  km = flexclust,
  readxl[read_excel],
)

dplyr::filter -> filter

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
# region: onet data
# similarity matrix
getOption("atlas.mod") |>
  file.path(
    "roadmap",
    "path",
    "data",
    "rds",
    "similarity.rds"
  ) |>
  readRDS() ->
df_similarity

# occupations' ids and soc codes
df_similarity$to |>
  as_tibble() |>
  rename(
    occupation = 1
  ) |>
  mutate(
    .before = 1,
    id = row_number()
  ) |>
  inner_join(
    getOption("atlas.oldata") |>
      read.csv() |>
      select(
        id_soc_code,
        occupation
      ) |>
      arrange(
        occupation
      ) |>
      mutate(
        .before = 1,
        id = row_number()
      )
  ) |>
  relocate(
    -occupation
  ) ->
df_ids

# requirements
getOption("atlas.data") |>
  file.path(
    "db_27_3_excel",
    "Education, Training, and Experience.xlsx"
  ) |>
  read_excel() |>
  dplyr::filter(
    `Scale ID` %in% c("RW", "RL")
  ) ->
df_req

# # (Category, `O*NET-SOC Code`, `Scale ID`) are the composite primary key
# all(df_req |> group_by(Category, `O*NET-SOC Code`, `Scale ID`) |> tally() |> pull(n) == 1)

getOption("atlas.data") |>
  file.path(
    "db_27_3_excel",
    "Education, Training, and Experience Categories.xlsx"
  ) |>
  read_excel() |>
  dplyr::filter(
    `Scale ID` %in% c("RW", "RL")
  ) ->
df_req_cat

tribble(
  ~`Scale ID`, ~`Category`, ~`Category Description`, ~`years`,
  "RL", 1, "Less than a High School Diploma", education$high.school,
  "RL", 2, "High School Diploma - or the equivalent (for example, GED)", education$high.school,
  "RL", 3, "Post-Secondary Certificate - awarded for training completed after high school (for example, in agriculture or natural resources, computer services, personal or culinary services, engineering technologies, healthcare, construction trades, mechanic and repair technologies, or precision production)", education$associate,
  "RL", 4, "Some College Courses", education$associate,
  "RL", 5, "Associate's Degree (or other 2-year degree)", education$associate,
  "RL", 6, "Bachelor's Degree", education$bachelor,
  "RL", 7, "Post-Baccalaureate Certificate - awarded for completion of an organized program of study; designed for people who have completed a Baccalaureate degree but do not meet the requirements of academic degrees carrying the title of Master.", education$master,
  "RL", 8, "Master's Degree", education$master,
  "RL", 9, "Post-Master's Certificate - awarded for completion of an organized program of study; designed for people who have completed a Master's degree but do not meet the requirements of academic degrees at the doctoral level.", education$doctorate,
  "RL", 10, "First Professional Degree - awarded for completion of a program that: requires at least 2 years of college work before entrance into the program, includes a total of at least 6 academic years of work to complete, and provides all remaining academic requirements to begin practice in a profession.", education$doctorate,
  "RL", 11, "Doctoral Degree", education$doctorate,
  "RL", 12, "Post-Doctoral Training", education$doctorate,
  "RW", 1, "None", experience$intern,
  "RW", 2, "Up to and including 1 month", 1 / 12,
  "RW", 3, "Over 1 month, up to and including 3 months", mean(c(1, 3)) / 12,
  "RW", 4, "Over 3 months, up to and including 6 months", mean(c(3, 6)) / 12,
  "RW", 5, "Over 6 months, up to and including 1 year", mean(c(6, 12)) / 12,
  "RW", 6, "Over 1 year, up to and including 2 years", mean(c(1, 2)),
  "RW", 7, "Over 2 years, up to and including 4 years", mean(c(2, 4)),
  "RW", 8, "Over 4 years, up to and including 6 years", mean(c(4, 6)),
  "RW", 9, "Over 6 years, up to and including 8 years", mean(c(6, 8)),
  "RW", 10, "Over 8 years, up to and including 10 years", mean(c(8, 10)),
  "RW", 11, "Over 10 years", mean(c(10, 30)),
) ->
df_req_cat

# endregion
# model
# region: dsds
df_req |>
  mutate(
    .before = 1,
    id_soc_code =
      `O*NET-SOC Code` |>
        str_sub(1, -4)
  ) |>
  mutate(
    pct = `Data Value` / 100
  ) |>
  # dplyr::filter(
  #   pct > 0
  # ) |>
  # dplyr::filter(
  #   `Recommend Suppress` != "Y"
  # ) |>
  inner_join(
    df_req_cat,
    relationship = "many-to-many"
  ) |>
  select(
    scaleId = `Scale ID`,
    id_soc_code,
    onet_soc_code = `O*NET-SOC Code`,
    pct,
    years
  ) ->
onet_req

# get closest match
df_similarity |>
  pivot_longer(
    cols = -1,
    names_to = "from",
    values_to = "similarity"
  ) |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".to")),
    by = c(
      "to" = "occupation.to"
    )
  ) |>
  filter(
    id_soc_code.to %in% onet_req$id_soc_code
  ) |>
  group_by(from) |>
  arrange(
    -similarity,
    .by_group = T
  ) |>
  slice(-1) |>
  slice(1) |>
  ungroup() |>
  inner_join(
    df_ids |> rename_with(.fn = ~ .x |> paste0(".from")),
    by = c(
      "from" = "occupation.from"
    )
  ) ->
df_closest_match

all(
  df_closest_match |>
    filter(
      id.from %in% (df_ids |>
        filter(
          !(id_soc_code %in% onet_req$id_soc_code)
        ) |>
        pull(id))
    ) |>
    pull(id_soc_code.to) %in%
    onet_req$id_soc_code
)

df_closest_match |>
  transmute(
    id = id.from,
    occupation = from,
    id_soc_code = if_else(
      id_soc_code.from %in%
        onet_req$id_soc_code,
      id_soc_code.from,
      id_soc_code.to
    )
  ) ->
df_ids

df_ids |>
  inner_join(
    onet_req,
    relationship = "many-to-many"
  ) |>
  filter(
    onet_soc_code |>
      stringr::str_sub(-2) == "00"
  )

onet_req |>
  group_by(
    onet_soc_code
  ) |>
  group_split() ->
list_req

list_req[[28]] |> print(n = Inf)
list_req[[28]] |> filter(scaleId == "RW") -> dsds

dsds

as.pdf <- function(x, prob, n = 1024) {
  # assert args
  # approximate a probability density function from data
  return(
    density(
      x = x,
      weights = prob,
      n = n
    ) |>
      approxfun(
        yleft = 0,
        yright = 1
      )
  )
}

dsds$years |> as.pdf(dsds$pct) -> dsds.pdf
dsds$years |>
  density(weights = dsds$pct) |>
  plot(xlim = c(0, 25))
dsds.pdf |> plot(xlim = c(0, 25))

density(
  dsds$years,
  weights = dsds$pct,
  n = 1024,
  from = 0
) -> kde

kde$x |>
  sample(
    size = 1024,
    replace = T,
    prob = kde$y
  ) ->
dsdsds

dsdsds |> kmeans(5) -> kme

tibble(
  xp = kme$centers |> as.numeric(),
  pct = kme$size / sum(kme$size)
) |>
  arrange(xp) |>
  mutate(
    .before = 1,
    type = experience |> names()
  )

kde |> plot(from = 0)

# new coefficients: experience vs education relative importance
# note: compare with all careers and weigh by employment levels
#                | high experience      | low experience      |
# high education | rocket science       | education-intensive |
# low education  | experience-intensive | entry level         |

list_req[[239]] |> filter(scaleId == "RL") -> dsds

density(
  dsds$years,
  weights = dsds$pct,
  n = 1024,
  from = 0
) -> kde

kde$x |>
  sample(
    size = 1024,
    replace = T,
    prob = kde$y
  ) ->
dsdsds

dsdsds |> kmeans(5) -> kme

tibble(
  years = kme$centers |> as.numeric(),
  pct = kme$size / sum(kme$size)
  # years = education |> as.numeric(),
  # pct = (dsdsds |> findInterval(education |> as.numeric()) |> table() |> as.numeric()) / length(dsdsds)
) |>
  arrange(years)

kde |> plot(from = 0)

kme$centers |> sort()

kde |> plot()
dsdsds |>
  density() |>
  lines(col = "red")
kde.pdf(1:1024) |>
  density() |>
  lines(col = "blue")

dsdsds |> approxfun() -> kde.pdf

list_req[[1]] |>
  group_by(scaleId) |>
  reframe(
    kde = list(
      density(
        years,
        weights = pct,
        from = 0
      )
    )
    # kde = list(
    #   ks$kde(
    #     years,
    #     w = pct,
    #     xmin = 0
    #   )
    # )
  ) ->
dsds

dsds$kde[[1]] |> plot()

# km$bclust(
#   x = as.data.frame()
#    dsds$kde[[1]]$x,
#   k = 2,
#   weights = dsds$kde[[1]]$y
# )

# km$cclust(
#   dsds$kde[[1]]$x,
#   k = 5,
#   weights = dsds$kde[[1]]$y,
# )



list_req[1] |>
  lapply(
    function(df) {
      df |>
        group_by(`Scale ID`) |>
        reframe(
          kde = list(
            density(
              years,
              weights = pct,
              from = 0
            )
          )
        )
    }
  ) ->
list_kde

list_kde[[1]]$kde[[1]] |> plot()
list_kde[[1]]$kde[[2]] |> plot()

list_req |>
  sum(dsds$pct)

reframe(
  kde = mapply(
    density,
    x = years,
    weights = pct
  )
)

# endregion
