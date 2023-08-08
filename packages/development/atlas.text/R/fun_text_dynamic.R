# # [SETUP] ----------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# pkg <- c(
#   'dplyr', 'glue' #Data wrangling
# )
#
# # Activate / install packages
# lapply(pkg, function(x)
#   if(!require(x, character.only = T))
#   {install.packages(x); require(x)})
#
# # Package citation
# # lapply(pkg, function(x)
# #   {citation(package = x)})

# [FUNCTIONS] ------------------------------------------------------
# - Dynamic text data frame function --------------------------------------
fun_text_dynamic <- function(

  # Data frame
  df_text = tibble()
  # Input list
  , list_input = list()
  # NA action
  , chr_na = ''

){

  # Arguments validation
  stopifnot(
    "'df_text' must be a data frame." =
      is.data.frame(df_text)
  )

  stopifnot(
    "'list_input' must be a list of textual inputs." =
      all(
        is.list(list_input)
        , !is.data.frame(list_input)
      )
  )

  stopifnot(
    "'chr_na' must be either NULL or a character element." =
      any(
        !length(chr_na),
        all(
          length(chr_na),
          is.character(chr_na)
        )
      )
  )

  # Glue texts
  df_text %>%
    rowwise() %>%
    mutate(across(
      .cols = !where(is.numeric)
      ,.fns =
        ~ glue_data(
          list_input
          , .x
          , .na = chr_na
          , .trim = T
        )
    )) %>%
    ungroup() -> df_text

    # Output
    return(df_text)

}

# # [TEST] ------------------------------------------------------------------
# # - Test ------------------------------------------------------------------
# library(glue)
# library(dplyr)
#
# fun_text_dynamic(
#   df_text =
#     tibble(
#       dsds = '{dsds} {lalala}'
#     )
#   , list_input =
#     list(
#       dsds = 'ds'
#       , lalala = 'lala'
#     )
#   , chr_na = ''
# )
