# [SETUP] -----------------------------------------------------------------
# - Parameters ------------------------------------------------------------
# Language
chr_language <- 'en'

# - Workspace -------------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

tryCatch(
  
  load('./kflex_assessment_image.RData')
  
  , error = function(e){
    
    source('./kflex_assessment_setup.R')
    
  }
  
)

# - Packages --------------------------------------------------------------
lapply(
  chr_profile
  , function(x){
    if(!require(
      x, character.only = T
    )){
      install.packages(x)
      require(x)
    }}
)

# - Data ------------------------------------------------------------------
# User data
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
) -> df_input

# df_occupations %>%
#   slice_sample(
#     n = 1
#   ) %>%
#   select(
#     occupation
#     , ends_with('.l')
#   ) -> df_input

# [RESULTS] ----------------------------------------------
# - Estimate capital flexibility (user) ---------------------------------------------
fun_kflex.aggregate(
  .df_data = 
    df_input
  , .df_kflex.items = 
    df_kflex.items
) -> df_input.kflex

# # [TEXT REPORT] -----------------------------------------------------------
# # - Preliminary values for analyses ----------------------------------------------------------------------
# # Preliminary values for analyses
# list(
#   username =
#     df_input$
#     occupation
#   # username
#   , nrow_occupations =
#     df_occupations %>%
#     nrow()
#   , nitems.complete =
#     list_ai.impact$
#     items.impact %>%
#     nrow()
#   , nfactors =
#     list_df_text$
#     factor.model %>%
#     nrow()
#   , factor.names =
#     list_df_text$
#     factor.model$
#     factor.name %>%
#     fun_text.commas(
#       .chr_last.sep =
#         list_df_text$
#         last.comma$
#         text
#     )
#   , chr_most_affected.occupation =
#     df_max.aggregate$
#     occupation %>%
#     fun_text.commas()
#   , pct_most_affected.impact =
#     df_max.aggregate$
#     aggregate.impact %>%
#     percent(
#       accuracy = .01
#     ) %>%
#     str_replace(
#       '-'
#       , '--'
#     )
#   , pct_most_affected.remaining =
#     sum(
#       df_max.aggregate$
#         aggregate.impact
#       , 1
#     ) %>%
#     percent(
#       accuracy = .01
#     )
#   , pct_overall.impact =
#     list_ai.impact$
#     overall.impact$
#     aggregate.impact %>%
#     percent(
#       accuracy = .01
#     ) %>%
#     str_replace(
#       '-'
#       , '--'
#     )
#   , int_unemployment =
#     list_ai.impact$
#     aggregate.impact %>%
#     reframe(sum(
#       -aggregate.impact *
#         weight
#     )) %>%
#     pull() %>%
#     dollar(prefix = '')
#   , pct_user.impact =
#     list_ai.impact.user$
#     overall.impact$
#     aggregate.impact %>%
#     percent(
#       accuracy = .01
#     ) %>%
#     str_replace(
#       '-'
#       , '--'
#     )
# ) -> list_text
# 
# # - List for dictionary evaluation -------------------------------------------------------------------------
# # List for dictionary evaluation
# list(
#   chr_automation.negative =
#     list_ai.impact$
#     items.impact %>%
#     filter(
#       item.impact < 0
#     ) %>%
#     reframe(
#       item.impact =
#         mean(item.impact)
#     ) %>%
#     pull()
#   , most_affected.analysis =
#     df_max.aggregate$
#     aggregate.impact
#   , chr_analysis.panorama =
#     list_ai.impact$
#     overall.impact$
#     aggregate.impact /
#     df_max.aggregate$
#     aggregate.impact
#   , chr_is.isnot =
#     list_ai.impact$
#     overall.impact$
#     aggregate.impact /
#     df_max.aggregate$
#     aggregate.impact
#   , labor_market.analysis =
#     list_ai.impact.user$
#     overall.impact$
#     aggregate.impact
#   , chr_oecd.comparison =
#     list_ai.impact$
#     overall.impact$
#     aggregate.impact /
#     dbl_impact.oecd
#   , chr_user.analysis1 =
#     list_ai.impact.user$
#     overall.impact$
#     aggregate.impact
#   , chr_user.analysis2 =
#     list_ai.impact.user$
#     overall.impact$
#     aggregate.impact
#   , chr_market.comparison =
#     list_ai.impact.user$
#     overall.impact$
#     aggregate.impact /
#     list_ai.impact$
#     overall.impact$
#     aggregate.impact
# ) -> list_scores
# 
# # - Dictionary evaluation --------------------------------------------------------------------
# # Dictionary evaluation
# c(
#   list_text
#   , fun_dictionary.list(
#     .df_dictionary.long =
#       list_df_text$
#       dictionary.eval
#     , .list_dbl_score.eval =
#       list_scores
#   )
# ) -> list_text
# 
# # - Generate dynamic texts ------------------------------------------------
# # Impute dynamic text
# list_df_text[
#   list_df_text %>%
#     map_lgl(
#       ~ 'complexity' %in%
#         names(.x)
#     )
# ] -> list_df_text
# 
# map_if(
#   list_df_text
#   , ~ !any(.x$complexity == 'complex', na.rm = T)
#   , ~ fun_text.dynamic(.x, list_text)
# ) -> list_df_text
# 
# list_df_text$
#   sections %>%
#   mutate(
#     text = if_else(
#       section == 'date'
#       , format(
#         Sys.Date()
#         , text
#       ) , text
#     )) ->
#   list_df_text$
#   sections
# 
# # Text list
# list_df_text$
#   sections$
#   text %>%
#   as.list() ->
#   list_report.texts
# 
# # - Generate remaining text elements ------------------------------------------------
# # Section titles
# list_df_text$
#   sections.title %>%
#   mutate(
#     title =
#       paste(
#         strrep('#', level)
#         , title
#       )) %>%
#   pull(title) %>%
#   as.list() ->
#   list_report.titles
# 
# # Captions
# list_df_text$
#   plots %>%
#   pull(plot.caption) %>%
#   unique() %>%
#   as.list() ->
#   list_plots.caption
# 
# # Text elements
# list_df_text$
#   text.elements %>%
#   pull(title) %>%
#   as.list() ->
#   list_text.elements
# 
# # - Output / Render R Markdown report ----------------------------------------------
# rmarkdown::render(
#   './kflex_assessment_markdown.Rmd'
# )
# 
