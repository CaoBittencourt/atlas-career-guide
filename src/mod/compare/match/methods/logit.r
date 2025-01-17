modular::project.options('atlas')
# region: imports
box::use()

# endregion
# region: method name 

# endregion
# region: generic function

# endregion
# region: exports
box::export()

# endregion
getOption("atlas.skills_mtx") |> readRDS() -> dsds

dsds[-1] -> dsds

as.bernoulli <- function(x){
  # assert args
  if(x >= 0 & x <=1 ){

    as.integer(x * 100)
    
    return(x)
  }

  return(x)


}

dsds[1] -> ak
digits <- 2
ub <- 10 ^ digits
library(purrr)
list_c(map(
  .x = as.integer(ub * ak[,])
  , ~ rep(
    c(1,0), times = ceiling(c(
      .x, (ub - 0) - .x
    ))
  )
))

rm(ak)

# Convert data to a Bernoulli variable
map(
  .x = df_data_cols
  , ~
    as.matrix(list_c(map(
      .x = as.integer(.x)
      , ~ rep(
        c(1,0), times = ceiling(c(
          .x, (dbl_scale_ub - dbl_scale_lb) - .x
        ))
      )
    )))
) -> list_data_bernoulli

rm(df_data_cols)

# Logistic regression
if(!length(df_weights)){

  # Run logistic regression matching without weights
  map_dbl(
    .x = list_data_bernoulli
    , ~ coef(fastglmPure(
      x = .x
      , y = int_query_bernoulli * (
        .x ^ as.numeric(
          lgc_overqualification_sub
        )
      )
      , family = binomial(
        link = chr_method
      )
    ))
  ) -> dbl_similarity

} else {

  # Repeat df_weights' rows
  df_weights[rep(
    1:nrow(df_weights)
    , each =
      dbl_scale_ub -
      dbl_scale_lb
  ), ] -> df_weights

  # Run logistic regression matching with weights
  map2_dbl(
    .x = list_data_bernoulli
    , .y = df_weights
    , ~ coef(fastglmPure(
      x = .x
      , y = int_query_bernoulli * (
        .x ^ as.numeric(
          lgc_overqualification_sub
        )
      )
      , family = binomial(
        link = chr_method
      ), weights = .y
    ))
  ) -> dbl_similarity

}

# Extract probability
exp(dbl_similarity) /
  (1 + exp(dbl_similarity)) ->
  dbl_similarity
