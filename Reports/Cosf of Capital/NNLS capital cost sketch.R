install.packages('nnls')
# install.packages('bvls')

library(nnls)
# library(bvls)

df_occupations.pop %>%
# df_occupations %>%
  mutate(
    wage = annual_wage_2021
    , across(
      .cols = ends_with('.l')
      ,.fns = function(x){100 * x}
      )
    ) %>% 
  select(
     wage
    , ends_with('.l')
  ) -> dsds

dsds %>% 
  select(-wage) %>% 
  as.matrix() -> mtx_dsds

nnls::nnls(
  A = mtx_dsds
  , b = dsds$wage
) -> lalala

lalala$x
lalala$deviance
lalala$residuals
lalala$fitted
lalala$mode
lalala$passive
lalala$bound
lalala$nsetp

rbind(lalala$x) -> mtx_output

colnames(mtx_dsds) -> colnames(mtx_output)

mtx_output %>%
  as_tibble() %>% 
  pivot_longer(
    cols = everything()
    , values_to = 'capital.cost'
    , names_to = 'attribute'
  ) %>% 
  arrange(desc(
    capital.cost
    )) -> df_output
 
df_output %>% 
  view

bvls::bvls(
  A = mtx_dsds
  , b = dsds$wage
  , bl = rep(0, ncol(mtx_dsds))
  , bu = rep(Inf, ncol(mtx_dsds))
) -> lalala

lalala$deviance
lalala$x

lm(
  formula = wage ~ . + 0
  , data = dsds
) -> lalala

# Adjusted R2 = 89.21%
# Professional competencies explain ~ 90% of salary
# p-value of R2 = 0 (< 2.2e-16) 
summary(lalala)

lalala %>% 
  broom::tidy() -> lala

lala %>% 
  # slice(2:n()) %>%
  # mutate(
  #   estimate = estimate + abs(min(estimate))
  # ) %>%
  view
  
  