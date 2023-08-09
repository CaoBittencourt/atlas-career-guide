fun_best.model.workflow(
  .df_data.numeric = df_occupations.numeric.skill
  , .auto_select.nfactors = T
  # , .int_nfactors.vector = seq(1,4)
  , .int_min.factor_size = 1
  , .chr_rotation = 'oblimin'
  , .dbl_under_loading.threshold = .5
  , .dbl_cross_loading.threshold = .5
  , .remove_under_loading.items = F
  , .remove_cross_loading.items = F
  , .remove_unacceptable_MSAi.items = F
  , .show_diagrams = T
  , .show_results = T
) -> EFA_Skill

fa(
  df_occupations.numeric.skill
  , nfactors = 3
  , rotate = 'varimax'
) -> ds

factanal(
  df_occupations.numeric.skill
  , factors = 6
  , rotation = 'varimax'
) -> dsds

fa.diagram(dsds$loadings) 

EFA_Skill$all.models.reliability
EFA_Skill$best.models.reliability

congruence(EFA_Skill$best.model$model$loadings)
congruence(EFA_Skill$EFA.workflow$EFA$EFA.2Factors$model$loadings)
congruence(EFA_Skill$EFA.workflow$EFA$EFA.3Factors$model$loadings)
congruence(EFA_Skill$EFA.workflow$EFA$EFA.4Factors$model$loadings)
congruence(EFA_Skill$EFA.workflow$EFA$EFA.5Factors$model$loadings)

cohen.profile(EFA_Skill$EFA.workflow$EFA$EFA.2Factors$model$loadings)

cohen.profile(
  EFA_Skill$EFA.workflow$EFA$EFA.2Factors$model$loadings
  , M = c(
    EFA_Skill$EFA.workflow$EFA$EFA.2Factors$model$loadings[,] %>% 
      as_tibble() %>% 
      pull(1) %>% 
      mean()
    , EFA_Skill$EFA.workflow$EFA$EFA.2Factors$model$loadings[,] %>% 
      as_tibble() %>% 
      pull(2) %>% 
      mean()
  )
)

cor(EFA_Skill$EFA.workflow$EFA$EFA.2Factors$model$loadings)

faCor(
  df_occupations.numeric.skill
  , nfactors = c(2,2)
  , rotate = rep('oblimin', 2) 
)

fa(
  df_occupations.numeric.skill
  , nfactors = 2
  , rotate = 'oblimin'
) -> dsds

dsds




congruence(ds$loadings)

cor(ds$loadings)



cohen.profile(ds$loadings)

ds$score.cor

cohen.profile(ds$loadings)

cohen.profile(
  ds$loadings
  , M = c(
    ds$loadings[,] %>% 
      as_tibble() %>% 
      pull(1) %>% 
      mean()
    ,ds$loadings[,] %>% 
      as_tibble() %>% 
      pull(2) %>% 
      mean()
    ,ds$loadings[,] %>% 
      as_tibble() %>% 
      pull(3) %>% 
      mean()
  )
)


congruence(EFA_Skill$EFA.workflow$EFA$EFA.3Factors$model$loadings)
ds$score.cor

cohen.profile(EFA_Skill$best.model$model$loadings)



ds$score.cor

lalala$r

lalala$congruence

faCor(
  # df_occupations.numeric.skill
  EFA_Skill$EFA.workflow$EFA$EFA.3Factors$data
  , nfactors = c(3,3)
  , rotate = rep('promax', 3) 
)




factor.congruence(
  EFA_Skill$best.model$EFA.top.items$model$loadings[,1]
  , EFA_Skill$best.model$EFA.top.items$model$loadings[,2]
)

factor.congruence(
  EFA_Ablt$EFA.workflow$EFA.top.items$EFA.4Factors$model$loadings[,1]
  , EFA_Ablt$EFA.workflow$EFA.top.items$EFA.4Factors$model$loadings[,3]
)

congruence(EFA_Skill$EFA.workflow$EFA.top.items$EFA.2Factors$model$loadings)
congruence(EFA_Ablt$EFA.workflow$EFA.top.items$EFA.4Factors$model$loadings)
congruence(EFA_Know$EFA.workflow$EFA.top.items$EFA.4Factors$model$loadings)

congruence(EFA_Skill$EFA.workflow$EFA$EFA$EFA.2Factors$model$loadings)
congruence(EFA_Ablt$EFA.workflow$EFA$EFA$EFA.4Factors$model$loadings)
congruence(EFA_Know$EFA.workflow$EFA$EFA$EFA.4Factors$model$loadings)

EFA_Skill$best.model$top.items

EFA_Ablt$best.model$top.items

EFA_Know$best.model$top.items


# correlation ------------------------------------------------------------
?congruence()



# draft ------------------------------------------------------------------
# Best models (n factors)
EFA_Skill$best.model$top.items$Factor %>% unique() %>% length()
EFA_Ablt$best.model$top.items$Factor %>% unique() %>% length()
EFA_Know$best.model$top.items$Factor %>% unique() %>% length()

# Adequacy
EFA_Skill$best.model$EFA.top.items$adequacy.tests
EFA_Ablt$best.model$EFA.top.items$adequacy.tests
EFA_Know$best.model$EFA.top.items$adequacy.tests

EFA_Skill$EFA.workflow$EFA.top.items$EFA.2Factors$adequacy.tests
# EFA_Skill$EFA.workflow$EFA.top.items$EFA.3Factors$adequacy.tests

EFA_Ablt$best.model$EFA.top.items$adequacy.tests
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.2Factors$adequacy.tests
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.3Factors$adequacy.tests

EFA_Know$best.model$EFA.top.items$adequacy.tests
EFA_Know$EFA.workflow$EFA.top.items$EFA.3Factors$adequacy.tests


# Reliability
# Top items
# Congruence


