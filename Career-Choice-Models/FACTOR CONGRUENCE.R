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


