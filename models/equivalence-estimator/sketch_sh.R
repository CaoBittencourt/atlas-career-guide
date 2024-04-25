fun_dmax <- function(x, lb = 0, ub = 100){

  return(sum(pmax(ub - x, x - lb)))

}

df_occupations %>%
  slice(1) %>%
  select(
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) %>%
  as.numeric() ->
  dsds

df_occupations %>%
  slice(1) %>%
  pull(education_years) ->
  taudsds

df_occupations %>%
  slice(2) %>%
  select(
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) %>%
  as.numeric() ->
  lalala

df_occupations %>%
  slice(2) %>%
  pull(education_years) ->
  taulalala

fun_euclidean <- function(ak, aq, lb = 0, ub = 100){

  fun_aeq_aequivalence(aq) -> äq

  sqrt(sum(äq * (ak - aq) ^ 2)) -> dkq

  sqrt(sum(äq * pmax(ub - aq, aq - lb) ^ 2)) -> dmaxq

  return(list(
    ak = ak,
    aq = aq,
    dtilde = dkq / dmaxq,
    dkq = dkq,
    dmaxq = dmaxq,
    skq = 1 - dkq / dmaxq
  ))

}

dsds
lalala
fun_euclidean(
  ak = dsds,
  aq = lalala
) -> list_dkq

fun_seq <- function(list_dkq){

  fun_comp_competence(list_dkq$aq) -> cq

  # list_dkq$skq * ((
  #   1 + cq * list_dkq$dtilde * exp(
  #     - list_dkq$dmaxq * (
  #       list_dkq$dtilde - cq
  #     ) / (1 - cq)
  #   )
  # ) ^ (-cq/list_dkq$dtilde)) -> ß

  # list_dkq$skq * ((
  #   1 + cq * list_dkq$dtilde * exp(
  #     (cq * list_dkq$dmaxq  - 2 * list_dkq$dkq) /
  #       (1 - cq)
  #   )
  # ) ^ (-cq/list_dkq$dtilde)) -> ß

  list_dkq$skq ^ (
    1 / (1 - cq)
  ) -> ß

  return(ß)

}

list_dkq$skq
round(fun_seq(list_dkq), 2)

fun_sh <- function(ak, aq, tauk, tauq){

  fun_aeq_aequivalence(ak) -> äk
  fun_aeq_aequivalence(aq) -> äq

  fun_comp_competence(aq) -> cq

  # sqa
  sum(pmax(aq - ak, 0)) / sum(aq) -> sqa

  # eeq
  sum(äk * äq) / (
    sqrt(sum(äk^2)) *
    sqrt(sum(äq^2))
  ) -> coskq

  (1 + exp(
    # tauq * (
    1 * (
      1 + tauk - tauq
    ) / (1 - cq)
  )) ^ -1 -> teq

  teq * coskq -> eeq

  # seq
  sqrt(
    sum(äq * (ak - aq) ^ 2) /
      sum(äq * pmax(100 - aq, aq - 0) ^ 2)
  ) -> skq

  skq ^ (1 / (1 - cq)) -> seq

  # sh
  sh <- sqa * eeq * seq

  return(list(
    sqa = sqa,
    eeq = eeq,
    coskq = coskq,
    teq = teq,
    seq = seq,
    sh = sh
  ))

}

fun_sh(
  ak = dsds,
  aq = lalala,
  tauk = taudsds,
  tauq = taulalala
) -> list_sh

list_sh
list_sh |> lapply(round, digits = 2)
