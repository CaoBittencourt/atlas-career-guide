source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_plots.R')

seq(0,1,0.001) %>% 
  as_tibble() %>%
  rename(
    s = 1
  ) %>%
  mutate(
    Coef.I = s
    #, Coef.II = s ^ 2
    , Coef.II = s ^ 4
    #, Coef.III = s * s + (1 - s) * s ^ 2
    # , Coef.III = s * s + (1 - s) * s ^ 4
    , Coef.III = s * s + (1 - s) * s ^ ((1/s)^(1/s))
    # , Coef.IV = s ^ ((1/s)^((1/s)))
    # , Coef.IV = s ^ ((1/s)^(4*(1/s)))
    # , Coef.IV = s ^ ((1/s)^(4/s))
    , Coef.IV = s ^ ((1/s)^(4/s))
    # , Coef.V = 1 / exp(-(1/s)*(s-(1/s)))
    , Coef.V = s * s + (1 - s) * s ^ Inf
    #, Coef.IV = s ^ ((1/s)^(1/s))
    #, Coef.IV = s * s + (1 - s) * s ^ ((1/s)^(1/s))
    #, Coef.IV = s * s + (1 - s) * s ^ ((1/s)^(4))
    #, Coef.V = s * s + (1 - s) * s ^ ((1/s)^(1/s)^(1/s))
  ) %>% 
  rename(
    similarity = s
  ) %>% 
  pivot_longer(
    cols = starts_with('Coef')
    , names_to = 'coefficient'
    , values_to = 'interchangeability'
  ) %>% 
  fun_plot.line(aes(
    x = similarity
    , y = interchangeability
    , color = coefficient
  )
  , .list_labs = list(
    x = 'Similarity'
    , y = 'Interchangeability'
    , color = 'Coefficient'
  )
  , .fun_format.x = percent
  , .fun_format.y = percent
  , .theme = 
    ggridges::theme_ridges(font_size = 12) + 
    theme(
      legend.position = 'bottom'
      , axis.text.y = element_text(vjust = 0.5)
    )
  )
