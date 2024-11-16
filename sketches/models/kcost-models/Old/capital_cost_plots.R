source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')

chr_attributes <- unique(df_kcost.long$attribute)
names(chr_attributes) <- unique(df_kcost.long$attribute)

map(
  chr_attributes
  , function(x){
    
    df_kcost.long %>% 
      filter(attribute == x) %>%
      ggplot(aes(
        x = level
        , y = capital.cost.m
      )) + 
      geom_point() + 
      geom_smooth() + 
      xlim(c(0,1))
    
  }
) -> dsds

