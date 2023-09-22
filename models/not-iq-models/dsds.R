library(readr)
library(atlas.acti)
library(atlas.notiq)
library(Hmisc)

df_data <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSj7u2N59j8MTa7MZqk2Y-VDVWIWEDzAR_0gkb_jB_pBX4sm8yMS1N26ClmY6iWXA/pub?gid=145103706&single=true&output=csv')

df_occupations <- read_csv('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023_efa.csv')

efa_model <- read_rds('C:/Users/CAO/Documents/GitHub/atlas-research/data/efa/efa_equamax_14factors.rds')

atlas.acti::fun_acti_type(
  df_data = df_data
  , efa_model = efa_model
  , chr_factor_labels = c(
      'Ds', 'Eg', 'Hs',
      'Mn', 'Tr', 'Ad',
      'So', 'Ah', 'Hz',
      'An', 'Mt', 'Rb',
      'In', 'Mc'
    )
  , chr_data_id = 
    df_data$occupation
  , dbl_scale_lb = 0
) -> df_acti

df_acti$acti_type %>% first() %>% writeClipboard()

atlas.acti::fun_acti_plot_molecule(df_acti)

efa_model %>% 
  atlas.ftools::fun_ftools_factor_match() %>% 
  filter(
    factor %in% paste0(
      'factor', c(1, 11)
      # 'factor', c(1, 10)
      # 'factor', c(1, 10, 11)
      # 'factor', c(10, 11)
      # 'factor', 1
      # 'factor', 10
      # 'factor', 11
    )
  ) %>%
  pull(item) -> 
  chr_notiq

# chr_notiq[-19] -> 
#   chr_notiq

atlas.acti::fun_acti_competency(
  dbl_profile = 
    df_data %>% 
    slice(1) %>% 
    select(-1) %>% 
    as.numeric()
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

atlas.acti::fun_acti_competency(
  dbl_profile = 
    df_data %>% 
    slice(2) %>% 
    select(-1) %>% 
    as.numeric()
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

df_occupations %>% 
  select(
    occupation
    , employment_variants
    , all_of(chr_notiq)
  ) %>% 
  pivot_longer(
    cols = starts_with('item')
  ) %>% 
  reframe(
    mean = wtd.mean(value, employment_variants),
    sd = sqrt(wtd.var(value, employment_variants))
  ) -> df_notiq

atlas.notiq::fun_notiq_quotient(
  dbl_proxy_scores = 
    df_data %>% 
    select(any_of(
      chr_notiq
    )) %>% 
    slice(2) %>%
    as.numeric()
  , dbl_proxy_mean = 
    df_notiq$mean
  , dbl_proxy_sd = 
    df_notiq$sd
  , dbl_iq_mean = 100
  , dbl_iq_sd = 15
)

(
  128 /
atlas.notiq::fun_notiq_quotient(
  dbl_proxy_scores = 
    df_data %>% 
    select(any_of(
      chr_notiq
    )) %>% 
    slice(2) %>%
    as.numeric()
  , dbl_proxy_mean = 
    df_notiq$mean
  , dbl_proxy_sd = 
    df_notiq$sd
  , dbl_iq_mean = 100
  , dbl_iq_sd = 15
  )
) - 1

map_df(
  100 * round(seq(0, 1, length.out = 7), 2) %>% 
    set_names(100 * round(seq(0, 1, length.out = 7), 2))
  , ~ atlas.notiq::fun_notiq_quotient(
    dbl_proxy_scores = .x
    , dbl_proxy_mean = df_notiq$mean
    , dbl_proxy_sd = df_notiq$sd
    , dbl_iq_mean = 100
    , dbl_iq_sd = 15
  )
)
