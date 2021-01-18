### initial prostate data clean
library(tidyverse)
library(janitor)

ps <- read_csv('prostate/prostateList.txt', na = 'Blank(s)')

#pn <- as_tibble(names(ps))

## gets rid of parentheticals in prostate table names
# names1 <- pn %>% 
#      mutate(nm = tolower(str_extract(value, '^(?:(?! \\().)*'))) %>% 
#      select(nm) %>% 
#      .[[1]]



simpnames <- c('age_cat', 'yrdx',
               'hist', 'ajcc6_stage',
               'ajcc6_T','ajcc6_N',
               'ajcc6_M', 'tsize',
               'extent_clinical','ext_eval',
               'nodes','nodes_eval',
               'mets_eval', 'psa','psa_interp',
               'extent_path','apex_involve','gleason_primary_OBS',
               'gleason_pat_needle','gleason_score_turp',
               'gleason_pat_removal','gleason_score_removal',
               'gleason_tert_removal','positive_cores',
               'cores_examined','clinical_staging_procs','cod',
               'seer_cs_death','seer_other_death','surv_mo',
               'seq_no','first_primary','rx_prim_site_surg',
               'rx_scope_reg_ln_surg','rx_surg_other_regdist',
               'rad_seq','reason_no_surg','radiation','chemo',
               'agedx','race','sex','county','vs','modx','end_calc_vs',
               'begin_calc_age','no_int','cum_exp','final_int_exp','final_int_age',
               'final_int_year')

names(ps) <- simpnames

ps <- remove_empty(ps, quiet = F)
## psa_interp, gleason_tert_removal, and core_bx_finding were all empty

### get all the tables for lookup values
cs <- read_csv('CSTables_scrape_updated - CSTables.csv', col_types = 'dcccccc') 

cs_ps <- cs %>% 
     filter(cancer == "Prostate") %>% 
     select(short_table, code, descr)

tsize_tab <- cs_ps %>% 
     filter(short_table == 'tsize',
            as.numeric(code) > 989)

tsize_tab_exp <- expand_seer_cs_table(tsize_tab[,2:3], 989, c('unknown'), names = c('tsize','tsize.n'))

ps2 <- left_join(ps, tsize_tab_exp)
