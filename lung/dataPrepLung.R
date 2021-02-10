library(tidyverse)
lung <- read_csv('lung/lung.csv', na = c("", "NA","Blank(s)"))

long.names <- tolower(make.names(names(lung)))

pn <- as_tibble(names(lung))

## gets rid of parentheticals in prostate table names
 names1 <- pn %>%
      mutate(nm = tolower(str_extract(value, '^(?:(?! \\().)*'))) %>%
      select(nm) %>%
      .[[1]]
short.names <- c('age','sex','location','hist',
                 'hist.group','ajcc6_stage',
                 'ajcc6_T','ajcc6_N',
                 'ajcc6_M','surg.primary','tsize','extent_clinical',
                 'nodes','mets','ext_eval','nodes_eval',
                  'mets_eval','tumor_nodule_ipsi','pleural_elastic','cod','seer_cs_death',
                 'surv_mo','first_primary','race','status','yrdx','status_adj','no_int',
                 'cum_exp', 'final_int_exp','final_int_year')


######
### SSF1: Separate Tumor Nodules - Ipsilateral Lung
### 000 = No separate tumor nodules noted
### 010  = Separate nodules, ipsilateral lung, same lobe
### 020 = Separate tumor nodules in ipsilaterl lun, different lobe
#### 030 = both 020 and 010
#### 040 = Separate nodules, same lung, unknown if same lobe
### 888 = Obsolete
### 988 = NA
### 999 = Unk

######
# SSF2: Pleural or elastic layer invation
# 000: PL0 - no evidence of pleural invasion
# 010: PL1 - Extends through elastic layer
# 020: PL2 - Invasion to surface of visceral pleura
# 030: PL3 - Invades to parietal pleura
# 040: PL4 - Invasion of pleura, NOS
# 888: Obsolete
# 988: NA
# 998: No histologic examination
# 999: Unkown if pleural invasion present


names(lung) <- short.names

