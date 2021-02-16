library(tidyverse)
# lung <- read_csv('lung.csv', na = c("", "NA","Blank(s)"))
# save(lung, file = 'lung.rdata')
load('lung.rdata')
source('seer_util_fx.R')

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
                 'surv_mo','first_primary','status','yrdx','no_int',
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

### get all the tables for lookup values
cs <- read_csv('CSTables_scrape_updated - CSTables.csv', col_types = 'dcccccc') 

cs_lung <- cs %>% 
     filter(cancer == "Lung") %>% 
     select(short_table, code, descr)

### lung tsize
tsize_tab <- cs_lung %>% 
     filter(short_table == 'tsize',
            as.numeric(code) > 989)

tsize_tab_exp <- expand_seer_cs_table(tsize_tab[,2:3], 989, c('unknown'), names = c('tsize','tsize.n'))

lung2 <- left_join(lung, tsize_tab_exp)

lung <- lung2 %>% 
     select(-tsize) %>% 
     rename(tsize = tsize.n)
rm(lung2)
### lung extent
ext_tab <- cs_lung %>% 
     filter(short_table == 'extent') %>% 
     mutate(extent_clinical = as.numeric(code),
            extent_clinical.n = descr) %>% 
     select(extent_clinical, extent_clinical.n)

lung2 <- left_join(lung, ext_tab)
lung <- lung2 %>% 
     select(-extent_clinical) %>% 
     rename(extent_clinical = extent_clinical.n)


### lung nodes
nodes_tab <- cs_lung %>% 
     filter(short_table == 'nodes') %>% 
     mutate(nodes = as.numeric(code),
            nodes.n = descr) %>% 
     select(nodes, nodes.n)

lung2 <- left_join(lung, nodes_tab)
lung <- lung2 %>% 
     select(-nodes) %>% 
     rename(nodes = nodes.n)


### lung nodes eval
nodes_eval_tab <- cs_lung %>% 
     filter(short_table == 'nodes_eval') %>% 
     mutate(nodes_eval = as.numeric(code),
            nodes_eval.n = descr) %>% 
     select(nodes_eval, nodes_eval.n)

lung2 <- left_join(lung, nodes_eval_tab)
lung <- lung2 %>% 
     select(-nodes_eval) %>% 
     rename(nodes_eval = nodes_eval.n)
###
#surgery treatment
surgTx <- data.frame(code = c(00, 12, 13, 15, 19, 20:25,
                              30, 33, 45:48, 55, 56, 65, 66,
                              70, 80, 90, 99),
                     descr = c('No surg','Laser or cryo','fulguration',
                               'local destruction, NOS', 'local destruction, NOS',
                               'excision less than 1 lobe',
                               'excision, NOS','wedge resection','segmentectomy',
                               'laser excision', 'bronchial sleeve resection',
                               'lobectomy','lobectomy with LND','lobe or bilobectomy, NOS',
                               'lobe/bilobe with chest wall','lobe/bilobe with pericardium',
                               'lobe/bilobe with diaphragm','pneumonectomy','pneumonectomy with LND',
                               'extended pneumonectomy','ext. pneumonec. plus pleura',
                               'extended radical pneumonectomy','lung resection NOS','surgery NOS',
                               'unknown'))
