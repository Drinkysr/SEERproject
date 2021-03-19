### Utility functions for SEER 

###
# translate variables when most are numeric but some are 
# indicators of numeric ranges or missingness.

#ex: prostate reg_nodes_pos
# 1-89 indicates number of nodes
#90 = 90 or more
# 95 = positive aspirate (assigned 1 node in CSTable scrape)

#desired value is taken from descr column of CSTables_scrape


translate_seer_numeric <-function(var, lookup) {
     names(lookup) <- c('old','new')
     var_df <- data.frame(old = var)
     
     res <- left_join(var_df, lookup, by = 'old')
     res$new # return a vector, not a data.frame or tibble. Can be piped
}

expand_seer_cs_table <- function(table, upper, na.values, names = c('old','new')) {
     names(table) <- c('old','new')
     
     lookup2 <- data.frame(old = 0:upper,
                           new = 0:upper)
     
     
     res <- bind_rows(lookup2, table)
     
     names(res) <- names
     
     res
     
}

process_seer_numeric <- function(var, table, upper, na.values, 
                                 names = c('old','new')) {
     
     expanded_table <- expand_seer_cs_table(table = table,
                                            upper = upper,
                                            na.values = na.values,
                                            names = names)
     
     result <- translate_seer_numeric(var = var,
                                      lookup = expanded_table)
     
     return(result)
}




# test1 <- data.frame(old = c(99,95,93, 8))
# lup <- data.frame(bif = c(93, 95, 99),
#                   new = c(3, 5, NA))
# 
# translate_seer_numeric(test1, 90, lup)
# 
# test1 %>% 

#      mutate(varb = translate_seer_numeric(old, 90, lup))

#      mutate(varb = translate_seer_numeric(var = old, 90, lup))