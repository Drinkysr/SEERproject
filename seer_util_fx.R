### Utility functions for SEER 

###
# translate variables when most are numeric but some are 
# indicators of numeric ranges or missingness.

#ex: prostate reg_nodes_pos
# 1-89 indicates number of nodes
#90 = 90 or more
# 95 = positive aspirate (assigned 1 node in CSTable scrape)

#desired value is taken from descr column of CSTables_scrape

translate_seer_numeric <- function(var, upper, lookup) {
    names(lookup) <- c('old','new')
    names(var) <- 'old'
    var <- as.data.frame(var)
    lookup2 <- data.frame(old = c(1:upper),
                          new = c(1:upper))
    lookup3 <- rbind(lookup, lookup2)
    print(var)   
    res <- left_join(var, lookup3, by = 'old') %>% 
        select(new)
    
    res
    
}

expand_seer_cs_table <- function(table, upper, na.values, names = c('old','new')) {
    names(table) <- c('old','new')
    
    lookup2 <- data.frame(old = c(1:upper),
                          new = c(1:upper))
    
    lookup3 <- rbind(lookup2, table)
    
    res <- lookup3 %>% 
        mutate(new = ifelse(new %in% na.values, NA, new),
               old = as.numeric(old),
               new = as.numeric(new))
    
    names(res) <- names
    
    res
    
}

# test1 <- data.frame(old = c(99,95,93, 8))
# lup <- data.frame(bif = c(93, 95, 99),
#                   new = c(3, 5, NA))
# 
# translate_seer_numeric(test1, 90, lup)
# 
# test1 %>% 
#      mutate(varb = translate_seer_numeric(var = old, 90, lup))