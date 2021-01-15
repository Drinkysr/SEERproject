### scrape

library(tidyverse)
library(rvest)


link <- 'http://web2.facs.org/cstage0205/schemalist.html'

# x is the html of the big CS schema table of cancer
x <- read_html(link) 

# y is a vector of all the links in the schema list
y <- x %>% html_nodes('a') %>% 
  html_attr("href") %>% 
  paste0('http://web2.facs.org/cstage0205/', .)

# z is just an example of the html from the first link
# should turn this into a function to s-apply over all cancers

# table Names is the names of each CS table on the cancer-specific page

getCStable <- function(cs.url) {
z <- read_html(cs.url)
tableNames <- z %>% html_nodes('td a') %>% 
  html_text()

#z.text is all the text broken up into a vector
## the tables which have 9, 988 or NA after them are not
## of interest, as they are not used
## So elimIndex is the index of all of the 9s, 988s and NAs
## plus every item immediately preceeding them
## Once these are eliminated we have a name of each used table
## now in the wantedTables vector
 z.text <- z %>% html_nodes('b, td a') %>% 
    html_text()

 elimIndex1 <-c(which(z.text %in% c("9","988", "NA")),
               which(z.text %in% c("9","988", "NA"))-1)
 
 elimOther1 <- grep('*Table*', z.text)
 elimOther2 <- grep('*Stage*', z.text)
 
elimIndex <- c(elimIndex1, elimOther1, elimOther2) 

wantedTables <-  z.text[-elimIndex] 
#print(wantedTables)
 
# zhref is the link portion of each table name
# it is a child link so it needs to be joined with the 
# first portion of the link from each cancer

zhref <- z %>% html_nodes('td a') %>% 
  html_attr('href')


# this trims off the final portion of the URL
# so that the initial part can be used to 
# construct the URL for the individual tables
address_trim <- regex('^(.*[\\/])')
link.start <- str_extract(cs.url, address_trim)

q <- data.frame(tableNames, zhref = paste0(link.start, zhref))
#print(q)
q.final <- q %>% 
  filter(tableNames %in% wantedTables)  %>% 
  select(zhref) %>% 
  .[[1]]

return(q.final)}


allTableLinks <- unlist(sapply(y, getCStable))

## This is the function to get the full hTable for
## a given cancer
#q.final is a data frame with the tableNames and the links

getCodeTable <- function(cs.table.url) {

#hTable is the final table of the codes for each CS variable
h <- read_html(cs.table.url)
hTable <- h %>% html_table(header = T)
hTable <- as.data.frame(hTable[[1]])
if(names(hTable)[1] != 'Code') {
  return(NULL)
}

## get the name of the cancer
cancerName <- h %>% 
  html_nodes('h1') %>% 
  html_text()

tableName <- h %>% 
  html_nodes('h2') %>% 
  html_text()


#names(hTable) <- c('Code','Description')

hTable <- hTable %>% 
  mutate(cancer = cancerName,
         variable = tableName,
         Code = as.character(Code)) %>% 
select(cancer, variable, Code, Description)

print(paste0("Completed ", cancerName, ", ", tableName))

return(as.data.frame(hTable))
}

#getCodeTable(allTableLinks[10]) %>% str()

res1 <- sapply(allTableLinks, FUN = getCodeTable)

cancerIndex <- seq(1, length(res1), by = 4)
tableIndex <- seq(1, length(res1), by = 4) + 1
codeIndex <- seq(1, length(res1), by = 4) + 2
descIndex <- seq(1, length(res1), by = 4) + 3

finalCStable <- data.frame(cancer = unlist(res1[cancerIndex]),
                           table = unlist(res1[tableIndex]),
                           code = unlist(res1[codeIndex]),
                           description = unlist(res1[descIndex]))

write.csv(finalCStable, file = 'CSTables.csv', row.names = F)
