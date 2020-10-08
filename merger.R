##### Read in the 3 csvs, or open them via workspace (way faster)
data.function <- read.csv("LEE_Function.csv")
data.srst <- read.csv("LEE_srst.csv")
data.final <- read.csv("LEE_Final.csv")

##### Remove rows without errors (i.e. pfcode != 0000) in SRST & Function tests
data.function <- data.function[!data.function$pfcode == '0000', ]
data.srst <- data.srst[!data.srst$pfcode == '0000', ]

##### Bind rows in order of {Final -> SRST -> Function} (library 'dplyr' is required)
library(dplyr)
bind <- bind_rows(data.final, data.srst)
bind <- bind_rows(bind, data.function)

##### Optional: sort by column value(s), in this case by hddsn then by pheadno
#bind <- bind[order(data$hddsn, data$pheadno),]

##### Write the bound data to the csv, saving the workspace is also recommended
write.csv(bind,"finalsrstfunc.csv",row.names = FALSE, quote = FALSE)



