##MAKE SURE TO READ THE FULL CSV IN BEFORE RUNNING THIS SCRIPT!
##OR JUST OPEN 'envi10gb.RData' IN GOOGLE DRIVE!

data.function <- read.csv("LEE_Function.csv")
data.srst <- read.csv("LEE_srst.csv")
data.final <- read.csv("LEE_Final.csv")

data <- data[order(data$hddsn, data$pheadno),]

#write.csv(data,('LEE_Final_Sorted.csv'), row.names = FALSE)

colMatch <- intersect(names(data.final10000), names(data.srst10000))
unique <- setdiff(names(data.final10000), names(data.srst10000))

hddsnMatch <- intersect(data.final10000[,1], data.srst10000[,1])
finalMatch <- apply(data.final10000, 1, function(r) any(r %in% hddsnMatch))
finalMatch2 <- data.final10000[finalMatch, ]
finalMatch2[,names(finalMatch2)] <- lapply(finalMatch2[,names(finalMatch2)] , factor)

library(dplyr)
bind <- bind_rows(data.final10000, data.srst_clean)
bind <- bind_rows(bind, data.function_clean)

length(levels(bind$ic0n_packwt_time_6800))
length(unique(bind$ic0n_packwt_time_6800))

data.function_error_only <- data.func10000[!data.func10000$pfcode == '0000', ]
data.srst_error_only <- data.srst10000[!data.srst10000$pfcode == '0000', ]

write.csv(bind,"merged_example.csv",row.names = FALSE, quote = FALSE)
