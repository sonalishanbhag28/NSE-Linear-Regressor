#install.packages("nse2r")
library(nse2r)
df <- nse_stock_top_gainers()
df<- as.data.frame(df)
df$timestamp <- format(Sys.time(),'%d-%m-%y %H:%M:%S')
write.table(df,file="path-to-file.csv",sep=",",append=TRUE,col.names=FALSE,row.names=FALSE)

