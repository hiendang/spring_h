#test3R
library(readr)
train <- read_csv("input/ptrain.csv")
test  <- read_csv("input/ptest.csv")
y = train$target
test_id = test$ID
train = subset(train, select=-c(target))
test = subset(test,select=-c(ID))
gc()
removing_column <- c("VAR_0008","VAR_0009","VAR_0010","VAR_0011","VAR_0012","VAR_0018","VAR_0019","VAR_0020","VAR_0021","VAR_0022","VAR_0023","VAR_0024","VAR_0025","VAR_0026","VAR_0027","VAR_0028","VAR_0029","VAR_0030","VAR_0031","VAR_0032","VAR_0038","VAR_0039","VAR_0040","VAR_0041","VAR_0042","VAR_0043","VAR_0044","VAR_0045","VAR_0098","VAR_0099","VAR_0106","VAR_0107","VAR_0114","VAR_0115","VAR_0117","VAR_0130","VAR_0131","VAR_0132","VAR_0138","VAR_0139","VAR_0140","VAR_0180","VAR_0181","VAR_0182","VAR_0183","VAR_0188","VAR_0189","VAR_0190","VAR_0191","VAR_0192","VAR_0193","VAR_0194","VAR_0195","VAR_0196","VAR_0197","VAR_0199","VAR_0202","VAR_0203","VAR_0207","VAR_0213","VAR_0214","VAR_0215","VAR_0216","VAR_0221","VAR_0222","VAR_0223","VAR_0229","VAR_0230","VAR_0236","VAR_0239","VAR_0246","VAR_0362","VAR_0371","VAR_0377","VAR_0378","VAR_0386","VAR_0387","VAR_0388","VAR_0392","VAR_0394","VAR_0395","VAR_0396","VAR_0399","VAR_0411","VAR_0413","VAR_0414","VAR_0428","VAR_0429","VAR_0438","VAR_0445","VAR_0456","VAR_0459","VAR_0460","VAR_0463","VAR_0470","VAR_0471","VAR_0472","VAR_0476","VAR_0478","VAR_0502","VAR_0508","VAR_0509","VAR_0521","VAR_0524","VAR_0526","VAR_0527","VAR_0529","VAR_0530","VAR_0638","VAR_0789","VAR_0840","VAR_0847","VAR_1013","VAR_1158","VAR_1198","VAR_1213","VAR_1427","VAR_1428","VAR_1588","VAR_1594","VAR_1616","VAR_1619","VAR_1656","VAR_1675","VAR_1723","VAR_1845","VAR_1848")
train=train[,!(names(train) %in% removing_column)]
test=test[,!(names(test) %in% removing_column)]
d = rbind(train,test)

library(Rtsne)
tsne <- Rtsne(d, dims = 3, perplexity=30, max_iter = 1000,initial_dims = 1000, check_duplicates=FALSE)
write.table(tsne$Y, file = "tsne.out.csv",sep=",",row.names=FALSE)


#### first 1/3 
#d <- d[,1:600]
#train <- 0
#test <- 0
#tsne <- Rtsne(d, dims = 3, perplexity=30, max_iter = 2000,initial_dims = 1000, check_duplicates=FALSE)
#write.table(tsne$Y, file = "tsne.out.csv.1",sep=",",row.names=FALSE)


#### second 1/3 
#d <- d[,600:1200]
#train<-0
#test<-0
#tsne <- Rtsne(d, dims = 3, perplexity=30, max_iter = 1000,initial_dims = 1000, check_duplicates=FALSE)
#write.table(tsne$Y, file = "tsne.out.csv.2",sep=",",row.names=FALSE)