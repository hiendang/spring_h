#test3R
library(readr)
library(xgboost)
train <- read_csv("input/train.csv")
test  <- read_csv("input/test.csv")
y = train$target
test_id = test$ID
gc()
train <- read_csv("input/ptrain.csv")
test <-  read_csv("input/ptest.csv")

train = subset(train, select=-c(target))
test = subset(test,select=-c(ID))
gc()
#col1000 <- c("VAR_0087", "VAR_0074", "VAR_0063", "VAR_0072", "VAR_0136", "VAR_0068", "VAR_1127", "VAR_1329", "VAR_0233", "VAR_0144", "VAR_0069", "VAR_0716", "VAR_0004", "VAR_0085", "VAR_0073", "VAR_0886", "VAR_1824", "VAR_0228", "VAR_0707", "VAR_1119", "VAR_1110", "VAR_0065", "VAR_0853", "VAR_1128", "VAR_0540", "VAR_1136", "VAR_1530", "VAR_0217", "VAR_0884", "VAR_0579", "VAR_0082", "VAR_0241", "VAR_0078", "VAR_0089", "VAR_0137", "VAR_0273", "VAR_1823", "VAR_0013", "VAR_0968", "VAR_0064", "VAR_0077", "VAR_0080", "VAR_0282", "VAR_0860", "VAR_1385", "VAR_0075", "VAR_0712", "VAR_1183", "VAR_0079", "VAR_1146", "VAR_1154", "VAR_0970", "VAR_1125", "VAR_0015", "VAR_0836", "VAR_0881", "VAR_0708", "VAR_1100", "VAR_0200", "VAR_0720", "VAR_1265", "VAR_1865", "VAR_0722", "VAR_1380", "VAR_1147", "VAR_0086", "VAR_0543", "VAR_1179", "VAR_0869", "VAR_1151", "VAR_0204", "VAR_1130", "VAR_0002", "VAR_0322", "VAR_0061", "VAR_0721", "VAR_0922", "VAR_1337", "VAR_0272", "VAR_1148", "VAR_0555", "VAR_1531", "VAR_0888", "VAR_0255", "VAR_0198", "VAR_1152", "VAR_1116", "VAR_0227", "VAR_1390", "VAR_1775", "VAR_0084", "VAR_0324", "VAR_1135", "VAR_0242", "VAR_0587", "VAR_0631", "VAR_1512", "VAR_0550", "VAR_1073", "VAR_0212", "VAR_0135", "VAR_1129", "VAR_1576", "VAR_0302", "VAR_1149", "VAR_1088", "VAR_1182", "VAR_0003", "VAR_1751", "VAR_0517", "VAR_0885", "VAR_0863", "VAR_0544", "VAR_1117", "VAR_0254", "VAR_0895", "VAR_1123", "VAR_1145", "VAR_0868", "VAR_0892", "VAR_1096", "VAR_1809", "VAR_1201", "VAR_1120", "VAR_0870", "VAR_1087", "VAR_0541", "VAR_1184", "VAR_1150", "VAR_0614", "VAR_1842", "VAR_1494", "VAR_0142", "VAR_0313", "VAR_1089", "VAR_1086", "VAR_1259", "VAR_0515", "VAR_0704", "VAR_0848", "VAR_0257", "VAR_0234", "VAR_0611", "VAR_0871", "VAR_0688", "VAR_1114", "VAR_0917", "VAR_0909", "VAR_1044", "VAR_1124", "VAR_0778", "VAR_1351", "VAR_1399", "VAR_0263", "VAR_0880", "VAR_1497", "VAR_0237", "VAR_1495", "VAR_1573", "VAR_1095", "VAR_0898", "VAR_0103", "VAR_1153", "VAR_0338", "VAR_0304", "VAR_1180", "VAR_0648", "VAR_0070", "VAR_0341", "VAR_1376", "VAR_1335", "VAR_0806", "VAR_0876", "VAR_0983", "VAR_0861", "VAR_0274", "VAR_0907", "VAR_1266", "VAR_0279", "VAR_0652", "VAR_0573", "VAR_1330", "VAR_0950", "VAR_1082", "VAR_0705", "VAR_1144", "VAR_0361", "VAR_0951", "VAR_1336", "VAR_0715", "VAR_0610", "VAR_1134", "VAR_1828", "VAR_0887", "VAR_0088", "VAR_0921", "VAR_0649", "VAR_0514", "VAR_1319", "VAR_1525", "VAR_0938", "VAR_0083", "VAR_0918", "VAR_1097", "VAR_0632", "VAR_0536", "VAR_0934", "VAR_1572", "VAR_1841", "VAR_1791", "VAR_0558", "VAR_0979", "VAR_1358", "VAR_1902", "VAR_0293", "VAR_0613", "VAR_0713", "VAR_0981", "VAR_1248", "VAR_1513", "VAR_0875", "VAR_1489", "VAR_0556", "VAR_1090", "VAR_1014", "VAR_1113", "VAR_1016", "VAR_0709", "VAR_1384", "VAR_1138", "VAR_0985", "VAR_0807", "VAR_0829", "VAR_1228", "VAR_0920", "VAR_0711", "VAR_1748", "VAR_1141", "VAR_1536", "VAR_1425", "VAR_1122", "VAR_0823", "VAR_1133", "VAR_1323", "VAR_0899", "VAR_0943", "VAR_1388", "VAR_1260", "VAR_0954", "VAR_0687", "VAR_1301", "VAR_0980", "VAR_1227", "VAR_0893", "VAR_1502", "VAR_0858", "VAR_0706", "VAR_0796", "VAR_0877", "VAR_1211", "VAR_0915", "VAR_1801", "VAR_0758", "VAR_0795", "VAR_1815", "VAR_1034", "VAR_1371", "VAR_1203", "VAR_1860", "VAR_0813", "VAR_1357", "VAR_1825", "VAR_0608", "VAR_0319", "VAR_1403", "VAR_1750", "VAR_0792", "VAR_1038", "VAR_1424", "VAR_0342", "VAR_1567", "VAR_0805", "VAR_1081", "VAR_1008", "VAR_1374", "VAR_1210", "VAR_1894", "VAR_1314", "VAR_0855", "VAR_0989", "VAR_0700", "VAR_1516", "VAR_0812", "VAR_1462", "VAR_0748", "VAR_1204", "VAR_1040", "VAR_1094", "VAR_1810", "VAR_0607", "VAR_0288", "VAR_0976", "VAR_0768", "VAR_0794", "VAR_1496", "VAR_1391", "VAR_1181", "VAR_1139", "VAR_0561", "VAR_1893", "VAR_0905", "VAR_1381", "VAR_1919", "VAR_1515", "VAR_0982", "VAR_1913", "VAR_1372", "VAR_0612", "VAR_1126", "VAR_0309", "VAR_1760", "VAR_1526", "VAR_0992", "VAR_1835", "VAR_0071", "VAR_1808", "VAR_1624", "VAR_0831", "VAR_1118", "VAR_1313", "VAR_0764", "VAR_1316", "VAR_0717", "VAR_1524", "VAR_0628", "VAR_1517", "VAR_1455", "VAR_0930", "VAR_1561", "VAR_1005", "VAR_0854", "VAR_1892", "VAR_0559", "VAR_0655", "VAR_0862", "VAR_1019", "VAR_0320", "VAR_1419", "VAR_0866", "VAR_1529", "VAR_0874", "VAR_1221", "VAR_0996", "VAR_0321", "VAR_1807", "VAR_0333", "VAR_1861", "VAR_0729", "VAR_0006", "VAR_1321", "VAR_0340", "VAR_1901", "VAR_0336", "VAR_0596", "VAR_0419", "VAR_0081", "VAR_1827", "VAR_1036", "VAR_0296", "VAR_0110", "VAR_1327", "VAR_0578", "VAR_1132", "VAR_1520", "VAR_1006", "VAR_0896", "VAR_0791", "VAR_1022", "VAR_1759", "VAR_0900", "VAR_0692", "VAR_1579", "VAR_1514", "VAR_0331", "VAR_1029", "VAR_0723", "VAR_0998", "VAR_0825", "VAR_0800", "VAR_1898", "VAR_1031", "VAR_0584", "VAR_0793", "VAR_1912", "VAR_0351", "VAR_0824", "VAR_1537", "VAR_0585", "VAR_1320", "VAR_0583", "VAR_0935", "VAR_0997", "VAR_0864", "VAR_0059", "VAR_1241", "VAR_1837", "VAR_1899", "VAR_0916", "VAR_1143", "VAR_1331", "VAR_0801", "VAR_0516", "VAR_0630", "VAR_1859", "VAR_0774", "VAR_0993", "VAR_0849", "VAR_0542", "VAR_1426", "VAR_0557", "VAR_0834", "VAR_1465", "VAR_1169", "VAR_0856", "VAR_0238", "VAR_0560", "VAR_1831", "VAR_0316", "VAR_0931", "VAR_0337", "VAR_1398", "VAR_0597", "VAR_0719", "VAR_0329", "VAR_1836", "VAR_1580", "VAR_0554", "VAR_0872", "VAR_1386", "VAR_0323", "VAR_0367", "VAR_1208", "VAR_1170", "VAR_0852", "VAR_0604", "VAR_1258", "VAR_1895", "VAR_0609", "VAR_0785", "VAR_0408", "VAR_1200", "VAR_0266", "VAR_0814", "VAR_0245", "VAR_0695", "VAR_1453", "VAR_1032", "VAR_1318", "VAR_1359", "VAR_0859", "VAR_1121", "VAR_1075", "VAR_1541", "VAR_1115", "VAR_0694", "VAR_1853", "VAR_1353", "VAR_1420", "VAR_1000", "VAR_1338", "VAR_0592", "VAR_1575", "VAR_0822", "VAR_0991", "VAR_0867", "VAR_0104", "VAR_0699", "VAR_0301", "VAR_1354", "VAR_1789", "VAR_0317", "VAR_0325", "VAR_0845", "VAR_0837", "VAR_1740", "VAR_0299", "VAR_0315", "VAR_0360", "VAR_0835", "VAR_0295", "VAR_0846", "VAR_1550", "VAR_0334", "VAR_0366", "VAR_1747", "VAR_0810", "VAR_1360", "VAR_1011", "VAR_1020", "VAR_0882", "VAR_0300", "VAR_1312", "VAR_0766", "VAR_1028", "VAR_1830", "VAR_0211", "VAR_1383", "VAR_1021", "VAR_0693", "VAR_1229", "VAR_1243", "VAR_0753", "VAR_0330", "VAR_0310", "VAR_1052", "VAR_1742", "VAR_0908", "VAR_0570", "VAR_1035", "VAR_1137", "VAR_0802", "VAR_1891", "VAR_1328", "VAR_0593", "VAR_1571", "VAR_1387", "VAR_1540", "VAR_1915", "VAR_0661", "VAR_1463", "VAR_0235", "VAR_1802", "VAR_1109", "VAR_0969", "VAR_1373", "VAR_0503", "VAR_1202", "VAR_1074", "VAR_0749", "VAR_1053", "VAR_0208", "VAR_1361", "VAR_1864", "VAR_1464", "VAR_0940", "VAR_1007", "VAR_1072", "VAR_1856", "VAR_1523", "VAR_1369", "VAR_1348", "VAR_1030", "VAR_1222", "VAR_1281", "VAR_0174", "VAR_1889", "VAR_1767", "VAR_0946", "VAR_0623", "VAR_1267", "VAR_0962", "VAR_1238", "VAR_0335", "VAR_0725", "VAR_1178", "VAR_0633", "VAR_0988", "VAR_1421", "VAR_1397", "VAR_0897", "VAR_1261", "VAR_1655", "VAR_0953", "VAR_1839", "VAR_1556", "VAR_1224", "VAR_1083", "VAR_1322", "VAR_1460", "VAR_0857", "VAR_1223", "VAR_0936", "VAR_1840", "VAR_0588", "VAR_0728", "VAR_0439", "VAR_0910", "VAR_0635", "VAR_0726", "VAR_1519", "VAR_1769", "VAR_1566", "VAR_0815", "VAR_1363", "VAR_0959", "VAR_0434", "VAR_0698", "VAR_0942", "VAR_1503", "VAR_0987", "VAR_1719", "VAR_1454", "VAR_0821", "VAR_1315", "VAR_1185", "VAR_1157", "VAR_1821", "VAR_0818", "VAR_1039", "VAR_1286", "VAR_1209", "VAR_1890", "VAR_1018", "VAR_1033", "VAR_1527", "VAR_0231", "VAR_1041", "VAR_1878", "VAR_0902", "VAR_1654", "VAR_1766", "VAR_1004", "VAR_1829", "VAR_0651", "VAR_1112", "VAR_1217", "VAR_0636", "VAR_1511", "VAR_0964", "VAR_1368", "VAR_0977", "VAR_0752", "VAR_1393", "VAR_0683", "VAR_0060", "VAR_1172", "VAR_1877", "VAR_1457", "VAR_1793", "VAR_0727", "VAR_1504", "VAR_1832", "VAR_0359", "VAR_0483", "VAR_1355", "VAR_1518", "VAR_1854", "VAR_0358", "VAR_1774", "VAR_0894", "VAR_1003", "VAR_0883", "VAR_0051", "VAR_1264", "VAR_1048", "VAR_1838", "VAR_1456", "VAR_1401", "VAR_0903", "VAR_1108", "VAR_0621", "VAR_0878", "VAR_1382", "VAR_1159", "VAR_0832", "VAR_0297", "VAR_1578", "VAR_0947", "VAR_1625", "VAR_0686", "VAR_1333", "VAR_0364", "VAR_0586", "VAR_0605", "VAR_0929", "VAR_1220", "VAR_1199", "VAR_0945", "VAR_0581", "VAR_1001", "VAR_1042", "VAR_1407", "VAR_1396", "VAR_1532", "VAR_0955", "VAR_1755", "VAR_0754", "VAR_1452", "VAR_0990", "VAR_1685", "VAR_1160", "VAR_0999", "VAR_0260", "VAR_0403", "VAR_1554", "VAR_0656", "VAR_0841", "VAR_1317", "VAR_0365", "VAR_0850", "VAR_0919", "VAR_1412", "VAR_1347", "VAR_0465", "VAR_1900", "VAR_1356", "VAR_1046", "VAR_1324", "VAR_1922", "VAR_1263", "VAR_0776", "VAR_0879", "VAR_0650", "VAR_0889", "VAR_1862", "VAR_0984", "VAR_1080", "VAR_1193", "VAR_1626", "VAR_0685", "VAR_1349", "VAR_0417", "VAR_1294", "VAR_0256", "VAR_1332", "VAR_1565", "VAR_1783", "VAR_0697", "VAR_1738", "VAR_1691", "VAR_1786", "VAR_1027", "VAR_0318", "VAR_0787", "VAR_1056", "VAR_1177", "VAR_0770", "VAR_1362", "VAR_0488", "VAR_0961", "VAR_0788", "VAR_1744", "VAR_0210", "VAR_0482", "VAR_0531", "VAR_1274", "VAR_0404", "VAR_0925", "VAR_1863", "VAR_1334", "VAR_1101", "VAR_1570", "VAR_0797", "VAR_1720", "VAR_0654", "VAR_0298", "VAR_0267", "VAR_0176", "VAR_0406", "VAR_0262", "VAR_1131", "VAR_1816", "VAR_0701", "VAR_0343", "VAR_0066", "VAR_1306", "VAR_0653", "VAR_1923", "VAR_0803", "VAR_1275", "VAR_1233", "VAR_1811", "VAR_1873", "VAR_0646", "VAR_0781", "VAR_0617", "VAR_1377", "VAR_1790", "VAR_1745", "VAR_1551", "VAR_1903", "VAR_1924", "VAR_0780", "VAR_0816", "VAR_0851", "VAR_0937", "VAR_0817", "VAR_1648", "VAR_1762", "VAR_1743", "VAR_0618", "VAR_0724", "VAR_0826", "VAR_1483", "VAR_1047", "VAR_1105", "VAR_1171", "VAR_0972", "VAR_1522", "VAR_0790", "VAR_1161", "VAR_0865", "VAR_0890", "VAR_0627", "VAR_0486", "VAR_0067", "VAR_1207", "VAR_1768", "VAR_1037", "VAR_1206", "VAR_1269", "VAR_1023", "VAR_1104", "VAR_1539", "VAR_1400", "VAR_1709", "VAR_0327", "VAR_1834", "VAR_1749", "VAR_1897", "VAR_1392", "VAR_0839", "VAR_0782", "VAR_1304", "VAR_0771", "VAR_1191", "VAR_1271", "VAR_1644", "VAR_1649", "VAR_1618", "VAR_0283", "VAR_1642", "VAR_1043", "VAR_0575", "VAR_1887", "VAR_1564", "VAR_0965", "VAR_1395", "VAR_0901", "VAR_1062", "VAR_1142", "VAR_0007", "VAR_0143", "VAR_1226", "VAR_1879", "VAR_1278", "VAR_0427", "VAR_0769", "VAR_1298", "VAR_0485", "VAR_0744", "VAR_1521", "VAR_0034", "VAR_1389", "VAR_0838", "VAR_1282", "VAR_1474", "VAR_1262", "VAR_0354", "VAR_1756", "VAR_0625", "VAR_1402", "VAR_0760", "VAR_1826", "VAR_0454", "VAR_1822", "VAR_0440", "VAR_0710", "VAR_1405", "VAR_0911", "VAR_1582", "VAR_0405", "VAR_0755", "VAR_0830", "VAR_1510", "VAR_0363", "VAR_0442", "VAR_0844", "VAR_1904", "VAR_0742", "VAR_1192", "VAR_1059", "VAR_1470", "VAR_1449", "VAR_1295", "VAR_1568", "VAR_1339", "VAR_0746", "VAR_1581", "VAR_0718", "VAR_1509", "VAR_0734", "VAR_1866", "VAR_1882", "VAR_1712", "VAR_1763", "VAR_0033", "VAR_0926", "VAR_1546", "VAR_1914", "VAR_0056", "VAR_1236", "VAR_1857", "VAR_1268", "VAR_1933", "VAR_1918", "VAR_1833", "VAR_1858", "VAR_1234", "VAR_1765", "VAR_1874", "VAR_0547", "VAR_0357", "VAR_1276", "VAR_0986", "VAR_1257", "VAR_0535", "VAR_1237", "VAR_1583", "VAR_0963", "VAR_1472", "VAR_1326", "VAR_1311", "VAR_0062", "VAR_0447", "VAR_1107", "VAR_0978", "VAR_1553", "VAR_0974", "VAR_1908", "VAR_0677", "VAR_0179", "VAR_1617", "VAR_0424", "VAR_1741", "VAR_0629", "VAR_0489", "VAR_0957", "VAR_1272", "VAR_1002", "VAR_0368", "VAR_0913", "VAR_0224", "VAR_0314", "VAR_1761", "VAR_1414", "VAR_1690", "VAR_0591", "VAR_1560", "VAR_1884", "VAR_1872", "VAR_1921", "VAR_1058", "VAR_0400", "VAR_1270", "VAR_1173", "VAR_1631", "VAR_1375", "VAR_1231", "VAR_1461", "VAR_1413", "VAR_1528", "VAR_0055", "VAR_1256", "VAR_0798", "VAR_1870", "VAR_1538", "VAR_1418", "VAR_0166", "VAR_0268", "VAR_0294", "VAR_1650", "VAR_0952", "VAR_1103", "VAR_0577", "VAR_1156", "VAR_1764", "VAR_1931", "VAR_0506", "VAR_1780", "VAR_0973", "VAR_0673", "VAR_1710", "VAR_1482", "VAR_1574", "VAR_1534", "VAR_1406", "VAR_0571", "VAR_0164", "VAR_0819", "VAR_0773", "VAR_1739", "VAR_0572", "VAR_1929", "VAR_1451", "VAR_0258", "VAR_1758", "VAR_1410")
removing_cols <-c("VAR_0087", "VAR_0074", "VAR_0063", "VAR_0072", "VAR_0136", "VAR_0068", "VAR_1127", "VAR_1329", "VAR_0233", "VAR_0144", "VAR_0069", "VAR_0716", "VAR_0004", "VAR_0085", "VAR_0073", "VAR_0886", "VAR_1824", "VAR_0707", "VAR_1119", "VAR_1110", "VAR_0065", "VAR_0853", "VAR_1128", "VAR_0540", "VAR_1136", "VAR_1530", "VAR_0217", "VAR_0884", "VAR_0579", "VAR_0082", "VAR_0241", "VAR_0078", "VAR_0089", "VAR_0137", "VAR_0273", "VAR_1823", "VAR_0968", "VAR_0064", "VAR_0077", "VAR_0080", "VAR_0282", "VAR_0860", "VAR_1385", "VAR_0075", "VAR_0712", "VAR_1183", "VAR_0079", "VAR_1146", "VAR_1154", "VAR_0970", "VAR_1125", "VAR_0015", "VAR_0836", "VAR_0881", "VAR_0708", "VAR_1100", "VAR_0200", "VAR_0720", "VAR_1265", "VAR_1865", "VAR_0722", "VAR_1380", "VAR_1147", "VAR_0086", "VAR_0543", "VAR_1179", "VAR_0869", "VAR_1151", "VAR_0204", "VAR_1130", "VAR_0002", "VAR_0322", "VAR_0061", "VAR_0721", "VAR_0922", "VAR_1337", "VAR_0272", "VAR_1148", "VAR_0555", "VAR_1531", "VAR_0888", "VAR_0255", "VAR_0198", "VAR_1152", "VAR_1116", "VAR_0227", "VAR_1390", "VAR_1775", "VAR_0084", "VAR_0324", "VAR_1135", "VAR_0242", "VAR_0587", "VAR_0631", "VAR_1512", "VAR_0550", "VAR_1073")
train = train[,!(names(train) %in% removing_cols)]
test  = test[,!(names(test) %in% removing_cols)]

#train=train[,(names(train) %in% col1000)]
#test=test[,(names(test) %in% col1000)]

##load tsne data
tsne_data <- read_csv("tsne.out.csv")
train = cbind(train,tsne_data[1:nrow(train),])
test  = cbind(test ,tsne_data[(nrow(train)+1):nrow(tsne_data),])
cat(paste("dim is ",dim(train),"\n"))
for (ti in 0:7){
	if (ti != 5){
		tsne_data <- read_csv(paste("tsne.out.csv.",ti,sep=""))
		train = cbind(train,tsne_data[1:nrow(train),])
		test  = cbind(test ,tsne_data[(nrow(train)+1):nrow(tsne_data),])
	}
}
cat(paste("dim is ", dim(train),"\n"))
tsne_data <- 0
##load pca data
pca_data_train <- read_csv("input/pca100train.csv")
pca_data_test  <- read_csv("input/pca100test.csv")
train <- cbind(train,pca_data_train)
test  <- cbind(test ,pca_data_test)
pca_data_train <-0
pca_data_test <- 0
cat(paste("dim is ",dim(test),"\n"))
n <- 100000
##
for (i in 1:2){
	set.seed(i*9-5)
	h <- sample(nrow(train), 130000)
	
	val<-train[-h,]
	gc()
	dtrain <-train[h,]
	gc()
	dtrain <- xgb.DMatrix(data.matrix(dtrain), label=y[h])

	dval <- xgb.DMatrix(data.matrix(val), label=y[-h])
	gc()

	watchlist <- list(eval = dval)
	param <- list(  objective           = "binary:logistic",
				eta                 = 0.0015,
				max_depth           = 12-i%/%2,  # changed from default of 6
				subsample           = 0.7,
				colsample_bytree    = 0.7,
				min_child_weight	= 6,
				eval_metric         = "auc"
                )
	clf <- xgb.train(   params         	= param, 
                    data                = dtrain, 
                    nrounds             = n, # changed from 300
                    verbose             = 1, 
                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = TRUE)
	submission <- data.frame(ID=test_id)
	submission$target <- NA 
	for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
		submission[rows, "target"] <- predict(clf, data.matrix(test[rows,]),ntreelimit=clf$bestInd)		
	}
	cat("saving the submission file\n")
	write_csv(submission, paste("xgbf.binary_logistic.csv.",i,sep = "_"))
	set.seed(i*113-17)
	h <- sample(nrow(train), 130000)
	val<-train[-h,]
	gc()
	dtrain <-train[h,]
	gc()
	dtrain <- xgb.DMatrix(data.matrix(dtrain), label=y[h])

	dval <- xgb.DMatrix(data.matrix(val), label=y[-h])
	gc()
	param <- list(  objective           = "multi:softprob",#"binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.0015+i/1500, #0.06, #0.01,
                max_depth           = 10-i%/%2,  # changed from default of 8
				num_class			= 2,
                subsample           = 0.75 -i/20,
                colsample_bytree    = 0.65 +i/20,
                min_child_weight    = 6-i%/%2,
                eval_metric         = "mlogloss"
                )

	clf <- xgb.train(   params              = param, 
						data                = dtrain, 
						nrounds             = n, #280, #125, #250, # changed from 300
						verbose             = 1, 
						early.stop.round    = 100,
						watchlist           = watchlist,
						maximize            = FALSE)


	submission <- data.frame(ID=test_id)
	submission$target <- NA 
	for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
		tmp <- predict(clf, data.matrix(test[rows,]),ntreelimit=clf$bestInd)
		tmp <- matrix(tmp, ncol=2, byrow=TRUE)
		submission[rows, "target"] <- tmp[,2]
	}


	cat("saving the submission file\n")
	write_csv(submission, paste("xgbf.multisoftprob.csv.",i,sep="_"))
}

####

