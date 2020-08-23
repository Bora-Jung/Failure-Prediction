### false data

# false data : gate test �ҷ� ������(MES)
library(readxl)
false_data1 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190401~190510.xlsx",
                         sheet = "Sheet1")

false_data2 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190701~190816.xlsx",
                         sheet = "Sheet1")

false_data3 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190816~191107.xlsx",
                         sheet = "Sheet1")

false_data4 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/��Ʈ������_�ҷ�����Ʈ_190510~190630.xlsx",
                         sheet = "Sheet1")


library(dplyr)
false_data1 <- false_data1 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �߻����, �ҷ�����, �۾���)

false_data2 <- false_data2 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �ҷ�����, �ҷ�����, �۾���)

false_data3 <- false_data3 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �ҷ�����, �ҷ�����, �۾���)

false_data4 <- false_data4 %>% select(LOT, ����, ����, ����, ����, ǰ��, �۾�ȣ��, �˻����, �ҷ�����, 
                                      �͹̳�, �۾�����, �ҷ�����, �ҷ�����, �۾���)

colnames(false_data1)[12] <- "�ҷ�����"


false_data <- rbind(false_data1, false_data2)
false_data <- rbind(false_data, false_data3)
false_data <- rbind(false_data, false_data4)


false_data1 <- c()
false_data2 <- c()
false_data3 <- c()
false_data4 <- c()




false_data$ngrate <- false_data$�ҷ�����/false_data$�˻����
raw_false_data$ngrate <- raw_false_data$�ҷ�����/raw_false_data$�˻����

str(false_data)







### MES data

library(DBI)
library(RMySQL)
library(dplyr)

#### DB CONNECT ####
con <- dbConnect(MySQL(), user="riskmg", password="riskmg", dbname="riskmg",
                 host="172.21.0.153")

dbListTables(con)

mes_autodata <- con %>% dbGetQuery("select * from mes_autodata where fdate >= '2019-04-01'")







#------------------------------------------
### applicator data
#------------------------------------------
library(readxl)
library(stringr)

### ���� ������ ������ : ������ ���� Ÿ�� ������
kh_master <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/Ÿ��������/����������_201907_201910.xlsx",
                        sheet = "m_appjigmst")

kh_master$app_code <- str_trim(kh_master$app_code, side=c("both", "left", "right"))
kh_master$jigcode <- str_trim(kh_master$jigcode, side=c("both", "left", "right"))

kh_master <- kh_master %>% arrange(app_code, jigcode)
kh_master <- unique(kh_master)

kh_master <- kh_master[,-c(9:11)]
kh_master <- kh_master[,-c(3)]

kh_master <- transform(kh_master, opt_jaego = as.numeric(opt_jaego))
kh_master <- transform(kh_master, chktasu = as.numeric(chktasu))
kh_master <- transform(kh_master, chktasu2 = as.numeric(chktasu2))
kh_master <- transform(kh_master, chktasu3 = as.numeric(chktasu3))
kh_master <- transform(kh_master, chgtasu = as.numeric(chgtasu))

str(kh_master)
kh_master <- unique(kh_master)


### ���� Ÿ�� ������ : ����-���� �� ����, ������ Ÿ�� ������
kh1 <- read.csv("D:/workspace/��Ʈ��(��������)/dataset/Ÿ��������/��Ʈ�� ����Ÿ��.csv", header = T)
kh2 <- read_xlsx("D:/workspace/��Ʈ��(��������)/dataset/Ÿ��������/����������_201907_201910.xlsx",
                 sheet = "m_appjig_tasu")

str(kh1)
str(kh2)

kh1 <- transform(kh1, cur_tasu = as.numeric(cur_tasu))
kh2 <- transform(kh2, cur_tasu = as.numeric(cur_tasu))
kh1 <- transform(kh1, last_tasu = as.numeric(last_tasu))
kh2 <- transform(kh2, last_tasu = as.numeric(last_tasu))
kh1 <- transform(kh1, chg_tasu = as.numeric(chg_tasu))
kh2 <- transform(kh2, chg_tasu = as.numeric(chg_tasu))
kh1 <- transform(kh1, chg_cnt = as.numeric(chg_cnt))
kh2 <- transform(kh2, chg_cnt = as.numeric(chg_cnt))

kh1 <- kh1[,-c(9:10)]
kh2 <- kh2[,-c(9:10)]

for(i in 1:3){
  kh1[,i] <- str_trim(kh1[,i], side=c("both", "left", "right"))
}

for(i in 1:3){
  kh2[,i] <- str_trim(kh2[,i], side=c("both", "left", "right"))
}


kh1$chg_date <- as.POSIXct(kh1$chg_date, format = '%Y-%m-%d %H:%M:%S')
kh2$chg_date <- as.POSIXct(kh2$chg_date, format = '%Y-%m-%d %H:%M:%S')

kh <- rbind(kh1, kh2)

kh <- unique(kh)

kh <- kh %>% arrange(app_code, jig_code, chg_date)

kh <- kh[,-6]   #chg tasu �� 0�̶� ����





### ���� ������ + �����͵����� ����

str(kh2)
str(kh_master)

# key ����
kh2$app_jig <- paste(kh2$app_code, kh2$jig_code, sep="_")
kh_master$app_jig <- paste(kh_master$app_code, kh_master$jigcode, sep="_")

tmp <- merge(kh2, kh_master,
             by = "app_jig",
             all.x = TRUE)


tmp <- unique(tmp)
colSums(is.na(tmp))

str(tmp)


tmp$ter_app <- paste(tmp$app_code.x, tmp$app_bcode, sep="_")

false_data$terminal <- substr(false_data$�͹̳�, 1, 10)
false_data$ter_app <- paste(false_data$terminal, false_data$�ҷ�����, sep="_")

data_kh <- merge(tmp, false_data,
                 by = "ter_app",
                 all.x = TRUE)

colSums(is.na(data_kh))
data_kh <- data_kh %>% filter(LOT != "")
data_kh <- unique(data_kh)

unique(data_kh$ter_app)
unique(data_kh$app_bcode)

str(data_kh)

data_kh_hogichange <- data_kh[,c(1, 31, 30, 24)]
str(data_kh_hogichange)

data_kh_hogichange <- unique(data_kh_hogichange)
hogi_mean <- data_kh_hogichange %>% group_by(ter_app) %>% summarise(mean=mean(ngrate))

library(reshape2)
d <- dcast(data_kh_hogichange, ter_app~�۾�ȣ��)
str(d)

for(i in 2:ncol(d)) {
  d[,i] <- ifelse(d[,i] == 0, d[,i] <- 0, d[,i] <- 1)
}

d$sum <- apply(d[,-1], 1, sum)
d <- d[,c(1,62)]

d <- cbind(d, hogi_mean)


falsename <- do.call(rbind, lapply(split(data_kh, data_kh$ter_app),
                                   function(aa){
                                     aa$val2 <- paste(aa$�ҷ�����, collapse=",")
                               
                               return(aa)
                             }))

falsename <- falsename[,c(1,33)]
falsename <- unique(falsename)




d <- cbind(d, falsename)



data_kh_hogichange$ter_app_hogi <- paste(data_kh_hogichange$ter_app, data_kh_hogichange$�۾�ȣ��, sep="_")


h <- data_kh_hogichange %>% group_by(ter_app_hogi, �ҷ�����) %>% summarise(n=n(), mean=mean(ngrate))



###
data_kh_hogichange$ter_app_fal <- paste(data_kh_hogichange$ter_app, data_kh_hogichange$�ҷ�����, sep="_")


data_kh_hogichange %>% group_by(ter_app_fal) %>% summarise(mean=mean(ngrate))
chan <- merge(data_kh_hogichange, d,
              by = "ter_app", all.x = TRUE)

str(chan)
colnames(chan)[7] <- "hogicng_cnt"
chan <- chan[,-c(1,4)]
str(chan)
chan <- chan[,c(1,2,5)]


cor(chan$ngrate, chan$hogicng_cnt)
co <- chan %>% group_by(hogicng_cnt) %>% 
  filter(ngrate <= 0.09) 

cor(co$hogicng_cnt, co$ngrate)

write.csv(chan, "D:/Workspace/ȣ���̵����ҷ���.csv", row.names=FALSE)


chan$hogicng_cnt <- paste(chan$hogicng_cnt, "ȸ", sep="")
chan$hogicng_cnt <- as.factor(chan$hogicng_cnt)

chan %>% filter(ngrate <=0.09) %>% 
ggplot(aes(x=hogicng_cnt, y=ngrate, fill=hogicng_cnt)) +
  geom_boxplot() +
  xlab("ȣ���̵�Ƚ��") +
  ylab("�ҷ���") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  scale_x_discrete(limits=c("1ȸ", "2ȸ", "3ȸ", "4ȸ", "5ȸ", "6ȸ", "7ȸ", "8ȸ", "9ȸ", "10ȸ", "13ȸ")) 
  



false_hogicng <- dcast(chan, �ҷ�����~hogicng_cnt, value.var="ngrate", mean, na.rm=T)


#### 2020/01/22 durl Rkwl gka
### ppt 6p.

