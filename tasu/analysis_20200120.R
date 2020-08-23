

false_data <- false_data %>% filter(!is.na(선번))

false_data$jijung <- ifelse(false_data$지시호기 == false_data$작업호기, false_data$jijung <- "O", false_data$jijung <- "X")
table(false_data$jijung)

summary(false_data$ngrate)
false_data %>% group_by(jijung) %>% 
  filter(ngrate <= 0.09) %>% 
  summarise(mean=mean(ngrate, na.rm=T),
            n=n(),
            sd = sd(ngrate, na.rm=T))


library(ggplot2)

summary(false_data$ngrate)

false_data %>% 
  filter(ngrate <= 0.09) %>% 
  ggplot(aes(x=jijung, y=ngrate, color=jijung)) +
  # geom_point() +
  geom_boxplot() +
  xlab("지정호기여부") +
  ylab("불량률") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")

out <- aov(false_data$ngrate ~ false_data$jijung)
summary(out)
tukey = TukeyHSD(out)
tukey



### data input
library(readxl)

# 금형 마스터 데이터 : 금형별 기준 타수 데이터
kh_master1 <- read.csv("D:/workspace/유라테크(베트남)/dataset/타수데이터/베트남 금형 마스터.csv", header = T)
kh_master2 <- read_xlsx("D:/workspace/유라테크(베트남)/dataset/타수데이터/금형마스터_201907_201910.xlsx",
                        sheet = "m_appjigmst")
kh_master <- rbind(kh_master1, kh_master2)


# 금형 타수 데이터 : 금형-지그 별 현재, 마지막 타수 데이터
kh1 <- read.csv("D:/workspace/유라테크(베트남)/dataset/타수데이터/베트남 금형타수.csv", header = T)
kh2 <- read_xlsx("D:/workspace/유라테크(베트남)/dataset/타수데이터/금형마스터_201907_201910.xlsx",
                 sheet = "m_appjig_tasu")

colnames(kh1)[9] <- "chg_yezi"
kh <- rbind(kh1, kh2)


kh$ter_app <- paste(kh$app_code, kh$app_bcode, sep="_")
unique(kh$ter_app)
kh_n <- kh %>% group_by(ter_app) %>% summarise(n=n())


### data cleansing

# 공백 제거
library(stringr)

for(i in 1:3){
  kh[,i] <- str_trim(kh[,i], side=c("both", "left", "right"))
}

for(i in 1:2){
  kh_master[,i] <- str_trim(kh_master[,i], side=c("both", "left", "right"))
}


# 금형 데이터 + 마스터데이터 병합
kh <- kh[,c(1,2,4:9)]
kh_master <- kh_master[,c(1,2,5:8)]

kh <- unique(kh)
kh_master <- unique(kh_master)

# 공백제거
kh_master$app_code <- str_trim(kh_master$app_code, side=c("both", "left", "right"))
kh_master$jig_code <- str_trim(kh_master$jig_code, side=c("both", "left", "right"))


# key 생성
kh$key <- paste(kh$app_code, kh$jig_code, sep="_")
kh_master$key <- paste(kh_master$app_code, kh_master$jigcode, sep="_")

tmp <- merge(kh, kh_master,
             by = "key",
             all.x = TRUE)

tmp <- tmp[,-c(9,10)]

colnames(tmp)[1:3] <- c("key", "app_code", "jig_code")

colSums(is.na(tmp))

str(tmp)
tmp %>% filter(app_bcode %in% false_data$불량금형)

str(false_data)
false_data$terminal <- substr(false_data$터미널, 1, 10)
tmp <- tmp[,-c(9,10)]
str(tmp)
tmp <- tmp[,-c(9)]
colnames(false_data)[13] <- "app_bcode"

str(false_data)
kh_all <- merge(false_data, tmp,
                by = "app_bcode",
                all.x = TRUE)

kh_all <- kh_all %>% filter(app_bcode != "")
colSums(is.na(kh_all))
kh_all <- kh_all %>% filter(key != "")
str(kh_all)
kh_all <- unique(kh_all)

str(tmp)
str(false_data)
false_data$ter_app <- paste(false_data$terminal, false_data$불량금형, sep="_")


false_data <- false_data %>% filter(!is.na(ter_app))

false_data %>% filter(ter_app %in% tmp$ter_app)

kh_false <- merge(false_data, tmp,
                  by="ter_app",
                  all.x = TRUE)
str(kh_false)



