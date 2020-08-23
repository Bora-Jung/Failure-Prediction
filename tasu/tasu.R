data_kh
data_kh <- unique(data_kh)

summary(data_kh)



str(kh)
str(kh_master)

# key 생성
kh$app_jig <- paste(kh$app_code, kh$jig_code, sep="_")
kh_master$app_jig <- paste(kh_master$app_code, kh_master$jigcode, sep="_")

khall_tmp <- merge(kh, kh_master,
             by = "app_jig",
             all.x = TRUE)


khall_tmp <- unique(khall_tmp)
colSums(is.na(khall_tmp))

khall_tmp$ter_app <- paste(khall_tmp$app_code.x, khall_tmp$app_bcode, sep="_")


unique(khall_tmp$ter_app)


khall_tmp %>% filter(ter_app == 'TAP13JPT04_A50-0332')

false_data %>% filter(ter_app == 'TAP13JPT04_A50-0332')



false_data$terminal <- substr(false_data$터미널, 1, 10)
false_data$ter_app <- paste(false_data$terminal, false_data$불량금형, sep="_")



# 더 최근 타수만 뽑기
khall_tmp <- khall_tmp %>% group_by(app_jig, app_bcode) %>% mutate(n=order(desc(cur_tasu)))
head(khall_tmp)
khall_tmp <- khall_tmp %>% filter(n == 1)
khall_tmp <- khall_tmp[,-17]

khdata <- merge(khall_tmp, false_data,
                by = "ter_app",
                all.x = TRUE)

colSums(is.na(khdata))
khdata <- khdata %>% filter(LOT != "")
khdata <- unique(khdata)

unique(khdata$ter_app)



# 타수 비율
khdata$rate <- khdata$cur_tasu/khdata$chgtasu
summary(khdata$rate)



cutdata <- transform(khdata, 
                     cut = cut(rate, breaks = c(0, 0.8, 0.85, 0.9, 0.95, 1.0, 1.1, 1.2, 1.3, 1.4,  1.5),
                               include.lowest = TRUE,
                               right = FALSE,
                               labels = c("~80", "~85", "~90", "~95", "~100", "~110", "~120", "~130", "~140", "~150")))

cutdata <- cutdata %>% filter(ngrate <= 0.09)

cut_I <- cutdata %>% filter(jig_code == "IA" | jig_code == "IC")
cut_W <- cutdata %>% filter(jig_code == "WA" | jig_code == "WC")

count <- cutdata %>% select(ter_app, jig_code, cur_tasu, rate, cut)
count <- unique(count)
count %>% group_by(cut) %>% summarise(n=n())



out <- aov(cut_W$ngrate ~ cut_W$cut)
summary(out)
tukey = TukeyHSD(out)
tukey

summary(cut_I$ngrate)
cut_W %>% group_by(cut) %>% 
  summarise(mean = mean(ngrate, na.rm=T), n=n())



I <- khdata %>% filter(jig_code == "IA" | jig_code == "IC") %>% 
  filter(ngrate <= 0.09)
cor(I$rate, I$ngrate)
W <- khdata %>% filter(jig_code == "WA" | jig_code == "WC") %>% 
  filter(ngrate <= 0.09)
cor(W$rate, W$ngrate)
  


ggplot(cut_W, aes(x=cut, y=ngrate, color=cut)) +
  geom_boxplot() +
  xlab("사용비율(%)") +
  ylab("불량률") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")


fal_cut <- cutdata %>% group_by(cut, 불량내용) %>% summarise(mean = mean(ngrate))
#write.csv(fal_cut, "D:/Workspace/불량내용사용구간별불량률.csv", row.names=FALSE)



kijun <- transform(khdata, 
                     cut = cut(rate, breaks = c(0,  1.2,  1.5),
                               include.lowest = TRUE,
                               right = FALSE,
                               labels = c("~kijun", "~150")))


kijun %>% group_by(cut) %>% filter(jig_code == "IA" | jig_code == "IC") %>% filter(ngrate <= 0.09) %>% summarise(mean=mean(ngrate)) 


kijun_I <- kijun %>% filter(jig_code == "IA" | jig_code == "IC") %>% filter(ngrate <= 0.09)
kijun_W <- kijun %>% filter(jig_code == "WA" | jig_code == "WC") %>% filter(ngrate <= 0.09)

summary(aov(kijun_I$ngrate ~ kijun_I$cut))
summary(aov(kijun_W$ngrate ~ kijun_W$cut))


summary(aov(kijun$ngrate ~ kijun$cut))

ggplot(kijun, aes(x=cut, y=ngrate, color=cut)) +
  geom_boxplot()

kijun %>% group_by(cut)%>% summarise(mean=mean(ngrate))

out <- aov(kijun$ngrate ~ kijun$cut)
summary(out)
tukey = TukeyHSD(out)
tukey
