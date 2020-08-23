#--------------------------------------------------
# 타수 구간 별, 불량내용(유형)
# 금형데이터 : khdata
# 불량률 3사분위수(0.09) 초과값 제외
#--------------------------------------------------


str(khdata)


library(reshape2)


cut_kh <- transform(khdata,
                    cut = cut(rate, breaks = c(0, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5),
                              include.lowest = TRUE,
                              ight = FALSE,
                              labels = c("~80", "80~90", "90~100", "100~110", "110~120", "120~130", "130~140", "140~150")))

summary(cut_kh$ngrate)
cut_kh <- cut_kh %>% filter(ngrate <= 0.09)

cut_mean_all <- dcast(cut_kh, 불량내용~cut, value.var="ngrate", mean, na.rm=T) 


cnt <- cut_kh %>% group_by(cut) %>% filter(jig_code == "WA" | jig_code == "WC") %>% distinct(app_jig)
cnt %>% group_by(cut) %>% summarise(n=n())



cut_kh %>% 
  filter(jig_code == "WA" | jig_code == "WC") %>%
  ggplot(aes(x=cut, y=ngrate, color=cut)) +
  # geom_point() +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")

cut_kh_I <- cut_kh %>%
  filter(jig_code == "IA" | jig_code == "IC")

cut_kh %>%
  filter(jig_code == "IA" | jig_code == "IC") %>%
  group_by(cut) %>% 
  summarise(mean = mean(ngrate, na.rm=T))



out <- aov(cut_kh_I$ngrate ~ cut_kh_I$cut)
summary(out)
tukey = TukeyHSD(out)
tukey


cut_kh_W <- cut_kh %>%
  filter(jig_code == "WA" | jig_code == "WC")

cut_kh %>%
  filter(jig_code == "WA" | jig_code == "WC") %>%
  group_by(cut) %>% 
  summarise(mean = mean(ngrate, na.rm=T))

out <- aov(cut_kh_W$ngrate ~ cut_kh_W$cut)
summary(out)
tukey = TukeyHSD(out)
tukey


#--------------------------------------------------
# 불량내용(유형)
#--------------------------------------------------
cut_mean_I <- dcast(cut_kh_I, 불량내용~cut, value.var="ngrate", mean, na.rm=T) 
cut_mean_W <- dcast(cut_kh_W, 불량내용~cut, value.var="ngrate", mean, na.rm=T) 
