#---------------------------------------------------------
# 단자압착 검증 결과 기준
# 타수 차이 비교
# 2020-01-30
#---------------------------------------------------------

library(readxl)

press <- read_xlsx("S:/단자압착검증결과.xlsx", sheet = "Sheet1")

str(khdata)

press$terminal <- substr(press$단자명, 1, 10)
press$ter_app <- paste(press$terminal, press$금형코드, sep="_")
press$no <- 1:nrow(press)

library(dplyr)
press_tasu <- press %>% filter(no %% 3 == 2)
press_tasu <- press_tasu %>% filter(ter_app %in% khdata$ter_app)

merge(press_tasu, khdata,
      by = "ter_app",
      all.x = TRUE)


joindata <- khdata[,c(1, 17:25, 29, 30, 32)]

unique(joindata)
joindata <- joindata[,-c(14:15)]
joindata <- unique(joindata)
str(joindata)


str(press_tasu)
press_tasu$와이어부 <- as.numeric(press_tasu$와이어부)
press_tasu$인슈부 <- as.numeric(press_tasu$인슈부)


summary(press_tasu$와이어부)
hist(press_tasu$와이어부)

press_cut <- transform(press_tasu, 
                       cut = cut(와이어부, breaks = c(0, 0.8, 0.85, 0.9, 0.95, 1.0, 5.0, 7.0 , 15),
                                     include.lowest = TRUE,
                                     right = FALSE,
                                     labels = c("~80", "80~90", "90~100", "100~500", "500~700", "700~1500")))

press_cut_w <- transform(press_tasu,
                         cut = cut(와이어부, breaks = c(0, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 15),
                                      include.lowest = TRUE,
                                      right = FALSE,
                                      labels = c("~70", "~75", "~80", "~85", "~90", "~95", "~100", "~110", "~120", "~130", "~140", "~150", "~160", "~170", "~180", "~190", "~200", "200~")))



library(dplyr)
library(ggplot2)

press_cut_w %>% 
  ggplot(aes(x=cut, fill=피복부결과)) +
  geom_bar(position = "fill") +
  xlab("타수비율") +
  ylab("불량률") +
  facet_wrap(~구분, ncol=1) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")



table(press_cut_w$cut, press_cut_w$피복부결과)


summary(press_tasu$인슈부)
hist(press_tasu$인슈부)
press_cut_insu <- transform(press_tasu,
                       cut = cut(인슈부, breaks = c(0, 0.8, 0.85, 0.9, 0.95, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 15),
                                    include.lowest = TRUE,
                                    right = FALSE,
                                    labels = c("~80", "~85", "~90", "~95", "~100", "~110", "~120", "~130", "~140", "~150", "~160", "~170", "~180", "~190", "~200", "200~")))



hist(press_cut_insu$인슈부)
#일반 \r\nth?????ng 
#쌍압착\r\nNoi doi

press_cut_insu %>% 
  filter(구분 == "Seal") %>% 
  ggplot(aes(x=cut, fill=도체부결과)) +
  geom_bar(position = "fill") +
  xlab("타수비율") +
  ylab("도체부 결과 판정 비율") +
  facet_wrap(~구분, ncol=1) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")


insu_n <- press_cut_insu %>% group_by(cut, 구분, 도체부결과) %>% 
  filter(구분 == "Seal") %>% 
  summarise(n=n())

press_cut_insu %>% group_by(cut) %>% 
  filter(구분 == "일반 \r\nth?????ng") %>% 
  summarise(n=n())

table(press_cut_insu$cut, press_cut_insu2$도체부결과)


str(press_cut)

table(press_cut_insu$cut, press_cut_insu$도체부결과)
chisq.test(press_cut_insu$cut, press_cut_insu$도체부결과)


install.packages("descr")
library(descr)
CrossTable(press_cut_insu$cut, press_cut_insu2$도체부결과,
           prop.r = FALSE, prop.c = FALSE, prop.t = FALSE,
           chisq = TRUE, expected=TRUE)



