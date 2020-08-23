#---------------------------------------------------------
# ���ھ��� ���� ��� ����
# Ÿ�� ���� ��
# 2020-01-30
#---------------------------------------------------------

library(readxl)

press <- read_xlsx("S:/���ھ����������.xlsx", sheet = "Sheet1")

str(khdata)

press$terminal <- substr(press$���ڸ�, 1, 10)
press$ter_app <- paste(press$terminal, press$�����ڵ�, sep="_")
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
press_tasu$���̾�� <- as.numeric(press_tasu$���̾��)
press_tasu$�ν��� <- as.numeric(press_tasu$�ν���)


summary(press_tasu$���̾��)
hist(press_tasu$���̾��)

press_cut <- transform(press_tasu, 
                       cut = cut(���̾��, breaks = c(0, 0.8, 0.85, 0.9, 0.95, 1.0, 5.0, 7.0 , 15),
                                     include.lowest = TRUE,
                                     right = FALSE,
                                     labels = c("~80", "80~90", "90~100", "100~500", "500~700", "700~1500")))

press_cut_w <- transform(press_tasu,
                         cut = cut(���̾��, breaks = c(0, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 15),
                                      include.lowest = TRUE,
                                      right = FALSE,
                                      labels = c("~70", "~75", "~80", "~85", "~90", "~95", "~100", "~110", "~120", "~130", "~140", "~150", "~160", "~170", "~180", "~190", "~200", "200~")))



library(dplyr)
library(ggplot2)

press_cut_w %>% 
  ggplot(aes(x=cut, fill=�Ǻ��ΰ��)) +
  geom_bar(position = "fill") +
  xlab("Ÿ������") +
  ylab("�ҷ���") +
  facet_wrap(~����, ncol=1) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")



table(press_cut_w$cut, press_cut_w$�Ǻ��ΰ��)


summary(press_tasu$�ν���)
hist(press_tasu$�ν���)
press_cut_insu <- transform(press_tasu,
                       cut = cut(�ν���, breaks = c(0, 0.8, 0.85, 0.9, 0.95, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 15),
                                    include.lowest = TRUE,
                                    right = FALSE,
                                    labels = c("~80", "~85", "~90", "~95", "~100", "~110", "~120", "~130", "~140", "~150", "~160", "~170", "~180", "~190", "~200", "200~")))



hist(press_cut_insu$�ν���)
#�Ϲ� \r\nth?????ng 
#�־���\r\nNoi doi

press_cut_insu %>% 
  filter(���� == "Seal") %>% 
  ggplot(aes(x=cut, fill=��ü�ΰ��)) +
  geom_bar(position = "fill") +
  xlab("Ÿ������") +
  ylab("��ü�� ��� ���� ����") +
  facet_wrap(~����, ncol=1) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")


insu_n <- press_cut_insu %>% group_by(cut, ����, ��ü�ΰ��) %>% 
  filter(���� == "Seal") %>% 
  summarise(n=n())

press_cut_insu %>% group_by(cut) %>% 
  filter(���� == "�Ϲ� \r\nth?????ng") %>% 
  summarise(n=n())

table(press_cut_insu$cut, press_cut_insu2$��ü�ΰ��)


str(press_cut)

table(press_cut_insu$cut, press_cut_insu$��ü�ΰ��)
chisq.test(press_cut_insu$cut, press_cut_insu$��ü�ΰ��)


install.packages("descr")
library(descr)
CrossTable(press_cut_insu$cut, press_cut_insu2$��ü�ΰ��,
           prop.r = FALSE, prop.c = FALSE, prop.t = FALSE,
           chisq = TRUE, expected=TRUE)


