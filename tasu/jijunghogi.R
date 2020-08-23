
### ����ȣ��(ǰ�������� �ٽ� �� �����ͷ� Ȯ��) �� �ҷ��� ����

library(readxl)

jj <- read_xlsx("S:/����������Ȳ.xlsx", sheet = "Sheet2")


jj$jijung2 <- ifelse(jj$�������� == jj$�۾�ȣ��, jj$jijung2 <- "O", jj$jijung2 <- "X")
table(jj$jijung2)

colSums(is.na(jj))

summary(jj$ngrate)
jj %>% group_by(jijung2) %>% 
  filter(ngrate <= 0.09) %>% 
  summarise(mean=mean(ngrate, na.rm=T),
            n=n(),
            sd = sd(ngrate, na.rm=T))

library(dplyr)
library(ggplot2)

summary(jj$ngrate)

jj %>% 
  filter(ngrate <= 0.09) %>% 
  ggplot(aes(x=jijung2, y=ngrate, color=jijung2)) +
  # geom_point() +
  geom_boxplot() +
  xlab("����ȣ�⿩��") +
  ylab("�ҷ���") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")


j <- jj %>% filter(ngrate <= 0.09 )

out <- aov(j$ngrate ~ j$jijung2)
summary(out)
tukey = TukeyHSD(out)
tukey


