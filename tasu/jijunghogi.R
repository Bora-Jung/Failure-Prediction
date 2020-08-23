
### 지정호기(품질팀에서 다시 준 데이터로 확인) 별 불량률 차이

library(readxl)

jj <- read_xlsx("S:/금형지정현황.xlsx", sheet = "Sheet2")


jj$jijung2 <- ifelse(jj$지정금형 == jj$작업호기, jj$jijung2 <- "O", jj$jijung2 <- "X")
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
  xlab("지정호기여부") +
  ylab("불량률") +
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



