#실습에 필요한 패키지 설치
install.packages("dplyr")
install.packages("ggplot2")

#library 설치
library(dplyr)
library(ggplot2)

#구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)

#결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시 30분 출발기차의 결측치 제거
congestion1 <- congestion[!is.na(congestion$s2330),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))
#이상치 확인
ggplot(congestion1,aes(y=s0530))+
  geom_boxplot()

summary(congestion1$s0530)

#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330','s0000','s0030')])

str(congestion1$day_mean)
mean(congestion1$day_mean)

#2. 지하철 호선별 하루 평균 혼잡도
table(congestion1$line)
line1 <- subset(congestion1,congestion1$line == "1")
line2 <- subset(congestion1,congestion1$line == "2")
line3 <- subset(congestion1,congestion1$line == "3")
line4 <- subset(congestion1,congestion1$line == "4")
line5 <- subset(congestion1,congestion1$line == "5")
line6 <- subset(congestion1,congestion1$line == "6")
line7 <- subset(congestion1,congestion1$line == "7")
line8 <- subset(congestion1,congestion1$line == "8")

mean(line1$day_mean)
mean(line2$day_mean)
mean(line3$day_mean)
mean(line4$day_mean)
mean(line5$day_mean)
mean(line6$day_mean)
mean(line7$day_mean)
mean(line8$day_mean)

#3. 지하철 호선별 출근시간(07:00~09:00)대의 평균 혼잡도
line1go <- subset(line1, select = c("s0700","s0730","s0800","s0830","s0900"))
line1go$day_mean <- rowMeans(line1go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line1go$day_mean)

line2go <- subset(line2, select = c("s0700","s0730","s0800","s0830","s0900"))
line2go$day_mean <- rowMeans(line2go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line2go$day_mean)

line3go <- subset(line3, select = c("s0700","s0730","s0800","s0830","s0900"))
line3go$day_mean <- rowMeans(line3go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line3go$day_mean)

line4go <- subset(line4, select = c("s0700","s0730","s0800","s0830","s0900"))
line4go$day_mean <- rowMeans(line4go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line4go$day_mean)

line5go <- subset(line5, select = c("s0700","s0730","s0800","s0830","s0900"))
line5go$day_mean <- rowMeans(line5go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line5go$day_mean)

line6go <- subset(line6, select = c("s0700","s0730","s0800","s0830","s0900"))
line6go$day_mean <- rowMeans(line6go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line6go$day_mean)

line7go <- subset(line7, select = c("s0700","s0730","s0800","s0830","s0900"))
line7go$day_mean <- rowMeans(line7go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line7go$day_mean)

line8go <- subset(line8, select = c("s0700","s0730","s0800","s0830","s0900"))
line8go$day_mean <- rowMeans(line8go[,c("s0700","s0730","s0800","s0830","s0900")])
mean(line8go$day_mean)

summary(line1go$day_mean)
summary(line2go$day_mean)
summary(line3go$day_mean)
summary(line4go$day_mean)
summary(line5go$day_mean)
summary(line6go$day_mean)
summary(line7go$day_mean)
summary(line8go$day_mean)

summary(line1go$s0700)
summary(line1go$s0730)
summary(line1go$s0800)
summary(line1go$s0830)
summary(line1go$s0900)

go <- congestion1%>%
  group_by(congestion1$line)%>%
  summarize(mean_s0700=mean(s0700),mean_s0730=mean(s0730),mean_s0800=mean(s0800),mean_s0830=mean(s0830),mean_s0900=mean(s0900))

s0700 = c(go$mean_s0700)
s0730 = c(go$mean_s0730)
s0800 = c(go$mean_s0800)
s0830 = c(go$mean_s0830)
s0900 = c(go$mean_s0900)

df = data.frame('s0700'=s0700, 's0730'=s0730,'s0800'=s0800,'s0830'=s0830,'s0900'=s0900)

barplot(as.matrix(df),beside = T,ylim = c(0,50),col=rainbow(nrow(df)))
legend(0,50,c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선"),cex=0.8,fill=rainbow(nrow(df)))

#4.8시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%
  mutate(s80_grade=
           ifelse (s0800<=80, "good", ifelse (s0800<=130, "normal", ifelse (s0800<=150, "caution","bad"))))%>%
  group_by (s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))
#4-1. 호선별로 8시 지하철 혼잡도 범주화
congestion1 %>%
  mutate(s80_grade=
           ifelse (s0800<=80, "good", ifelse (s0800<=130, "normal", ifelse (s0800<=150, "caution","bad"))))%>%
  group_by (line, s80_grade) %>%
  summarise (n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line, s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(8)

#5. 지하철 호선별 퇴근시간(18:00~20:00)대의 평균 혼잡도
line1out <- subset(line1, select = c("s1800","s1830","s1900","s1930","s2000"))
line1out$day_mean <- rowMeans(line1out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line1out$day_mean)
summary(line1out$day_mean)

line2out <- subset(line2, select = c("s1800","s1830","s1900","s1930","s2000"))
line2out$day_mean <- rowMeans(line2out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line2out$day_mean)
summary(line2out$day_mean)

line3out <- subset(line3, select = c("s1800","s1830","s1900","s1930","s2000"))
line3out$day_mean <- rowMeans(line3out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line3out$day_mean)
summary(line3out$day_mean)

line4out <- subset(line4, select = c("s1800","s1830","s1900","s1930","s2000"))
line4out$day_mean <- rowMeans(line4out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line4out$day_mean)
summary(line4out$day_mean)

line5out <- subset(line5, select = c("s1800","s1830","s1900","s1930","s2000"))
line5out$day_mean <- rowMeans(line5out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line5out$day_mean)
summary(line5out$day_mean)

line6out <- subset(line6, select = c("s1800","s1830","s1900","s1930","s2000"))
line6out$day_mean <- rowMeans(line6out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line6out$day_mean)
summary(line6out$day_mean)

line7out <- subset(line7, select = c("s1800","s1830","s1900","s1930","s2000"))
line7out$day_mean <- rowMeans(line7out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line7out$day_mean)
summary(line7out$day_mean)

line8out <- subset(line8, select = c("s1800","s1830","s1900","s1930","s2000"))
line8out$day_mean <- rowMeans(line8out[,c("s1800","s1830","s1900","s1930","s2000")])
mean(line8out$day_mean)
summary(line8out$day_mean)

out <- congestion1%>%
  group_by(congestion1$line)%>%
  summarize(mean_s1800=mean(s1800),mean_s1830=mean(s1830),mean_s1900=mean(s1900),mean_s1930=mean(s1930),mean_s2000=mean(s2000))

s1800 = c(out$mean_s1800)
s1830 = c(out$mean_s1830)
s1900 = c(out$mean_s1900)
s1930 = c(out$mean_s1930)
s2000 = c(out$mean_s2000)

df1 = data.frame('s1800'=s1800, 's1830'=s1830,'s1900'=s1900,'s1930'=s0830,'s2000'=s2000)

barplot(as.matrix(df1),beside = T,ylim = c(0,50),col=rainbow(nrow(df)))
legend("topright",c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선"),cex=0.8,fill=rainbow(nrow(df)))

#6. 18시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%
  mutate(s18_grade=
           ifelse (s1800<=80, "good", ifelse (s1800<=130, "normal", ifelse (s1800<=150, "caution","bad"))))%>%
  group_by (s18_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s18_grade,n,pct)%>%
  arrange(desc(n))

#6-1. 호선별로 18시 지하철 혼잡도 범주화
congestion1 %>%
  mutate(s18_grade=
           ifelse (s1800<=80, "good", ifelse (s1800<=130, "normal", ifelse (s1800<=150, "caution","bad"))))%>%
  group_by (line, s18_grade) %>%
  summarise (n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s18_grade=="bad")%>%
  select(line, s18_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)