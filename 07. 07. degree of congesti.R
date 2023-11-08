library(dplyr)
library(ggplot2)

#csv형식의 파일 불러와서 subway객체에 임력하고 구조 확인
str(congestion)
#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#첫차 결측치가 있는 행 제거
congestion1 <- congestion[!is.na(congestion$s0530),]
colSums(is.na(congestion1))

#23시30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))
#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00    6.30   10.70   14.68   18.90  109.00 

#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330','s0000','s0030')])
#수도권 지하철의 하루평균 혼잡도
mean(congestion1$day_mean)
[1] 25.9822
#지하철 호선별 하루 혼잡도
#지하철 호선별 출근시간(7~9)대의 평균혼잡도
#기술통계분석 결과 포함
#평균혼잡도가 제일 높은 시간대를 막대그래프로 그리기
#평균혼잡도 상위 4개 호선의 역별 기여도

#출발시간 8시의 지하철 혼잡도 범주화/범주별 빈도분석
#출발시간 8시, 호선별 "caution"빈도와 caution이 전체등급에서 차지하는 비율계산

#지하철 호선별 퇴근시간 18~20대의 평균 혼잡도
#기술통계분석 결과 포함
#평균혼잡도가 제일 높은 시간대를 막대그래프로 그리기
#평균혼잡도 상위 4개 호선의 역별 기여도

#출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석
#출발시간 18시, 호선별 "bad"빈도와 bad가 전체 등급에서 차지하는 비율계산


