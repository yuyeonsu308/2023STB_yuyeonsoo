#히스토그램
hist(finedust$`3_ultrafine dust`, main = "서울시 서대문구 2020년 1월 초미세먼지 측정분포", col=terrain.colors(12))
 #확률밀도그래프(Y축 값 확률로 변경)
hist(finedust$`3_ultrafine dust`, main="서울시 서대문구 초미세먼지 측정분포", col = terrain.colors(12),freq = FALSE)
 #히스토그램의 확률밀도를 선으로 추가
lines(density(finedust$`3_ultrafine dust`), lwd=2)
#박스플롯
boxplot(finedust$`3_fine dust`, main = "야식업의 2020년 1월 미세먼지 발생현황", col = "yellow")
 #박스플롯 2개 비교
boxplot(finedust$`3_fine dust`, finedust$`7_fine dust`, main = "업종별 2020년 1월 미세먼지 발생현황", col="yellow", names = c("야식업","중식"))
#산점도
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main = "미세먼지와 초미세먼지의 변화")
 #산점도 편집
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main = "미세먼지와 초미세먼지의 변화", pch=24, col="red", bg="yellow", cex=1.5)
 #type에 따른 산점도 그래프 변화
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main = "미세먼지와 초미세먼지의 변화", type = "h")
