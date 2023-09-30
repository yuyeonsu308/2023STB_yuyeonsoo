#히스토그램
hist(finedust$`3_ultrafine dust`, main = "서울시 서대문구 2020년 1월 초미세먼지 측정분포", col=terrain.colors(12))
#확률밀도그래프(Y축 값 확률로 변경)
hist(finedust$`3_ultrafine dust`, main="서울시 서대문구 초미세먼지 측정분포", col = terrain.colors(12),freq = FALSE)
#히스토그램의 확률밀도를 선으로 추가
lines(density(finedust$`3_ultrafine dust`), lwd=2)