#2Step 1개 인자 도수분포표 
table(X2023_STB_survey $Gender)

#3step 1개 인자 상대도수분포표
ECN <- table(X2023_STB_survey $Gender)
prop.table(ECN)
#4step 2개 인자 교차표
table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)
#5step 1개 인자 막대그래프
barplot(table(X2023_STB_survey $Nationality))
#6step 1개 인자 가로막대그래프
barplot(table(X2023_STB_survey $`residential area`))
barplot(table(X2023_STB_survey $`residential area`), horiz = TRUE)
#7step 2개 인자 막대그래프
barplot(table(X2023_STB_survey $Gender, X2023_STB_survey $Grade))
#8step 1개 인자 파이차트
pie(table(X2023_STB_survey $Grade))
#9step 히스토그램
hist(X2023_STB_survey $Age, main = "경영통계분석2 나이분포", xlab = "나이",ylab = "frequency", col="lightblue")
#10step
boxplot(Age ~ Grade, data = X2023_STB_survey, 
        main = "grade별 age", xlab = "Grade", ylab = "age")
#11step 산점도
plot(x=X2023_STB_survey $Grade, y=X2023_STB_survey$Age, xlab="grade", ylab="age")
              
