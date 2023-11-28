#언데분 13주차 메모

#중간고사 리뷰


#알파는 적분된 넓이
#이때 t-value가 x값임


#14_t_test_alter.pdf
#

#2p
#ㅜnormality test 했는데 아니어서
#(정규분포가 아니라면)
#non-parametric으로 한다

#분석할 때는
#테스트를 하고, 왜 이 테스트를 선택했는지 어떻게 돌렸는지에대한 내용 포함

library(languageR)
data(ratings)
head(ratings)
View(ratings)
#3p
#dependent한 데이터임
#동일한 아몬드에대해 Weight과 Size로 분석(동일한 대상에대해 2번을 다른 방법으로 측정)
#->paired t-test

#4p
##dependent한 데이터니까
-> paired t-test 진행
#parametric t-test 할 수 있는지 체크


#5~6p 
#그게 바로 nomality 체크

md = ratings
boxplot(md$meanWeightRating, md$meanSizeRating, names=c('weight', 'size'))
boxplot(md$meanWeightRating - md$meanSizeRating, names = "difference", ylab = "mean rating difference")

#두개의 차(-)로 바로 boxplot으로 그려줌
#skew도 구할 수 있음

#7p

#null hypohtesis는 우리가 원하는 것의 반대.
#p가 커야된다


알파레벨이 0.05면 데이터가 노말하지 않다. 0.01로 선택하면 데이터가 노말하다 이다.

#일단 많이 쓰이는 0.05 선택
#윌콕슨 테스트 할 것임

#8p
a

#9p 만약 실수로 t-test 한다면
#어차피 결과가 H0 나온건 우연의 일치,

#여기까지 한것들 실습. boxplot histogram , 샤필오일, 윌콕스 테스트

#--------1교시 끝----

#10~11p
am = c(144, 147, 148, 172, 170, 133, 141, 144, 146, 148, 152, 149, 143, 138, 165, 143, 142, 155, 135, 144)
pm = c(133, 138, 139, 160, 165, 133, 140, 142, 144, 142, 139, 144, 139, 130, 164, 140, 128, 129, 133, 144)

shapiro.test(am-pm)
wilcox.test(am, pm, paired=T)

#
#noramality 결과 해석하고, 그 다음 테스트들 하는 것
#시험에서는 그 경우에 맞춰 테스트 선택해야함

#12p
#나이 많은, 적은 사람들간의 변수 비교
yng = c(5.2, 4.9, 5.7, 5.1, 4.9, 4.8, 5.7, 6.5, 4.1, 4.9)
old = c(5.3, 5.1, 4.2, 4.2, 5.0, 5.6, 3.9, 4.1, 4.4, 5.0)

#13p
independent한 데이터니까 각자 샤피로 테스트 해줘야함

shapiro.test(yng)
shapiro.test(old)

wilcox.test(yng, old)

#이거 녹음 돌리면서 다시 공부해야겠다, 진도를 못따라가고 있어

#노말하니까 각자 돌린 것이다. 중요!! 
##논페어 안했다고?
#아래 윌콕스는 그냥 한 것임.


#15_linear_regression.pdf

#2p
#3p
#한 변수가 움직일 때 다른 변수가 얼마나 움직이는지.

#선을 가지고 관계를 설명하는 것임.
#4p
#기울기인 a와 절편인 b만 있으면 됨

#5p
# ~ 표시는, y ~ x로 많이 표시됨.

lm(formula = meanSizeRating ~ meanWeightRating, data = ratings)

plot(ratings$meanWeightRating, ratings$meanSizeRating, col = 'red')

lm.ratings = lm(meanSizeRating ~ meanWeightRating, data=ratings)
abline(lm.ratings)#lm으로 모델링한 값 넣기
#abline(0.527, 0.926)

cor(ratings$meanWeightRating, ratings$meanSizeRating)

#6p 예측
new.weight = data.frame(meanWeightRating = 2.5)
new.size = predict(lm.ratings, new.weight)
new.size

new.size = predict(lm.ratings, 2,5)#에러남. 데이터프레임 자료형으로만 가능함

#7p
am = c(144, 147, 148, 172, 170, 133, 141, 144, 146, 148, 152, 149, 143, 138, 165, 143, 142, 155, 135, 144)
pm = c(133, 138, 139, 160, 165, 133, 140, 142, 144, 142, 139, 144, 139, 130, 164, 140, 128, 129, 133, 144)

lm.tt <- lm(formula = pm ~ am)
plot(am, pm, col='red')
abline(lm(pm ~ am))

#or abline(lm.tt)
new.am = data.frame(am = 128)
new.pm = predict(lm.tt, new.am)
new.pm


#8p 레포트까지 잘써서 과제하듯이 철저히 준비해서 다음주 발표 하기
#열심히 잘해가자!!


#추가점수 2점임!! ㄴㅇㅅ 

#기말은 16주차. 12시.






