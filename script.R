#library import
library('tidyverse')



# 꺾은선 그래프 그리기(년도별 교통사고 건수 )
ggplot(data = count_traffic_accident_by_year, aes(x = 사고년도, y = count)) + geom_line() + geom_point() + labs(title="년도별 교통사고 건수", x = "년도", y = "사고건수")


# 년도별 가해자법규위반(원인)별 사망자수
ggplot(data = traffic_accident, aes(x = 사고년도, y = 사망자수, fill=가해자법규위반)) + geom_bar(stat = "identity", position=position_dodge())

# 년도별 가해자법규위반(원인)별 부상자수
ggplot(data = traffic_accident, aes(x = 사고년도, y = 부상자수, fill=가해자법규위반)) + geom_bar(stat = "identity", position=position_dodge())

# Top3 : 안전운전의무 불이행, 중앙선 침법, 신호위반 Bottom 3 : 교차로 통행방법 위반, 안전거리 마확보, 보행자 보호의무 위반

# 가해자법규위반별 총 부상자수를 알기위한 tibble과 시각화
tmp7 <- traffic_accident %>% group_by(가해자법규위반) %>% summarise(부상자수=sum(부상자수))
ggplot(data=tmp7, aes(x=가해자법규위반, y=부상자수)) + geom_bar(stat = "identity", position=position_dodge(), fill='#FFA800') + geom_text(aes(label=부상자수), color='black')
# 가해자법규위반별 평균 부상자수를 알기위한 tibble과 시각화
tmp8 <- traffic_accident %>% group_by(가해자법규위반) %>% summarise(부상자수=mean(부상자수))
ggplot(data=tmp8, aes(x=가해자법규위반, y=부상자수)) + geom_bar(stat = "identity", position=position_dodge(), fill='#FFA800') + geom_text(aes(label=부상자수), color='black')


# 도로형태 별 사망자, 부상자수 ( 시각화 안해놓음..ㅎ )

tmp9 <- traffic_accident %>% group_by(`도로형태`) %>% summarise(총사망자=sum(사망자수), 총부상자=sum(부상자수), 평균=sum(사망자수)/sum(부상자수))
tmp9

# 사망자/부상자가 많은 사고유형을 알아내기 위한 tibble
tmp10 <- traffic_accident %>% group_by(`사고유형`) %>% summarise(사망자수 = sum(사망자수), 부상자수 = sum(부상자수), 중상자수 = sum(중상자수), 경상자수 = sum(경상자수))
tmp10

# tpm9, tmp10에서 구한 사망자/부상자가 많은 사고유형에서 기타를 제외한 상위 2개만 필터링(분석 편하게 하려고)
tmp11 <- traffic_accident %>% filter(사고유형 == '측면충돌' | 사고유형 == '횡단중' | 사고유형 == '기타')

#측면충돌이랑 횡단중 사고유형이 주간,야간 중 어느 시간대에 많이 일어나는 지 알기 위한 식
# true면 주간이 더 많음, false면 야간이 더 많음
count(tmp11, 사고유형 == '측면충돌' & 주야구분 == '주간')$n[[2]] > count(tmp11, 사고유형 == '측면충돌' & 주야구분 == '야간')$n[[2]]
count(tmp11, 사고유형 == '횡단중' & 주야구분 == '주간')$n[[2]] > count(tmp11, 사고유형 == '횡단중' & 주야구분 == '야간')$n[[2]]

# 위에서 구한 사고유형의 가해자 법규 위반을 분석하기위함
tmp12 <- tmp11 %>% filter(사고유형 == '측면충돌') %>% group_by(가해자법규위반) %>% summarise(n = n())
tmp12a <- tmp11 %>% filter(사고유형 == '횡단중') %>% group_by(가해자법규위반) %>% summarise(n = n())
# 위에서 구한 사고유형의 도로형태를 분석하기위함
tmp13 <- tmp11 %>% filter(사고유형 == '측면충돌') %>% group_by(도로형태) %>% summarise(n = n())
tmp13a <- tmp11 %>% filter(사고유형 == '횡단중') %>% group_by(도로형태) %>% summarise(n = n())
