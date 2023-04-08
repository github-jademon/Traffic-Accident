#library import
library('tidyverse')

# 최종분석

# 분석1
# 1. 년도별 사망교통사고 발생 건수(꺾은선 그래프)
ggplot(data = count_traffic_accident_by_year, aes(x = 사고년도, y = count)) + 
  geom_line() + 
  geom_point() + 
  labs(title="년도별 교통사고 건수", x = "년도", y = "사고건수") + 
  scale_y_continuous(limits = c(0, 6000))

# 분석2
# 1. 가해자 법규 위반에 따른 사고 건수(막대 그래프)
tmp1 <- traffic_accident %>% group_by(가해자법규위반) %>% summarise(사고건수 = n())
ggplot(data=tmp1, aes(x=reorder(가해자법규위반, -사고건수), y=사고건수)) + 
  geom_bar(stat = "identity", position=position_dodge(), fill='#FFA800') + 
  geom_text(aes(label=사고건수), color='black') + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(title="가해자 법규 위반에 따른 사고건수", x='가해자 법규 위반')

# 2. 설문조사 결과(어떤 이유로 사고가 많이 발생할까)(막대 그래프)
data <- data.frame(
  사고유형=c('안전운전 의무 불이행', '신호위반', '중앙선 침범', '과속', '보행자 보호의무 위반', '기타', '안전거리 미확보', '교차로 통행방법 위반'),
  응답건수=c(11, 5, 0, 2, 0, 0, 7, 0)
)

data  %>% arrange(desc(응답건수)) %>% 
  ggplot(aes(reorder(x=사고유형, -응답건수),y=응답건수)) +
  geom_bar(stat="identity", color="white", fill='#FFA800', position=position_dodge()) + 
  geom_text(aes(label=응답건수), color='black') + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(title="설문조사 결과 (어떤 이유로 사고가 많이 발생할까)", x='가해자 법규 위반')


# 분석3
# 1. 도로형태별 총사망자와 총부상자자(막대 그래프)
tmp2 <- traffic_accident %>% group_by(`도로형태`) %>% 
  summarise(총사망자=sum(사망자수), 총부상자=sum(부상자수)) %>% 
  arrange(desc(총사망자)) %>% 
  pivot_longer(c(`총사망자`, `총부상자`), names_to = "type", values_to ="n")

ggplot(data=tmp2, aes(x=reorder(`도로형태`, -n), y=n, fill=type)) + 
  geom_bar(position = position_dodge(0.8), stat = "identity", size=.4, width=0.8) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, size=10)) + 
  labs(title="도로형태별 총사망자와 총부상자", x='도로형태', y = '사고인원')

# 분석4
# 1. 사고유형별 사망자수와 부상자수(표)
tmp3 <- traffic_accident %>% group_by(`사고유형`) %>% 
  summarise(사망자수 = sum(사망자수), 부상자수 = sum(부상자수)) %>% 
  pivot_longer(c(`사망자수`, `부상자수`), names_to = "type", values_to ="n")
tmp3

# 2. 사고유형별 사망자수와 부상자수(막대그래프)
ggplot(data=tmp3, aes(x=reorder(사고유형, -n), y=n, fill=type)) + 
  geom_bar(position = position_dodge(0.8), stat = "identity", size=.4, width=0.8) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, size=10)) + 
  labs(title="사고유형별 사망자수와 부상자수", x='사고유형', y = '사고인원')

# 3. 사고유형의 상위 3가지 사망률(기타제외)(막대그래프)
tmp4 <- traffic_accident %>% group_by(`사고유형`) %>% 
  summarise(사망자수 = sum(사망자수), 
            부상자수 = sum(부상자수) - sum(사망자수),
            평균 = sum(부상자수)/sum(사망자수)*100) %>% 
  arrange(desc(사망자수)) %>% 
  filter(사고유형 == '횡단중' | 사고유형 == '측면충돌' | 사고유형 == '공작물충돌') %>% 
  pivot_longer(c(`사망자수`, `부상자수`), names_to = "type", values_to ="n")


ggplot(tmp4) +
  geom_bar(aes(x=reorder(`사고유형`, 평균), y = n, fill = type), 
           stat='identity', position='fill') +
  theme_classic() +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, size=10)) + 
  labs(title="사고유형의 상위 3가지 사망률(기타제외)", x='사고유형', y = '사망률') + 
  scale_fill_manual(values = c("#dddddd", "#F8766D"))

# 4. 횡단중의 경우 주야별 사고율 비교(원그래프프)
tmp5 <- traffic_accident %>% filter(사고유형 == '횡단중')
tmp6 <- tmp5 %>%
  group_by(주야구분) %>%
  summarise(n=n()) %>% 
  mutate(pct=n/sum(n)*100)
ggplot(tmp6, aes(x='', y=pct, fill=주야구분)) +
  labs(title="횡단중") +
  geom_bar(stat='identity', color=NA) +
  theme_void() +
  coord_polar('y', start=0) +
  geom_text(aes(label=paste0(round(pct,1), '%')), position=position_stack(vjust=0.5))

# 5. 측면충돌의 경우 주야별 사고율 비교(원그래프)
tmp7 <- traffic_accident %>% filter(사고유형 == '측면충돌')
tmp8 <- tmp7 %>%
  group_by(주야구분) %>%
  summarise(n=n()) %>% 
  mutate(pct=n/sum(n)*100)
ggplot(tmp8, aes(x='', y=pct, fill=주야구분)) +
  labs(title="측면충돌") +
  geom_bar(stat='identity', color=NA) +
  theme_void() +
  coord_polar('y', start=0) +
  geom_text(aes(label=paste0(round(pct,1), '%')), position=position_stack(vjust=0.5))

# 6. 횡단중의 경우에 따른 도로형태(막대 그래프프)
tmp9 <- traffic_accident %>% filter(사고유형 == '횡단중') %>% group_by(도로형태) %>%
  summarise(사고수 = n()) %>% arrange(desc(사고수))
ggplot(data=tmp9, aes(x=reorder(도로형태, -사고수), y=사고수)) + 
  geom_bar(stat = "identity", position=position_dodge(), fill='#FFA800') + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(title="횡단중의 경우에 따른 도로형태", x='도로형태')

# 7. 측면충돌의 경우에 따른 도로형태(막대 그래프)
tmp10 <- traffic_accident %>% filter(사고유형 == '측면충돌') %>% 
  group_by(도로형태) %>% summarise(사고수 = n()) %>% arrange(desc(사고수))
ggplot(data=tmp10, aes(x=reorder(도로형태, -사고수), y=사고수)) + 
  geom_bar(stat = "identity", position=position_dodge(), fill='#FFA800') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(title="측면충돌의 경우에 따른 도로형태", x='도로형태')
