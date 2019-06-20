ggplot(data = region_satisfaction, aes(x=region, y=pct, fill = code_satisfaction)) + 
  geom_col() + coord_flip() + scale_x_discrete(limits = order)


#coord_flip() 막대그래프를 옆으로 눕히는 명령어
#scale_x_discreate(limits) 그래프의 나타내는 순위를 조정할 수 있는 명령어


#09-8 종교유무에 따른 이혼율
#round함수는 round(수 , 소숫자리 반올림)
#group_by로 종교와 결혼을 그룹별로 확인

install.packages("foreign")
library(foreign)   #spss파일 불러오기
library(dplyr)     #전처리
library(ggplot2)   #시각화
library(readxl)    #엑셀파일 불러오기

raw_welfare <- read.spss(file = "koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)               #데이터 프레임 형태로 불러옴
welfare <- raw_welfare

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_religion = h10_reg7)


#09-6 직업별 월급차이
list_job <- read_excel("koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)

welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(10)
 
job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))

head(job_income)

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) + geom_col() + coord_flip()



#09-7 성별 직업 빈도
job_male <- welfare %>%
  filter(!is.na(job) & sex == 1) %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male

job_female <- welfare %>%
  filter(!is.na(job) & sex == 2) %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female

ggplot(data = job_male, aes(x = reorder(job, n), y = n)) + geom_col() + coord_flip()

ggplot(data = job_female, aes(x= reorder(job, n), y = n)) + geom_col() + coord_flip()


# scale_x_discrete : 그래프 만들기할때 높은 순서대로 데이터를 나열할 수 있음, 259페이지 참조
# character를 factor로 변경하여 levels로 인식하게 하면 원하는 문자의 순서대로 그래프 나열 가능, 260페이지 참조

welfare$
  
#09-4 연련대에 따른 월급차이
welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young", 
                       ifelse <= "middle", "old")) 

install.packages("rJava")
install.packages("memoise")    
install.packages("KoNLP")

library("rJava")
library(KoNLP)
Sys.setenv(JAVA_HOME = "C:\Program Files (x86)\Java\jre1.8.0_211")

