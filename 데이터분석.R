# jdk 지정
Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/jdk-15.0.1.jdk/Contents/Home')
dyn.load('/Library/Java/JavaVirtualMachines/jdk-15.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')
Sys.getenv("JAVA_HOME")

# rJava 패키지 
install_jdk()
install.packages("rJava")
library(rJava)

# jdk 사용 패키지 다운로드 
install.packages("memoise")
install.packages("multilingeur")

#KoNLP 패키지 
install.packages('devtools')
devtools::install_github('haven-jeon/KoNLP')

install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos = NULL, type="source", INSTALL_opts = c('--no-lock'))
library('KoNLP')

install.packages(c('stringr', 'hash','tau','Sejong','RSQLite','devtools'), type = "binary")
# install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.0-7.4.tar.gz", repos = NULL, type = "soirce", INSTALL_opts = c('--no-lock'))
# install.packages("KoNLP")
install.packages("remotes")

# 분석, 워드클라우드 
install.packages("dplyr")
install.packages("stringr")
install.packages("RColorBrewer")
install.packages("wordcloud")

#라이브러리
library(memoise)
library(stringr)
library(hash)
library(tau)
library(Sejong)
library(RSQLite)
library(devtools)
library(KoNLP)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(reshape)
library(stats)
library(base)

# 파생변수 생성 라이브러리
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)

#세종사전 사용
useSejongDic()
#userDic <- data.frame(term = c("노사연","송가인","손가인"))
#buildDictionary(ext_dic = 'sejong' user_dic = userDic)

# 엑셀 읽어오기
install.packages('readxl')
library(readxl) 

setwd("/Users/parkjihyun/Desktop/leesohee")
data_1 <- read_excel("data2.xlsx", skip = 2)

# 성별(남:1/여:2)
# 연령(-1: 미상, 60대, 70대, 80대) 
# 발화문
# 건수 
View(data_1)

#변수명 바꾸기
names(data_1) <- c("gender","age","speak","count")
View(data_1)

# 문자열 데이터 가공 처리
install.packages("stringr")
library(stringr)

# 데이터 전처리
data_1$speak <- gsub('노래', '', data_1$speak)
data_1$speak  <- gsub('날씨', '', data_1$speak )
data_1$speak  <- gsub('영상', '', data_1$speak )
data_1$speak  <- gsub('얘기', '', data_1$speak )
data_1$speak  <- gsub('오늘', '', data_1$speak )
data_1$speak  <- gsub('통화', '', data_1$speak )
data_1$speak  <- gsub('전화', '', data_1$speak )
data_1$speak  <- gsub('우리', '', data_1$speak )
data_1$speak  <- gsub('있습니', '', data_1$speak )
data_1$speak  <- gsub('말씀', '', data_1$speak )
data_1$speak  <- gsub('시간', '', data_1$speak )
data_1$speak  <- gsub('엄마', '', data_1$speak )
data_1$speak  <- gsub('소리', '', data_1$speak )
data_1$speak  <- gsub('안녕', '', data_1$speak )
data_1$speak  <- gsub('알겠습니', '', data_1$speak )
data_1$speak  <- gsub('하세', '', data_1$speak )
data_1$speak  <- gsub('먹었', '', data_1$speak )
data_1$speak  <- gsub('고맙습니', '', data_1$speak )
data_1$speak  <- gsub('가지', '', data_1$speak )
data_1$speak  <- gsub('감사', '', data_1$speak )
data_1$speak  <- gsub('필요', '', data_1$speak )
data_1$speak  <- gsub('괜찮', '', data_1$speak )
data_1$speak  <- gsub('어떠', '', data_1$speak )
data_1$speak  <- gsub('고맙', '', data_1$speak )
data_1$speak  <- gsub('때문', '', data_1$speak )
data_1$speak  <- gsub('맛있', '', data_1$speak )
data_1$speak  <- gsub('사람들', '', data_1$speak )
data_1$speak  <- gsub('다솜', '', data_1$speak )
data_1$speak  <- gsub('김포', '', data_1$speak )
data_1$speak  <- gsub('알았', '', data_1$speak )
data_1$speak  <- gsub('음악', '', data_1$speak )
data_1$speak  <- gsub('재밌는', '', data_1$speak )
data_1$speak  <- gsub('가슴', '', data_1$speak )
data_1$speak  <- gsub('이거', '', data_1$speak )
data_1$speak  <- gsub('노사', '노사연', data_1$speak )
data_1$speak  <- gsub('주현', '주현미', data_1$speak )
data_1$speak  <- gsub('손가인', '송가인', data_1$speak )

data_1$speak <- str_replace_all(data_1$speak,'손가인','송가인')
data_1$speak <- str_replace_all(data_1$speak,'노사','노사연')


##### 1. 가장 많이 실행한 명령어 

#### 1. 전체 -> 송가인, 할머니, 찬송가, 이미자, 아이, 사랑, 뉴스, 주현미, 나훈아, 노사연  

#명사 추출
nouns <- sapply(data_1$speak, extractNoun, USE.NAMES=F)
View(nouns)

# 명사를 벡터로 변환
nouns <- unlist(nouns)


# 단어별 빈도표 생성
wordcount <- Filter(function(x){nchar(x) >= 2}, nouns)
wordcount <- table(wordcount)
View(wordcount)

head(sort(wordcount, decreasing=T), 20)

#워드클라우드
pal <- brewer.pal(12,"Paired")    #색깔지정
set.seed(1234)

wordcloud(names(wordcount), 
          freq=wordcount, 
          scale=c(3,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal,
          max.word =30)


#### 2.  그룹별 

### (1)  남/여별

## (1) - 1.　남자 -> 송가인, 주현미, 이미자, 나훈아, 멜로디, 김영임, 아리랑, 확진자

data_gender_1 <- subset(data_1, gender == 1)

#명사 추출
nouns_gender_1 <- sapply(data_gender_1$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_gender_1 <- unlist(nouns_gender_1)

# 단어별 빈도표 생성
wordcount_gender_1 <- Filter(function(x){nchar(x) >= 2}, nouns_gender_1)
wordcount_gender_1 <- table(wordcount_gender_1)
head(sort(wordcount_gender_1, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_gender_1), 
          freq=wordcount_gender_1, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (1) - 2. 여자 -> 할머니, 찬송가, 아이, 이미자, 뮤스, 사랑, 공부, 걱정, 나훈아, 심심, 포켓몬스터, 궁금, 동요

data_gender_2 <- subset(data_1, gender == 2)

#명사 추출
nouns_gender_2 <- sapply(data_gender_2$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_gender_2 <- unlist(nouns_gender_2)


# 단어별 빈도표 생성
wordcount_gender_2 <- Filter(function(x){nchar(x) >= 2}, nouns_gender_2)
wordcount_gender_2 <- table(wordcount_gender_2)
head(sort(wordcount_gender_2, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_gender_2), 
          freq=wordcount_gender_2, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)




### (2) 연령대별 

## (2) - 1. 60대 -> 찬송가, 예수가, 건강, 볼륨, 아침, 영탁 

data_age_60 <- subset(data_1, age == 60)

#명사 추출
nouns_age_60 <- sapply(data_age_60$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_age_60 <- unlist(nouns_age_60)

# 단어별 빈도표 생성
wordcount_age_60 <- Filter(function(x){nchar(x) >= 2}, nouns_age_60)
wordcount_age_60 <- table(wordcount_age_60)
head(sort(wordcount_age_60, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_age_60), 
          freq=wordcount_age_60, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (2) - 2. 70대 -> 뉴스, 사랑, 할머니, 아이, 나훈아, 이미자, 포켓몬스터, 고민, 궁금, 멜로디, 클래식, 행복, 걱정, 공부 

data_age_70 <- subset(data_1, age == 70)

#명사 추출
nouns_age_70 <- sapply(data_age_70$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_age_70 <- unlist(nouns_age_70)

# 단어별 빈도표 생성
wordcount_age_70 <- Filter(function(x){nchar(x) >= 2}, nouns_age_70)
wordcount_age_70 <- table(wordcount_age_70)
head(sort(wordcount_age_70, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_age_70), 
          freq=wordcount_age_70, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)



## (2) - 3. 80대 -> 할머니, 이미자, 아이, 손가인, 찬송가, 동요, 대통령, 문주란 

data_age_80 <- subset(data_1, age == 80)

#명사 추출
nouns_age_80 <- sapply(data_age_80$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_age_80 <- unlist(nouns_age_80)

# 단어별 빈도표 생성
wordcount_age_80 <- Filter(function(x){nchar(x) >= 2}, nouns_age_80)
wordcount_age_80 <- table(wordcount_age_80)
head(sort(wordcount_age_80, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_age_80), 
          freq=wordcount_age_80, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


### (3) 연령대 남/여별 

## (3) -1. 60대 남자 -> 메뉴, 건강, 볼륨, 아침, 영탁, 고등어 
data_1_60 <- data_1 %>% filter(gender==1) %>% subset(age==60)
View(data_1_60)

#명사 추출
nouns_1_60 <- sapply(data_1_60$speak, extractNoun, USE.NAMES=F)
View(nouns_1_60)

# 명사를 벡터로 변환
nouns_1_60 <- unlist(nouns_1_60)

# 단어별 빈도표 생성
wordcount_1_60 <- Filter(function(x){nchar(x) >= 2}, nouns_1_60)
wordcount_1_60 <- table(wordcount_1_60)

head(sort(wordcount_1_60, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_1_60), 
          freq=wordcount_1_60, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)



## (3) -2. 60대 여자 -> 찬송가 ,예수가 

data_2_60 <- data_1 %>% filter(gender==2) %>% subset(age==60)

#명사 추출
nouns_2_60 <- sapply(data_2_60$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_2_60 <- unlist(nouns_2_60)

# 단어별 빈도표 생성
wordcount_2_60 <- Filter(function(x){nchar(x) >= 2}, nouns_2_60)
wordcount_2_60 <- table(wordcount_2_60)

head(sort(wordcount_2_60, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_2_60), 
          freq=wordcount_2_60, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)

## (3) -3. 70대 남자 -> 멜로디, 사랑, 영혼, 나훈아, 아리랑, 김영임, 설날, 재미, 조용필, 최진희
data_1_70 <- data_1 %>% filter(gender==1) %>% subset(age==70)

#명사 추출
nouns_1_70 <- sapply(data_1_70$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_1_70 <- unlist(nouns_1_70)

# 단어별 빈도표 생성
wordcount_1_70 <- Filter(function(x){nchar(x) >= 2}, nouns_1_70)
wordcount_1_70 <- table(wordcount_1_70)

head(sort(wordcount_1_70, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_1_70), 
          freq=wordcount_1_70, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (3) -4. 70대 여자 -> 할머니, 뉴스, 아이, 사랑, 이미자 ,포켓몬스터, 찬송가, 고민, 궁금, 나훈아, 클래식, 행복
data_2_70 <- data_1 %>% filter(gender==2) %>% subset(age==70)

#명사 추출
nouns_2_70 <- sapply(data_2_70$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_2_70 <- unlist(nouns_2_70)


# 단어별 빈도표 생성
wordcount_2_70 <- Filter(function(x){nchar(x) >= 2}, nouns_2_70)
wordcount_2_70 <- table(wordcount_2_70)
head(sort(wordcount_2_70, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_2_70), 
          freq=wordcount_2_70, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal,
          max.word =20)

## (3) -5. 80대 남자 -> 손가인, 주현, 옛날, 이미자, 기특, 영림이, 확진자, 김수희, 인터넷, 
data_1_80 <- data_1 %>% filter(gender==1) %>% subset(age==80)

#명사 추출
nouns_1_80 <- sapply(data_1_80$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_1_80 <- unlist(nouns_1_80)


# 단어별 빈도표 생성
wordcount_1_80 <- Filter(function(x){nchar(x) >= 2}, nouns_1_80)
wordcount_1_80 <- table(wordcount_1_80)
head(sort(wordcount_1_80, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_1_80), 
          freq=wordcount_1_80, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)

## (3) -6. 80대 여자 -> 할머니, 아이, 찬송가, 대통령, 문주란, 심심, 하루, 걱정, 대통령, 박정희 
data_2_80 <- data_1 %>% filter(gender==2) %>% subset(age==80)

#명사 추출
nouns_2_80 <- sapply(data_2_80$speak, extractNoun, USE.NAMES=F)

# 명사를 벡터로 변환
nouns_2_80 <- unlist(nouns_2_80)

# 단어별 빈도표 생성
wordcount_2_80 <- Filter(function(x){nchar(x) >= 2}, nouns_2_80)
wordcount_2_80 <- table(wordcount_2_80)
head(sort(wordcount_2_80, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_2_80), 
          freq=wordcount_2_80, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)



##### 2. 노래 - 가장 많이 틀어달라고 한 노래


# 엑셀 읽어오기
install.packages('readxl')
library(readxl) 

setwd("/Users/parkjihyun/Desktop/leesohee")
data_2 <- read_excel("data2.xlsx", skip = 2)
View(data_2)

#변수명 바꾸기
names(data_2) <- c("gender","age","speak","count")
View(data_2)


# 노래 파일만 가져오기 
grep("[노래,영상]", data_2$speak, value = T)
music <- grep("[영상,노래]", data_2$speak)

data_music <-data_2[music,]
View(data_music)

data_music$speak <- gsub('노래', '', data_music$speak)
data_music$speak <- gsub('영상', '', data_music$speak)
data_music$speak <- gsub('알았', '', data_music$speak)

### (1) 전체 -> 이미자, 주현, 나훈아, 손가인, 노사연, 문주란, 임영웅, 김영임, 멜로디, 베스트

#명사 추출
nouns_m <- sapply(data_music$speak, extractNoun, USE.NAMES=F)
View(nouns_m)

# 명사를 벡터로 변환
nouns_m <- unlist(nouns_m)

# 단어별 빈도표 생성
wordcount_m <- Filter(function(x){nchar(x) >= 2}, nouns_m)
wordcount_m <- table(wordcount_m)
View(wordcount_m)

head(sort(wordcount_m, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m), 
          freq=wordcount_m, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)

### (2) 남/여별 

## (2) - 1. 남자 -> 손가인, 주현미, 송가인, 이미자, 나훈아, 김영임, ㅇ멜로디, 영림이, 재미, 김수희, 김연자, 아리랑, 영탁 

data_m_gender_1 <- subset(data_music, gender == 1)
View(data_m_gender_1)

#명사 추출
nouns_m_gender_1 <- sapply(data_m_gender_1$speak, extractNoun, USE.NAMES=F)
View(nouns_m_gender_1)

# 명사를 벡터로 변환
nouns_m_gender_1 <- unlist(nouns_m_gender_1)

# 단어별 빈도표 생성
wordcount_m_gender_1 <- Filter(function(x){nchar(x) >= 2}, nouns_m_gender_1)
wordcount_m_gender_1 <- table(wordcount_m_gender_1)
head(sort(wordcount_m_gender_1, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_gender_1), 
          freq=wordcount_m_gender_1, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)

## (2) - 2. 여자 -> 이미자, 노사연, 나훈아, 문주란, 임영웅, 클래식, 베스트, 주현미, 현철 

data_m_gender_2 <- subset(data_music, gender == 2)
View(data_m_gender_2)

#명사 추출
nouns_m_gender_2 <- sapply(data_m_gender_2$speak, extractNoun, USE.NAMES=F)
View(nouns_m_gender_2)

# 명사를 벡터로 변환
nouns_m_gender_2 <- unlist(nouns_m_gender_2)

# 단어별 빈도표 생성
wordcount_m_gender_2 <- Filter(function(x){nchar(x) >= 2}, nouns_m_gender_2)
wordcount_m_gender_2 <- table(wordcount_m_gender_2)
head(sort(wordcount_m_gender_2, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_gender_2), 
          freq=wordcount_m_gender_2, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


### (3) 연령대별 

## (3) - 1. 60대 -> 건강, 영탁

data_m_age_60 <- subset(data_music, age == 60)
View(data_m_age_60)

#명사 추출
nouns_m_age_60 <- sapply(data_m_age_60$speak, extractNoun, USE.NAMES=F)
View(nouns_m_age_60)

# 명사를 벡터로 변환
nouns_m_age_60 <- unlist(nouns_m_age_60)

# 단어별 빈도표 생성
wordcount_m_age_60 <- Filter(function(x){nchar(x) >= 2}, nouns_m_age_60)
wordcount_m_age_60 <- table(wordcount_m_age_60)
head(sort(wordcount_m_age_60, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_age_60), 
          freq=wordcount_m_age_60, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (3) - 2. 70대 -> 나훈아, 이미자, 사랑, 클래식, 노사연, 멜로디 , 베스트, 김영임, 아리랑, 임영웅 

data_m_age_70 <- subset(data_music, age == 70)
View(data_m_age_70)

#명사 추출
nouns_m_age_70 <- sapply(data_m_age_70$speak, extractNoun, USE.NAMES=F)
View(nouns_m_age_70)

# 명사를 벡터로 변환
nouns_m_age_70 <- unlist(nouns_m_age_70)

# 단어별 빈도표 생성
wordcount_m_age_70 <- Filter(function(x){nchar(x) >= 2}, nouns_m_age_70)
wordcount_m_age_70 <- table(wordcount_m_age_70)
head(sort(wordcount_m_age_70, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_age_70), 
          freq=wordcount_m_age_70, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (3) - 3. 80대 -> 이미자, 손가인, 옛날, 주현미, 노사연, 문주란, 영림이, 김수희 ,나훈아, 남상, 인터넷, 할머니 

data_m_age_80 <- subset(data_music, age == 80)
View(data_m_age_80)

#명사 추출
nouns_m_age_80 <- sapply(data_m_age_80$speak, extractNoun, USE.NAMES=F)
View(nouns_m_age_80)

# 명사를 벡터로 변환
nouns_m_age_80 <- unlist(nouns_m_age_80)

# 단어별 빈도표 생성
wordcount_m_age_80 <- Filter(function(x){nchar(x) >= 2}, nouns_m_age_80)
wordcount_m_age_80 <- table(wordcount_m_age_80)
head(sort(wordcount_m_age_80, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_age_80), 
          freq=wordcount_m_age_80, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


### (4) 연령대 남/여별

## (4) - 1. 60대 남자 -> 건강, 영탁 

data_m_1_60 <- data_music %>% filter(gender==1) %>% subset(age==60)

#명사 추출
nouns_m_1_60 <- sapply(data_m_1_60$speak, extractNoun, USE.NAMES=F)
View(nouns_m_1_60)

# 명사를 벡터로 변환
nouns_m_1_60 <- unlist(nouns_m_1_60)

# 단어별 빈도표 생성
wordcount_m_1_60 <- Filter(function(x){nchar(x) >= 2}, nouns_m_1_60)
wordcount_m_1_60 <- table(wordcount_m_1_60)
head(sort(wordcount_m_1_60, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_1_60), 
          freq=wordcount_m_1_60, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)

## (4) - 2. 60대 여자 -> 소리, 예수가, 찬송가 (1번씩나옴) 

data_m_2_60 <- data_music %>% filter(gender==2) %>% subset(age==60)

#명사 추출
nouns_m_2_60 <- sapply(data_m_2_60$speak, extractNoun, USE.NAMES=F)
View(nouns_m_2_60)

# 명사를 벡터로 변환
nouns_m_2_60 <- unlist(nouns_m_2_60)

# 단어별 빈도표 생성
wordcount_m_2_60 <- Filter(function(x){nchar(x) >= 2}, nouns_m_2_60)
wordcount_m_2_60 <- table(wordcount_m_2_60)
head(sort(wordcount_m_2_60, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_2_60), 
          freq=wordcount_m_2_60, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (4) - 3. 70대 남자 -> 사랑, 나훈아, 멜로디, 김영임, 아리랑, 최진희, 김성환, 김연자 

data_m_1_70 <- data_music %>% filter(gender==1) %>% subset(age==70)

#명사 추출
nouns_m_1_70 <- sapply(data_m_1_70$speak, extractNoun, USE.NAMES=F)
View(nouns_m_1_70)

# 명사를 벡터로 변환
nouns_m_1_70 <- unlist(nouns_m_1_70)

# 단어별 빈도표 생성
wordcount_m_1_70 <- Filter(function(x){nchar(x) >= 2}, nouns_m_1_70)
wordcount_m_1_70 <- table(wordcount_m_1_70)
head(sort(wordcount_m_1_70, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_1_70), 
          freq=wordcount_m_1_70, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (4) - 4. 70대 여자 -> 이미자, 나훈아, 클래식, 베스트, 임영웅, 현쳘, 폴킴

data_m_2_70 <- data_music %>% filter(gender==2) %>% subset(age==70)

#명사 추출
nouns_m_2_70 <- sapply(data_m_2_70$speak, extractNoun, USE.NAMES=F)
View(nouns_m_2_70)

# 명사를 벡터로 변환
nouns_m_2_70 <- unlist(nouns_m_2_70)

# 단어별 빈도표 생성
wordcount_m_2_70 <- Filter(function(x){nchar(x) >= 2}, nouns_m_2_70)
wordcount_m_2_70 <- table(wordcount_m_2_70)
head(sort(wordcount_m_2_70, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_2_70), 
          freq=wordcount_m_2_70, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (4) - 5. 80대 남자 -> 손가인, 주현미, 송가인, 이미자, 영림이, 김수희, 인터넷

data_m_1_80 <- data_music %>% filter(gender==1) %>% subset(age==80)

#명사 추출
nouns_m_1_80 <- sapply(data_m_1_80$speak, extractNoun, USE.NAMES=F)
View(nouns_m_1_80)

# 명사를 벡터로 변환
nouns_m_1_80 <- unlist(nouns_m_1_80)

# 단어별 빈도표 생성
wordcount_m_1_80 <- Filter(function(x){nchar(x) >= 2}, nouns_m_1_80)
wordcount_m_1_80 <- table(wordcount_m_1_80)
head(sort(wordcount_m_1_80, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_1_80), 
          freq=wordcount_m_1_80, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)


## (4) - 6. 80대 여자 -> 이미자, 노사연, 문주란, 나훈아

data_m_2_80 <- data_music %>% filter(gender==2) %>% subset(age==80)

#명사 추출
nouns_m_2_80 <- sapply(data_m_2_80$speak, extractNoun, USE.NAMES=F)
View(nouns_m_2_80)

# 명사를 벡터로 변환
nouns_m_2_80 <- unlist(nouns_m_2_80)

# 단어별 빈도표 생성
wordcount_m_2_80 <- Filter(function(x){nchar(x) >= 2}, nouns_m_2_80)
wordcount_m_2_80 <- table(wordcount_m_2_80)
head(sort(wordcount_m_2_80, decreasing=T), 20)

#워드클라우드
wordcloud(names(wordcount_m_2_80), 
          freq=wordcount_m_2_80, 
          scale=c(5,1), 
          rot.per=0.25, 
          min.freq=2, 
          random.order=F,  
          random.color=T,
          family="AppleGothic",
          colors=pal)