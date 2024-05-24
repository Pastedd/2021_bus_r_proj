##################################################
# 사용 데이터 출처
# https://www.data.go.kr/data/15047212/fileData.do
##################################################

# 작업할 폴더 경로 설정
setwd("C:/Users/igy17/Documents/final_proj")

# 해당 프로젝트에서 사용할 패키지 설치/로드
# dplyr : 데이터 조작, ggplot2 : 시각화, tm : 텍스트 마이닝, 4: 단어 구름 생성
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tm")
install.packages("wordcloud")
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)

# CSV 데이터셋 불러오기
file_path <- "dataset/2021_bus.csv" 
# 데이터셋이 한글파일이라 오류가 발생 -> euc-kr 인코딩을 통해 변환
data <- read.csv(file_path, fileEncoding = "euc-kr")

# 데이터 구조 확인
str(data)

# 결측치 확인 및 처리
sum(is.na(data))
data <- na.omit(data)

# 필요한 열 추출 및 데이터 타입 변환 (분석하기 위해서 거래건수가 필요함)
data$거래건수 <- as.numeric(data$거래건수)

# 해당 프로젝트에서는 어린이와 어른간의 차이를 중점적으로
# 분석할 것이므로 어린이와 어른의 데이터를 기준으로 각각 분리
children_data <- data %>% filter(권종구분 == "어린이")
adult_data <- data %>% filter(권종구분 == "일반")

################################ 자료 분석/시각화 단계

# 1. 승차 및 환승 통계 분석
children_boarding <- children_data %>% filter(승하차구분 == "승차")
children_transfer <- children_data %>% filter(승하차구분 == "환승")
adult_boarding <- adult_data %>% filter(승하차구분 == "승차")
adult_transfer <- adult_data %>% filter(승하차구분 == "환승")

children_boarding_sum <- sum(children_boarding$거래건수)
children_transfer_sum <- sum(children_transfer$거래건수)
adult_boarding_sum <- sum(adult_boarding$거래건수)
adult_transfer_sum <- sum(adult_transfer$거래건수)

boarding_transfer_summary <- data.frame(
  분류 = c("어린이 승차", "어린이 환승", "어른 승차", "어른 환승"),
  거래건수 = c(children_boarding_sum, children_transfer_sum, adult_boarding_sum, adult_transfer_sum)
)

# 원형 그래프 생성
ggplot(boarding_transfer_summary, aes(x = "", y = 거래건수, fill = 분류)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "어린이와 어른의 승차 및 환승 거래 건수",
       x = "",
       y = "") +
  theme_void()

print(boarding_transfer_summary)

# 2. 승하차 거래 건수 비교 분석
boarding_data <- data %>% filter(승하차구분 == "승차")
transferring_data <- data %>% filter(승하차구분 == "환승")

boarding_sum <- sum(boarding_data$거래건수)
transferring_sum <- sum(transferring_data$거래건수)

boarding_transfer_compare <- data.frame(
  분류 = c("승차", "환승"),
  거래건수 = c(boarding_sum, transferring_sum)
)

boarding_transfer_compare$퍼센트 <- round(boarding_transfer_compare$거래건수 / sum(boarding_transfer_compare$거래건수) * 100, 1)

# 원형 그래프 생성 (시각화)
ggplot(boarding_transfer_compare, aes(x = "", y = 거래건수, fill = 분류)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(퍼센트, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "승차 및 환승 거래 건수 비교",
       x = "",
       y = "") +
  theme_void()

print(boarding_transfer_compare)

# 3. 어린이와 어른이 이용하는 총 버스 승차 및 환승 거래 건수 분석
children_total <- sum(children_data$거래건수)
adult_total <- sum(adult_data$거래건수)

total_summary <- data.frame(
  분류 = c("어린이 총 거래건수", "어른 총 거래건수"),
  거래건수 = c(children_total, adult_total)
)

total_summary$퍼센트 <- round(total_summary$거래건수 / sum(total_summary$거래건수) * 100, 1)

# 원형 그래프 생성
ggplot(total_summary, aes(x = "", y = 거래건수, fill = 분류)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(퍼센트, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "어린이와 어른의 총 거래 건수",
       x = "",
       y = "") +
  theme_void()

print(total_summary)

# 4. 어린이가 자주 이용하는 목적지를 분석
top_10_children_stations <- children_data %>%
  group_by(정류장명) %>%
  summarise(거래건수 = sum(거래건수, na.rm = TRUE)) %>%
  arrange(desc(거래건수)) %>%
  head(10)

# 그래프 생성
ggplot(top_10_children_stations, aes(x = reorder(정류장명, -거래건수), y = 거래건수)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "어린이가 자주 이용하는 목적지 (상위 10개 정류장)",
       x = "정류장명",
       y = "거래건수") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_y_continuous(labels = scales::comma)

print(top_10_children_stations)

# 5. 어른이 자주 이용하는 목적지를 분석
top_10_adult_stations <- adult_data %>%
  group_by(정류장명) %>%
  summarise(거래건수 = sum(거래건수, na.rm = TRUE)) %>%
  arrange(desc(거래건수)) %>%
  head(10)

# 그래프 생성
ggplot(top_10_adult_stations, aes(x = reorder(정류장명, -거래건수), y = 거래건수)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "어른이 자주 이용하는 목적지 (상위 10개 정류장)",
       x = "정류장명",
       y = "거래건수") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_y_continuous(labels = scales::comma)

# 한국어 불용어 목록 정의 (해당 불용어를 정의하지 않으면 글자가 깨져 오류가 발생함)
korean_stopwords <- c("및", "이", "그", "저", "것", "수", "등", "들", "때", "만", "한", "가", "와", "의", "로", "을", "를", "은", "는", "이", "가", "에", "하", "고", "도", "다", "서", "지만", "하여")

print(top_10_adult_stations)

# 6. 어린이가 제일 많이 이용하는 정류장 10개 텍스트 마이닝
children_top_stations <- top_10_children_stations$정류장명
children_top_frequencies <- top_10_children_stations$거래건수

# 긴 문자열 자르기 (자르지 않으면 지저분해짐)
shorten_strings <- function(x) {
  ifelse(nchar(x) > 10, paste0(substr(x, 1, 10), "..."), x)
}

children_top_stations <- shorten_strings(children_top_stations)

corpus <- Corpus(VectorSource(children_top_stations))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, korean_stopwords)

# 데이터 프레임 생성
children_word_freq <- data.frame(word = children_top_stations, freq = children_top_frequencies)

# 단어 구름 생성
wordcloud(words = children_word_freq$word, freq = children_word_freq$freq, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"), scale = c(3, 0.5))

print(children_top_stations)

# 7. 어른이 제일 많이 이용하는 정류장 10개 텍스트 마이닝
adult_top_stations <- top_10_adult_stations$정류장명
adult_top_frequencies <- top_10_adult_stations$거래건수

# 긴 문자열 자르기 (자르지 않으면 지저분해짐)
adult_top_stations <- shorten_strings(adult_top_stations)

corpus <- Corpus(VectorSource(adult_top_stations))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, korean_stopwords)

# 데이터 프레임 생성
adult_word_freq <- data.frame(word = adult_top_stations, freq = adult_top_frequencies)

# 단어 구름 생성
wordcloud(words = adult_word_freq$word, freq = adult_word_freq$freq, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"), scale = c(3, 0.5))

print(adult_top_stations)

# 8. 제일 많이 이용하는 목적지들을 상위 오름차순으로 20개 분석하여 점도표 시각화
# 정류장별 거래 건수 집계
children_grouped <- children_data %>%
  group_by(정류장명) %>%
  summarise(어린이_거래건수 = sum(거래건수, na.rm = TRUE))

adult_grouped <- adult_data %>%
  group_by(정류장명) %>%
  summarise(어른_거래건수 = sum(거래건수, na.rm = TRUE))

merged_data <- merge(children_grouped, adult_grouped, by = "정류장명", all = TRUE)
merged_data[is.na(merged_data)] <- 0

# 상위 20개 정류장 어린이와 어래건수 합하여 추출
top_20_stations <- merged_data %>%
  arrange(desc(어린이_거래건수 + 어른_거래건수)) %>%
  head(20)

ggplot(top_20_stations, aes(x = reorder(정류장명, -어린이_거래건수 - 어른_거래건수), y = 어린이_거래건수 + 어른_거래건수)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "상위 20개 정류장 이용 건수",
       x = "정류장명",
       y = "거래건수") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  scale_y_continuous(labels = scales::comma)

print(top_20_stations)

# 9. 광주종합버스터미널 이용 비율이 전체 정류장에서 몇%인지 확인
# + 상위 10개 정류장들과 이용 비율 비교
terminal_data <- data %>% filter(정류장명 == "광주종합버스터미널")
terminal_total <- sum(terminal_data$거래건수)
total_transactions <- sum(data$거래건수)
terminal_ratio <- (terminal_total / total_transactions) * 100

# 광주종합버스터미널 이용 비율 계산
terminal_data <- data %>% filter(정류장명 == "광주종합버스터미널")
terminal_total <- sum(terminal_data$거래건수)
terminal_ratio <- (terminal_total / total_transactions) * 100

# 상위 10개 정류장의 거래 건수 계산
top_10_stations <- data %>%
  group_by(정류장명) %>%
  summarise(거래건수 = sum(거래건수, na.rm = TRUE)) %>%
  arrange(desc(거래건수)) %>%
  head(10)

# 전체 거래 건수 대비 비율 계산
top_10_stations <- top_10_stations %>%
  mutate(비율 = (거래건수 / total_transactions) * 100)

# 광주종합버스터미널 추가
top_10_stations <- rbind(top_10_stations, data.frame(정류장명 = "광주종합버스터미널", 거래건수 = terminal_total, 비율 = terminal_ratio))

# 시각화
ggplot(top_10_stations, aes(x = reorder(정류장명, -비율), y = 비율, fill = 정류장명 == "광주종합버스터미널")) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(비율, 2), "%")), vjust = -0.5) +
  labs(title = "광주종합버스터미널 이용 비율과 주요 정류장 비교",
       x = "정류장명",
       y = "이용 비율 (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("gray", "blue"))

# 결과 출력
print(paste("광주종합버스터미널 이용 비율: ", round(terminal_ratio, 2), "%"))

# 10. 전남대학교 관련 정류소의 거래 건수 계산 (정류장 이름에 전남대가 들어간 경우만 포함하므로 전남대학교 이용 학생의 대중교통 이용율을 표현하기엔 신뢰성이 낮음)
jnu_stations <- data %>% filter(정류장명 %in% c("전남대용봉탑", "전남대", "전남대스포츠센터", "전남대동문", "전남대공과대학", "전남대사거리(동)", "전남대사거리(서)"))
jnu_summary <- jnu_stations %>%
  group_by(정류장명) %>%
  summarise(거래건수 = sum(거래건수, na.rm = TRUE))

# 전체 상위 10개 정류소 거래 건수 계산
top_10_stations <- data %>%
  group_by(정류장명) %>%
  summarise(거래건수 = sum(거래건수, na.rm = TRUE)) %>%
  arrange(desc(거래건수)) %>%
  head(10)

# 전남대학교 관련 정류소를 상위 10개 정류소와 병합
combined_data <- rbind(top_10_stations, jnu_summary)
combined_data <- combined_data %>% distinct() %>% arrange(desc(거래건수))

# 상위 10개 정류소 이용률과 같이 시각화
ggplot(combined_data, aes(x = reorder(정류장명, -거래건수), y = 거래건수, fill = 정류장명 %in% c("전남대용봉탑", "전남대", "전남대스포츠센터", "전남대동문", "전남대공과대학", "전남대사거리(동)", "전남대사거리(서)"))) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = 거래건수), vjust = -0.3, size = 3.5) +
  labs(title = "전남대학교 관련 정류소와 상위 10개 정류소 거래건수 비교",
       x = "정류장명",
       y = "거래건수") +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("gray", "skyblue"), labels = c("기타 정류소", "전남대학교 관련 정류소")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))

print(jnu_summary)
