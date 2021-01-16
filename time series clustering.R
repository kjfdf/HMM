# proact 데이터 로딩
data_all <- read.csv("ALSFRS_R.csv")
# timoe series cluster를 위한 패키지 설치
install.packages("dtwclust")
library("dtwclust") 
