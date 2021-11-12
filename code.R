install.packages("dplyr", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
library(dplyr)
data <- read.csv("cyber-security-1_question-response.csv")
head(data)
str(data)
data[,c("cloze_response","question_type","submitted_at")] <- list(NULL)
colnames(data)

for(i in colnames(data)){
  print(sum(!duplicated(data[i])))
}

for(i in colnames(data)){
  print(sum(data[i]==""))
}

data[data==""]<-NA
data<-data[complete.cases(data),]
sum(data=="")

# which question is mostly correct
data %>%
  group_by(question_number, correct) %>%
  summarise(a_sum=sum(correct=="true",
                      correct=="false"))
# question 1 is mostly correct

# Total number of Corrected questions and incorrected questions
paste("Total number of corrected question:",p=sum(data$correct=="true"))
paste("Total number of incorrected question:",p=sum(data$correct=="false"))

# Top 10 high scorers
df1<- data.frame(data)
df2<-subset(df1, correct=="true")
z<-df2 %>%
  group_by(learner_id, correct=="true") %>%
  summarise(true_sum=sum(correct=="true",
                      correct=="false"))
z[order(-z$true_sum),]

# Which quiz question is most correctly marked
df1<- data.frame(data)
df2<-subset(df1, correct=="false")
data %>%
  group_by(quiz_question, correct) %>%
  summarise(true_sum=sum(correct=="true",
                      correct=="false"))
