#Package installation
install.packages("dplyr", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
library(dplyr)

# Data loading
data <- read.csv("cyber-security-1_question-response.csv")
head(data,10)
tail(data,10)
dim(data)
str(data)

# Dropping unnecessary columns
data[,c("cloze_response","question_type","submitted_at")] <- list(NULL)
colnames(data)

# How many unique elements are present in each column
for(i in colnames(data)){
  print(sum(!duplicated(data[i])))
}

# How many empty cells are present in each column
for(i in colnames(data)){
  print(sum(data[i]==""))
}

# Dropping empty cells
data[data==""]<-NA
data<-data[complete.cases(data),]
sum(data=="")


# which question is mostly correct in between all quizzes
data %>%
  group_by(question_number, correct) %>%
  summarise(a_sum=sum(correct=="true",
                      correct=="false"))
# question 1 is mostly correct



# Total number of Corrected questions and incorrected questions
paste("Total number of corrected question:",p=sum(data$correct=="true"))
paste("Total number of incorrected question:",p=sum(data$correct=="false"))
H <- c(sum(data$correct=="true"),sum(data$correct=="false"))
M <- c("corrected","incorrected")
# Plot the bar chart 
barplot(H,names.arg=M,xlab="",ylab="",col="brown",
        main="Total number of Corrected questions and incorrected questions",border="red")



# Top 10 high scorers (learning_id)
df1<- data.frame(data)
df2<-subset(df1, correct=="true")
z<-df2 %>%
  group_by(learner_id, correct=="true") %>%
  summarise(true_sum=sum(correct=="true",
                      correct=="false"))
z[order(-z$true_sum),]




# Which quiz question is most correctly marked
df1<- data.frame(data)
df2<-subset(df1, correct=="true")
z<-df2 %>%
  group_by(quiz_question, correct) %>%
  summarise(true_sum=sum(correct=="true",
                      correct=="false"))
z[order(-z$true_sum),]
# Plot the bar chart 
barplot(z$true_sum,names.arg=z$quiz_question,las=2,xlab="",ylab="",col="green",
        main="Quiz question correctly marked",border="black")




# Which quiz question is most incorrectly marked
df1<- data.frame(data)
df2<-subset(df1, correct=="false")
z<-df2 %>%
  group_by(quiz_question, correct) %>%
  summarise(true_sum=sum(correct=="true",
                         correct=="false"))
z[order(-z$true_sum),]
# Plot the bar chart 
barplot(z$true_sum,names.arg=z$quiz_question,las=2,xlab="",ylab="",col="red",
        main="Quiz question incorrectly marked",border="black")




# which step number got the most correctly marked
df1<- data.frame(data)
df2<-subset(df1, correct=="true")
z<-df2 %>%
  group_by(step_number, correct) %>%
  summarise(true_sum=sum(correct=="true",
                         correct=="false"))
z[order(-z$true_sum),]
# Plot the bar chart 
barplot(z$true_sum,names.arg=z$step_number,xlab="",ylab="",col="green",
        main="Total step numbers which are correctly marked",border="black")




# which step number got the most incorrectly marked
df1<- data.frame(data)
df2<-subset(df1, correct=="false")
z<-df2 %>%
  group_by(step_number, correct) %>%
  summarise(true_sum=sum(correct=="true",
                         correct=="false"))
z[order(-z$true_sum),]
# Plot the bar chart 
barplot(z$true_sum,names.arg=z$step_number,xlab="",ylab="",col="red",
        main="Total step numbers which are incorrectly marked",border="black")

