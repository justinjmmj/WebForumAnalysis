library(ggplot2)
library(tidyverse)
library(dplyr)
library(igraph)
library(igraphdata)
library(reshape2)


getwd()
setwd("~/Uni Mods/Sem 3/FIT 3152 Data Analytics/Assignment1")
rm(list=ls())
set.seed(31972123)
webforum.dat = read.csv("webforum.csv") 
#webforum.dat = webforum.dat[sample(nrow(webforum.dat),2000),]
summary(webforum.dat)

#Creating a new data frame and adding Year/Month and Year Column in Dataframe
webData = webforum.dat 
webData$YearMonth = strftime(webData$Date, "%Y-%m")
webData$Year = strftime(webData$Date, "%Y")

#Removing posts with 0 word count
webData = subset(webData, WC != 0)

#Remove guests users to use for networking
webDataUsers = subset(webData, AuthorID != "-1")

#plotting Occurrences 
monthDate.dat = table(webData["YearMonth"])
yearDate.dat = table(webData["Year"])
plot(monthDate.dat, type = "l")
plot(yearDate.dat, type = "l")

MonthDate = webData %>% 
  select(YearMonth,Date) %>%
  group_by(YearMonth) %>%
  tally() %>%
  ungroup()

YearDate = webData %>% 
  select(Year,Date) %>%
  group_by(Year) %>%
  tally() %>%
  ungroup()


#plotting WordCount over Time
MonthDateWC = webData %>% 
  select(YearMonth,WC) %>%
  group_by(YearMonth) %>%
  summarise(TotalWordCount = sum(WC)) %>%
  ungroup()

YearDateWC = webData %>% 
  select(Year,WC) %>%
  group_by(Year) %>%
  summarise(TotalWordCount = sum(WC)) %>%
  ungroup()


ggplot(data = MonthDateWC, mapping = aes(YearMonth,TotalWordCount))+geom_point()
ggplot(data =YearDateWC, mapping = aes(Year,TotalWordCount))+geom_point()

#plotting use of words over time
#Plot pronouns by Year/Month
groupi = webData %>%
  select(YearMonth,i, we, you, shehe, they) %>%
  group_by(YearMonth) %>%
  summarise(TotalI = sum(i), TotalWe = sum(we), TotalYou = sum(you),
            TotalShehe = sum(shehe), TotalThey = sum(they)) %>%
  ungroup()

groupiData = data.frame(YearMonth = groupi$YearMonth,
                        linguistic = c (groupi$TotalI,groupi$TotalWe,groupi$TotalYou,
                                        groupi$TotalShehe,groupi$TotalThey),
                        group = c(rep("TotalI",nrow(groupi)),
                                  rep("TotalWe",nrow(groupi)),
                                  rep("TotalYou",nrow(groupi)),
                                  rep("TotalShehe",nrow(groupi)),
                                  rep("TotalThey",nrow(groupi))))

ggplot(data = groupiData, mapping = aes(YearMonth,linguistic, color = group)) +
  geom_line(aes(group= group)) + geom_point()
ggplot(data = groupiData, mapping = aes(YearMonth,linguistic, color = group)) +
  geom_line(aes(group= group)) + geom_point() + facet_grid(group ~.) 

#Plot pronouns by Year
groupi = webData %>%
  select(Year,i, we, you, shehe, they) %>%
  group_by(Year) %>%
  summarise(TotalI = sum(i), TotalWe = sum(we), TotalYou = sum(you),
            TotalShehe = sum(shehe), TotalThey = sum(they)) %>%
  ungroup()

groupiData = data.frame(Year = groupi$Year,
                        linguistic = c (groupi$TotalI,groupi$TotalWe,groupi$TotalYou,
                                        groupi$TotalShehe,groupi$TotalThey),
                        group = c(rep("TotalI",nrow(groupi)),
                                  rep("TotalWe",nrow(groupi)),
                                  rep("TotalYou",nrow(groupi)),
                                  rep("TotalShehe",nrow(groupi)),
                                  rep("TotalThey",nrow(groupi))))

ggplot(data = groupiData, mapping = aes(Year,linguistic, color = group)) +
  geom_line(aes(group= group)) + geom_point()

  

#Plot Emotions per month
Emotions = webData %>%
  select(YearMonth,posemo,negemo) %>%
  group_by(YearMonth) %>%
  summarise(TotalPos = sum(posemo), TotalNeg = sum(negemo)) %>%
  ungroup()

emData = data.frame(YearMonth = Emotions$YearMonth,
                        Feelings = c (Emotions$TotalPos,Emotions$TotalNeg),
                        group = c(rep("TotalPos",nrow(Emotions)),
                                  rep("TotalNeg",nrow(Emotions))))
                              

ggplot(data = emData, mapping = aes(YearMonth,Feelings, color = group)) +
  geom_line(aes(group= group)) + geom_point()
ggplot(data = emData, mapping = aes(YearMonth,Feelings, color = group)) +
  geom_line(aes(group= group)) + geom_point() + facet_grid(group ~.) 


#Plot Emotions per Year
Emotions = webData %>%
  select(Year,posemo,negemo) %>%
  group_by(Year) %>%
  summarise(TotalPos = sum(posemo), TotalNeg = sum(negemo)) %>%
  ungroup()

emData = data.frame(Year = Emotions$Year,
                    Feelings = c (Emotions$TotalPos,Emotions$TotalNeg),
                    group = c(rep("TotalPos",nrow(Emotions)),
                              rep("TotalNeg",nrow(Emotions))))


ggplot(data = emData, mapping = aes(Year,Feelings, color = group)) +
  geom_line(aes(group= group)) + geom_point()




#B
#One Plot by each thread in comparison to emotions 
#One Graph one Thread with multiple lines indicating each emotion
#Going through time

#Grab all the thread and filter out all those with 0 value posemo && negemo
#as those thread with 0 on both posemo and negemo are invalid data

#Positive Ranking as of latest reading
Threads = webData %>% 
  select(ThreadID,Year,posemo,negemo) %>%
  group_by(ThreadID, Year) %>%
  filter(posemo != 0.0 & negemo != 0.0) %>%
  filter(Year == 2011) %>%
  summarise(positive = sum(posemo), negative = sum(negemo)) %>%
  mutate(EmotionDiff = positive - negative) %>%
  arrange(desc(EmotionDiff)) %>%
  ungroup()

Threads$Rankings = 1:nrow(Threads)

Threads = Threads %>% 
  head(10)

ThreadData = data.frame(ThreadID = Threads$ThreadID,
                        Rankings = Threads$Rankings,
                        Emotion = c(Threads$positive,Threads$negative),
                        group = c(rep("positive",nrow(Threads)),
                                  rep("negative",nrow(Threads))))


ggplot(data = ThreadData, mapping = aes(Rankings,Emotion, col = group)) + 
  geom_point() +  labs(x = "Ranking", y = "Rating")+
  ggtitle("2011")


#Positive Ranking as of the year before latest reading
Threads = webData %>% 
  select(ThreadID,Year,posemo,negemo) %>%
  group_by(ThreadID, Year) %>%
  filter(posemo != 0.0 & negemo != 0.0) %>%
  filter(Year == 2010) %>%
  summarise(positive = sum(posemo), negative = sum(negemo)) %>%
  mutate(EmotionDiff = positive - negative) %>%
  arrange(desc(EmotionDiff)) %>%
  ungroup()

Threads$Rankings = 1:nrow(Threads)

Threads = Threads %>% 
  head(10)

ThreadData = data.frame(ThreadID = Threads$ThreadID,
                        Rankings = Threads$Rankings,
                        Emotion = c(Threads$positive,Threads$negative),
                        group = c(rep("positive",nrow(Threads)),
                                  rep("negative",nrow(Threads))))


ggplot(data = ThreadData, mapping = aes(Rankings,Emotion, col = group)) + 
  geom_point()+  labs(x = "Ranking", y = "Rating")+
  ggtitle("2010")



#Positive Ranking as of Peak period
Threads = webData %>% 
  select(ThreadID,Year,posemo,negemo) %>%
  group_by(ThreadID, Year) %>%
  filter(posemo != 0.0 & negemo != 0.0) %>%
  filter(Year == 2006) %>%
  summarise(positive = sum(posemo), negative = sum(negemo)) %>%
  mutate(EmotionDiff = positive - negative) %>%
  arrange(desc(EmotionDiff)) %>%
  ungroup()

Threads$Rankings = 1:nrow(Threads)

Threads = Threads %>% 
  head(10)

ThreadData = data.frame(ThreadID = Threads$ThreadID,
                        Rankings = Threads$Rankings,
                        Emotion = c(Threads$positive,Threads$negative),
                        group = c(rep("positive",nrow(Threads)),
                                  rep("negative",nrow(Threads))))


ggplot(data = ThreadData, mapping = aes(Rankings,Emotion, col = group)) + 
  geom_point() +  labs(x = "Ranking", y = "Rating")+
  ggtitle("2006")

#Negative Ranking as of latest reading
Threads = webData %>% 
  select(ThreadID,Year,posemo,negemo) %>%
  group_by(ThreadID, Year) %>%
  filter(posemo != 0.0 & negemo != 0.0) %>%
  filter(Year == 2011) %>%
  summarise(positive = sum(posemo), negative = sum(negemo)) %>%
  mutate(EmotionDiff = positive - negative) %>%
  arrange(EmotionDiff) %>%
  ungroup()

Threads$Rankings = 1:nrow(Threads)

Threads = Threads %>% 
  head(10)

ThreadData = data.frame(ThreadID = Threads$ThreadID,
                        Rankings = Threads$Rankings,
                        Emotion = c(Threads$positive,Threads$negative),
                        group = c(rep("positive",nrow(Threads)),
                                  rep("negative",nrow(Threads))))


ggplot(data = ThreadData, mapping = aes(Rankings,Emotion, col = group)) + 
  geom_point() +  labs(x = "Ranking", y = "Rating")+
  ggtitle("2011")


#Negative Ranking as of the year before latest reading
Threads = webData %>% 
  select(ThreadID,Year,posemo,negemo) %>%
  group_by(ThreadID, Year) %>%
  filter(posemo != 0.0 & negemo != 0.0) %>%
  filter(Year == 2010) %>%
  summarise(positive = sum(posemo), negative = sum(negemo)) %>%
  mutate(EmotionDiff = positive - negative) %>%
  arrange(EmotionDiff) %>%
  ungroup()

Threads$Rankings = 1:nrow(Threads)

Threads = Threads %>% 
  head(10)

ThreadData = data.frame(ThreadID = Threads$ThreadID,
                        Rankings = Threads$Rankings,
                        Emotion = c(Threads$positive,Threads$negative),
                        group = c(rep("positive",nrow(Threads)),
                                  rep("negative",nrow(Threads))))


ggplot(data = ThreadData, mapping = aes(Rankings,Emotion, col = group)) + 
  geom_point()+  labs(x = "Ranking", y = "Rating")+
  ggtitle("2010")



#Negative Ranking as of Peak period
Threads = webData %>% 
  select(ThreadID,Year,posemo,negemo) %>%
  group_by(ThreadID, Year) %>%
  filter(posemo != 0.0 & negemo != 0.0) %>%
  filter(Year == 2006) %>%
  summarise(positive = sum(posemo), negative = sum(negemo)) %>%
  mutate(EmotionDiff = positive - negative) %>%
  arrange(EmotionDiff) %>%
  ungroup()

Threads$Rankings = 1:nrow(Threads)

Threads = Threads %>% 
  head(10)

ThreadData = data.frame(ThreadID = Threads$ThreadID,
                        Rankings = Threads$Rankings,
                        Emotion = c(Threads$positive,Threads$negative),
                        group = c(rep("positive",nrow(Threads)),
                                  rep("negative",nrow(Threads))))


ggplot(data = ThreadData, mapping = aes(Rankings,Emotion, col = group)) +
  geom_point()+  labs(x = "Ranking", y = "Rating")+
  ggtitle("2006")


#C
#Choose Top 30 Authors who wrote in the most threads

#Tallies the number of Threads Authors Wrote in
#Grabs the top30
network30 = webDataUsers %>%
  select(AuthorID, Year, ThreadID,Analytic,Clout,Authentic) %>%
  group_by(AuthorID,Year,ThreadID) %>%
  group_by(AuthorID,Year) %>%
  summarise(NoThreads = n()) %>%
  filter(Year == 2011) %>%
  arrange(desc(NoThreads)) %>%
  head(30) %>%
  ungroup()

#Grabs all the expressions of each post of the Top 30
network30ALL= webDataUsers %>%
  select(AuthorID, Year,WC, ThreadID,Analytic,Clout,Authentic,focuspast,focuspresent,focusfuture) %>%
  filter(Year == 2011,AuthorID %in% network30$AuthorID)

#Grabs all the threads the top 30 wrote in
ThreadNetwork = webDataUsers %>%
  select(AuthorID,Year,ThreadID) %>%
  filter(Year == 2011, AuthorID %in% network30$AuthorID)

topAuthors = network30 %>%
  select(AuthorID)
topAuthors$AuthorID = as.character(topAuthors$AuthorID)

AuthorThreads = network30ALL %>%
  select(AuthorID,ThreadID)
AuthorThreads$AuthorID = as.character(AuthorThreads$AuthorID)
AuthorThreads$ThreadID = as.character(AuthorThreads$ThreadID)


networkGraph <- make_empty_graph(directed = FALSE)
for(i in 1 :nrow(topAuthors)){
  networkGraph <- add_vertices(networkGraph,1, name =
  as.character(topAuthors$AuthorID[i]))
}

 # loop through each group
for (k in unique(AuthorThreads$ThreadID)){
  temp = AuthorThreads[(AuthorThreads$ThreadID == k),]
  # combine each pair of agents to make an edge list
  #Check whether there is a Thread with only 1 row and skip it
  if(nrow(temp)>1){
    Edgelist = as.data.frame(t(combn(temp$AuthorID,2)))
    colnames(Edgelist) = c("P1", "P2")
    # loop through pairs of edges and add
    for (i in 1 : nrow(Edgelist)) {
      networkGraph <- add_edges(networkGraph, c(as.character(Edgelist$P1[i]),as.character(Edgelist$P2[i])))
    }
  }
}

#Plot Network Graph
networkGraph = simplify(networkGraph)
plot(networkGraph)


#Finding the most important Author in the Network
degree = as.table(degree(networkGraph))
closeness = as.table(closeness(networkGraph))
betweenness = as.table(betweenness(networkGraph))
networkTab = as.data.frame((cbind(degree,closeness,betweenness)))

networkTab = networkTab %>%
  arrange(desc(degree))
  

#Differentiate Language Use
#Grab First 3 
Top3Auth = subset(network30ALL, AuthorID == "54960")
Top3Auth$Group <- "1"


#Summarize Language
Top3 = Top3Auth %>%
  select(everything()) %>%
  group_by(AuthorID,Group) %>%
  summarise(analytic = mean(Analytic),clout = mean(Clout), authentic = mean(Authentic),focusPast = mean(focuspast), focusPresent = mean(focuspresent), focusFuture = mean(focusfuture), wc = mean(WC)) %>%
  ungroup()

#Grab Mid 3
Mid3Auth = subset(network30ALL, AuthorID == "231245") 
Mid3Auth$Group <- "2"

#Summarize Language
Mid3 = Mid3Auth %>%
  select(everything()) %>%
  group_by(AuthorID,Group) %>%
  summarise(analytic = mean(Analytic),clout = mean(Clout), authentic = mean(Authentic),focusPast = mean(focuspast), focusPresent = mean(focuspresent), focusFuture = mean(focusfuture), wc = mean(WC)) %>%
  ungroup()

#Grab Last 3
Last3Auth = subset(network30ALL, AuthorID == "166362")
Last3Auth$Group <- "3"

#Summarize Language
Last3 = Last3Auth %>%
  select(everything()) %>%
  group_by(AuthorID,Group) %>%
  summarise(analytic = mean(Analytic),clout = mean(Clout), authentic = mean(Authentic),focusPast = mean(focuspast), focusPresent = mean(focuspresent), focusFuture = mean(focusfuture), wc = mean(WC)) %>%
  ungroup()

#Join the main groups to determine whether to use mean or median as the variables to compare.
Overall = rbind(Top3Auth,Mid3Auth,Last3Auth)
Overall$AuthorID = as.character(Overall$AuthorID)
ggplot(data = Overall,mapping = aes(x = Group,y = Analytic,group = Group))+geom_boxplot()
ggplot(data = Overall,mapping = aes(x = Group,y = Clout,group = Group))+geom_boxplot()
ggplot(data = Overall,mapping = aes(x = Group,y = Authentic,group = Group))+geom_boxplot()

ggplot(data = Overall,mapping = aes(x = Group,y = focuspast,group = Group))+geom_boxplot()
ggplot(data = Overall,mapping = aes(x = Group,y = focuspresent,group = Group))+geom_boxplot()
ggplot(data = Overall,mapping = aes(x = Group,y = focusfuture,group = Group))+geom_boxplot()

ggplot(data = Overall,mapping = aes(x = Group,y = WC,group = Group))+geom_boxplot()

#Join the groups and plot Means of each linguistic variable
OverallMed = rbind(Top3,Mid3,Last3)

Overalldata = data.frame(AuthorID = OverallMed$AuthorID,
                         Placings = as.character(OverallMed$Group),
                         Language = c(OverallMed$analytic,OverallMed$clout,OverallMed$authentic),
                         group = c(rep("Analytic",nrow(OverallMed)),
                                   rep("Clout",nrow(OverallMed)),
                                   rep("Authentic",nrow(OverallMed))))

ggplot(data = Overalldata, mapping = aes(group,Language)) +
  geom_point(aes(color= Placings)) +
  theme_minimal()+
  labs(x = "Language", y = "Rating")

Overalldata = data.frame(AuthorID = OverallMed$AuthorID,
                         Placings = as.character(OverallMed$Group),
                         Language = c(OverallMed$focusPast,OverallMed$focusPresent,OverallMed$focusFuture),
                         group = c(rep("Focus Past",nrow(OverallMed)),
                                   rep("Focus Present",nrow(OverallMed)),
                                   rep("Focus Future",nrow(OverallMed))))

ggplot(data = Overalldata, mapping = aes(group,Language)) +
  geom_point(aes(color= Placings)) +
  theme_minimal()+
  labs(x = "Language", y = "Rating")

Overalldata = data.frame(AuthorID = OverallMed$AuthorID,
                         Placings = as.character(OverallMed$Group),
                         WC = OverallMed$wc)

ggplot(data = Overalldata, mapping = aes(Placings,WC)) +
  geom_point(aes(color= Placings)) +
  theme_minimal()+
  labs(x = "Ranking", y = "Average Word Count")


