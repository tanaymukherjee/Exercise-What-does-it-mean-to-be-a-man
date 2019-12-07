library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(magrittr)
library(fivethirtyeight)
library(ggvis)
library(ggthemes)
library(tidyverse)
library(gtools)
library(data.table)
library(tidyverse)
library(tibble)

survey_538 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/masculinity-survey.csv")
responses_538 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/raw-responses.csv")

# 1. Reproduce the figures and tables in the article, but do separate analyses for
# Straight and Gay / Bisexual men. Don't do the breakup by age groups: there
# aren't many Gay / Bisexual men in the sample, so breaking up the analysis
# further into age groups leaves us with very few observations in some categories.
# Do you notice any differences? Explain what you see in a few paragraphs,
# written in non-technical terms.

# Do you think that society puts pressure on men in a way that is unhealthy or bad for them?

q1_q005_all_men <- responses_538 %>% select(q0005)  %>%
  count(q0005) %>% mutate(Percent = 100*prop.table(n))%>% 
  mutate(Category = "All adult men")

q1_q005_all_men_by_age<- responses_538 %>% select(q0005, age3)  %>%
  count(q0005, age3) %>% group_by(age3) %>% mutate(Percent = 100*prop.table(n)) %>%
  mutate(Category = "All adult men")

q1_q005_all_men_by_orientation <- responses_538 %>% select(q0005, orientation)  %>%
  count(q0005, orientation) %>% group_by(orientation) %>% mutate(Percent = 100*prop.table(n)) %>%
  mutate(Category = "All adult men") %>% 
  filter(orientation == "Straight" | orientation == "Gay/Bisexual")

q1_final <- bind_rows(q1_q005_all_men,q1_q005_all_men_by_age,q1_q005_all_men_by_orientation)

q1_q005_all_men %>% ggplot(aes(x=Category, y = Percent, fill=factor(q0005, levels = c("No", "No answer", "Yes")))) +
  geom_bar(position = "stack", stat = "identity", width = .3) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("Do you think that society \nputs pressure on men in a way \nthat is unhealthy or bad for them?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values=c("darkslategray3", "grey", "darkorange2")) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) 

q1_q005_all_men_by_age %>% ggplot(aes(x=age3, y = Percent, fill=factor(q0005, levels = c("No", "No answer", "Yes")))) +
  geom_bar(position = "stack", stat = "identity", width = .3) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("Do you think that society \nputs pressure on men in a way \nthat is unhealthy or bad for them?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values=c("darkslategray3", "grey", "darkorange2")) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) 



q1_q005_all_men_by_orientation %>% ggplot(aes(x=orientation, y = Percent, fill=factor(q0005, levels = c("No", "No answer", "Yes")))) +
  geom_bar(position = "stack", stat = "identity", width = .3) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("Do you think that society \nputs pressure on men in a way \nthat is unhealthy or bad for them?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values=c("darkslategray3", "grey", "darkorange2")) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) 

#---------------------------------------------------------------------------------------
# What do you worry about on a daily or near-daily basis?

q1_q008_all_men <- responses_538 %>% select(q0008_0001:q0008_0012)  %>%
  gather(Ques, Response, q0008_0001:q0008_0012) %>%
  count(Ques, Response) %>% group_by(Ques) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected") %>% arrange(desc(Percent))

q1_q008_all_men_by_orientation <- responses_538 %>% select(q0008_0001:q0008_0012, orientation) %>%
  filter(orientation == "Straight" | orientation == "Gay/Bisexual") %>%
  gather(Ques, Response, q0008_0001:q0008_0012) %>%
  count(Ques, Response, orientation) %>% group_by(Ques, orientation) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected") %>% arrange(desc(Percent))


q1_q008_all_men %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("What do you worry about on a daily or near-daily basis?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = paste(sprintf("%0.2f", round(Percent, digits = 2),"%")), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

q1_q008_all_men_by_orientation %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ orientation ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("What do you worry about on a daily or near-daily basis?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# How would you say it's an advantage to be a man at your work right now?

q1_q0010_all_men <- responses_538 %>% select(q0010_0001:q0010_0008)%>%
  gather(Ques, Response, q0010_0001:q0010_0008) %>%
  count(Ques, Response) %>% group_by(Ques) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q1_q0010_all_men_by_orientation <- responses_538 %>% select(q0010_0001:q0010_0008, orientation) %>%
  filter(orientation == "Straight" | orientation == "Gay/Bisexual") %>%
  gather(Ques, Response, q0010_0001:q0010_0008) %>%
  count(Ques, Response, orientation) %>% group_by(Ques, orientation) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q1_q0010_all_men %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How would you say it's an advantage to be a man at your work right now?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = paste(sprintf("%0.2f", round(Percent, digits = 2),"%")), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

q1_q0010_all_men_by_orientation %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ orientation ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How would you say it's an advantage to be a man at your work right now?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# How would you say it's a disadvantage to be a man at your work right now?

q1_q0011_all_men <- responses_538 %>% select(q0011_0001:q0011_0005)%>%
  gather(Ques, Response, q0011_0001:q0011_0005) %>%
  count(Ques, Response) %>% group_by(Ques) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q1_q0011_all_men_by_orientation <- responses_538 %>% select(q0011_0001:q0011_0005, orientation) %>%
  filter(orientation == "Straight" | orientation == "Gay/Bisexual") %>%
  gather(Ques, Response, q0011_0001:q0011_0005) %>%
  count(Ques, Response, orientation) %>% group_by(Ques, orientation) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q1_q0011_all_men %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How would you say it's a disadvantage to be a man at your work right now?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = paste(sprintf("%0.2f", round(Percent, digits = 2),"%")), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

q1_q0011_all_men_by_orientation  %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ orientation ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How would you say it's a disadvantage to be a man at your work right now?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# Dating and relationships
# How often do you try to be the one who pays when on a date?

q1_q0018_all_men_by_age <- responses_538 %>% select(q0018,age3)%>%
  count(q0018,age3) %>% group_by(age3) %>% mutate(Percent = 100*prop.table(n)) %>%
  filter(q0018 != "No answer") %>% arrange(desc(Percent))

q1_q0018_all_men_by_orientation <- responses_538 %>% select(q0018, orientation) %>%
  filter(orientation == "Straight" | orientation == "Gay/Bisexual") %>%
  count(q0018, orientation) %>% group_by(orientation) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(q0018 != "No answer") %>% arrange(desc(Percent))

q1_q0018_all_men_by_age %>% ggplot(aes(x= reorder(q0018, -Percent), y = Percent, fill=q0018)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ age3 ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How often do you try to be the one who pays when on a date?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = paste(sprintf("%0.2f", round(Percent, digits = 2),"%")), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

q1_q0018_all_men_by_orientation  %>% ggplot(aes(x= reorder(q0018, -Percent), y = Percent, fill=q0018)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ orientation ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How often do you try to be the one who pays when on a date?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# Dating and relationships
# When you want to be physically intimate with someone, how do you gauge their interest?

q1_q0020_all_men <- responses_538 %>% select(q0020_0001:q0020_0006)%>%
  gather(Ques, Response, q0020_0001:q0020_0006) %>%
  count(Ques, Response) %>% group_by(Ques) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected") %>% arrange(desc(Percent))

q1_q0020_all_men_by_orientation <- responses_538 %>% select(q0020_0001:q0020_0006, orientation) %>%
  filter(orientation == "Straight" | orientation == "Gay/Bisexual") %>%
  gather(Ques, Response, q0020_0001:q0020_0006) %>%
  count(Ques, Response, orientation) %>% group_by(Ques, orientation) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected") %>% arrange(desc(Percent))

q1_q0020_all_men %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("When you want to be physically intimate with someone, how do you gauge their interest?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = paste(sprintf("%0.2f", round(Percent, digits = 2),"%")), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

q1_q0020_all_men_by_orientation  %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ orientation ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("When you want to be physically intimate with someone, how do you gauge their interest?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# Que 2 - Same exercise as 1 for white vs non-white
# Reapet que for race 2 column

# 2. Reproduce the figures and tables in the article as in part 1, but now break up the
# analysis into White / non White men. Again, don't do the break up by age groups.
# Explain what you see.

# written in non-technical terms.

# Do you think that society puts pressure on men in a way that is unhealthy or bad for them?

q2_q005_all_men_by_race <- responses_538 %>% select(q0005, race2)  %>%
  count(q0005, race2) %>% group_by(race2) %>% mutate(Percent = 100*prop.table(n)) %>%
  mutate(Category = "All adult men")

q2_q005_all_men_by_race %>% ggplot(aes(x=race2, y = Percent, fill=factor(q0005, levels = c("No", "No answer", "Yes")))) +
  geom_bar(position = "stack", stat = "identity", width = .3) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("Do you think that society \nputs pressure on men in a way \nthat is unhealthy or bad for them?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values=c("darkslategray3", "grey", "darkorange2")) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) 

#---------------------------------------------------------------------------------------
# What do you worry about on a daily or near-daily basis?

q2_q008_all_men_by_race <- responses_538 %>% select(q0008_0001:q0008_0012, race2) %>%
  gather(Ques, Response, q0008_0001:q0008_0012) %>%
  count(Ques, Response, race2) %>% group_by(Ques, race2) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected") %>% arrange(desc(Percent))

q2_q008_all_men_by_race %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ race2 ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("What do you worry about on a daily or near-daily basis?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# How would you say it's an advantage to be a man at your work right now?

q2_q0010_all_men_by_race <- responses_538 %>% select(q0010_0001:q0010_0008, race2) %>%
  gather(Ques, Response, q0010_0001:q0010_0008) %>%
  count(Ques, Response, race2) %>% group_by(Ques, race2) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q2_q0010_all_men_by_race %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ race2) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How would you say it's an advantage to be a man at your work right now?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# How would you say it's a disadvantage to be a man at your work right now?

q2_q0011_all_men_by_race <- responses_538 %>% select(q0011_0001:q0011_0005, race2) %>%
  gather(Ques, Response, q0011_0001:q0011_0005) %>%
  count(Ques, Response, race2) %>% group_by(Ques, race2) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q2_q0011_all_men_by_race  %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ race2 ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How would you say it's a disadvantage to be a man at your work right now?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# Dating and relationships
# How often do you try to be the one who pays when on a date?

q2_q0018_all_men_by_race <- responses_538 %>% select(q0018, race2) %>%
  count(q0018, race2) %>% group_by(race2) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(q0018 != "No answer") %>% arrange(desc(Percent))

q2_q0018_all_men_by_race  %>% ggplot(aes(x= reorder(q0018, -Percent), y = Percent, fill=q0018)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ race2 ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How often do you try to be the one who pays when on a date?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# Dating and relationships
# When you want to be physically intimate with someone, how do you gauge their interest?

q2_q0020_all_men_by_race <- responses_538 %>% select(q0020_0001:q0020_0006, race2) %>%
  gather(Ques, Response, q0020_0001:q0020_0006) %>%
  count(Ques, Response, race2) %>% group_by(Ques, race2) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected") %>% arrange(desc(Percent))

q2_q0020_all_men_by_race  %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ race2 ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("When you want to be physically intimate with someone, how do you gauge their interest?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#---------------------------------------------------------------------------------------
# Que 3 
# 3. The survey had questions that weren't used in the article. Explain the results in 2
# questions that weren't analyzed. Compare the answers by some demographic
# information (education, age group, ethnicity, sexuality, etc.).

# a) chcking employment by age and education level

q3_q0009_all_men <- responses_538 %>% select(q0009)  %>%
  count(q0009) %>% mutate(Percent = 100*prop.table(n))%>% 
  mutate(Category = "All adult men") %>% mutate(Key = Category)

q3_q0009_all_men_by_age<- responses_538 %>% select(q0009, age3)  %>%
  count(q0009, age3) %>% group_by(age3) %>% mutate(Percent = 100*prop.table(n)) %>%
  mutate(Category = "All adult men") %>% mutate(Key = age3)

q3_q0009_all_men_by_education <- responses_538 %>% select(q0009, educ4)  %>%
  count(q0009, educ4) %>% group_by(educ4) %>% mutate(Percent = 100*prop.table(n)) %>%
  mutate(Category = "All adult men") %>% mutate(Key = educ4)

q3_q0009_final <- bind_rows(q3_q0009_all_men,q3_q0009_all_men_by_age)

q3_q0009_final %>% ggplot(aes(x=Key, y = Percent, fill=factor(q0009))) +
  geom_bar(position = "dodge", stat = "identity", width = .3) +
  scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("Understanding employment status of men - overall v/s by age")+
  theme(plot.title = element_text(hjust = 0.5))  +
  # scale_fill_manual(values=c("darkslategray3", "grey", "darkorange2")) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%"))

q3_q0009_all_men_by_education %>% ggplot(aes(x= reorder(q0009, -Percent), y = Percent, fill=q0009)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ educ4 ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("Understanding employment status of men - by education qualification")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

#-----
# b) checking response against harrasment by men

q3_q0012_all_men <- responses_538 %>% select(q0012_0001:q0012_0007)%>%
  gather(Ques, Response, q0012_0001:q0012_0007) %>%
  count(Ques, Response) %>% group_by(Ques) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q3_q0012_all_men_by_orientation <- responses_538 %>% select(q0012_0001:q0012_0007, orientation) %>%
  filter(orientation == "Straight" | orientation == "Gay/Bisexual") %>%
  gather(Ques, Response, q0012_0001:q0012_0007) %>%
  count(Ques, Response, orientation) %>% group_by(Ques, orientation) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q3_q0012_all_men_by_race <- responses_538 %>% select(q0012_0001:q0012_0007, race2) %>%
  gather(Ques, Response, q0012_0001:q0012_0007) %>%
  count(Ques, Response, race2) %>% group_by(Ques, race2) %>%
  mutate(Percent = 100*prop.table(n)) %>%
  filter(Response != "Not selected" & Response != "") %>% arrange(desc(Percent))

q3_q0012_all_men %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How did you respond to a sexual harassment?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = paste(sprintf("%0.2f", round(Percent, digits = 2),"%")), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

q3_q0012_all_men_by_orientation  %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ orientation ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How did you respond to a sexual harassment?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)

q3_q0012_all_men_by_race  %>% ggplot(aes(x= reorder(Response, -Percent), y = Percent, fill=Response)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ race2 ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("How did you respond to a sexual harassment?")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)


#-----
# c) Analysing demographics

# remove no children and take the screenshot of all 3
q3_q0024_all_men <- responses_538 %>% select(q0024,q0025_0001:q0025_0003)%>%
  gather(Ques, Response, q0025_0001:q0025_0003) %>%
  count(Ques, Response, q0024) %>% group_by(Ques, q0024) %>%
  mutate(Percent = 100*prop.table(n)) %>% 
  filter(Response != "Not selected" & Response != "No children") %>% arrange(desc(Percent))

q3_q0024_all_men  %>% ggplot(aes(x= reorder(q0024, -Percent),  y = Percent, fill=q0024)) +
  geom_bar(position = "stack", stat = "identity", width = .4) +
  facet_grid(. ~ Response ) +
  coord_flip() + scale_color_fivethirtyeight() + theme_fivethirtyeight() +
  ggtitle("Marital Status and its influence in terms of having a family")+ 
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10),labels = function(x) paste0(x,"%")) +
  geom_text(aes(label = sprintf("%0.2f", round(Percent, digits = 2)), Percent = Percent + 0.05), position = position_dodge(0.9),hjust = 0)
