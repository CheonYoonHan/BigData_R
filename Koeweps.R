install.packages("foreign")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)

welfare <- raw_welfare

welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
class(welfare$sex)

table(welfare$sex)

table(welfare$sex)

qplot(welfare$income)

qplot(welfare$income) + xlim(0, 1000)

summary(welfare$income)

welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA , welfare$income)

table(is.na(welfare$income))

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income

class(welfare$code_job)

table(welfare$code_job)

list_job <- read_excel("Koweps_Codebook.xlsx", col_names =T, sheet = 2)
head(list_job)
dim(list_job)

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

ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)

ggplot(data = bottom10, aes(x = reorder(job, -mean_income),
                            y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)

job_male <- welfare %>%
  filter(!is.na(job) & sex == "male") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% head(10)

job_male

job_female <- welfare %>%
  filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  head(10)
job_female

ggplot(data = job_male, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

ggplot(data = job_female, aes(x = reorder(job, n), y = n)) +
  geom_col() +
  coord_flip()

class(welfare$religion)

table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)

qplot(welfare$religion)

class(welfare$marriage)

table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA ))

table(welfare$group_marriage)

table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

religion_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))

religion_marriage

divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct)

divorce

ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))
