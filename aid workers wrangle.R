require(tidyverse)

aid2017 <- read.csv("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/Insecurity Insight/AID workers 2017.csv")
arrest17 <- read.csv("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/Insecurity Insight/arrested_2017.csv")
aid2018 <- read.csv("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/Insecurity Insight/AID workers 2018.csv")
healthedu_personnel_data <- read.csv("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/Insecurity Insight/healthedu_personnel_data.csv")

kidnapped2017 <- aid2017 %>% select(Country, Total.Kidnapped.2017..as.of.February.2018.) %>% 
  rename(Frequency = Total.Kidnapped.2017..as.of.February.2018.) %>% 
  rename(country = Country) %>%
  mutate(Sector = "Aid Workers") %>% 
  mutate(Incident = "kidnapped") %>% 
  mutate(Date = "2017")


arrest17 <- arrest17 %>%  select(Country, X2017..as.of.February.2018.) %>% 
  rename(country = Country) %>% 
  rename(Frequency = X2017..as.of.February.2018.) %>% 
  mutate(Sector = "Aid Workers") %>% 
  mutate(Incident = "arrested") %>% 
  mutate(Date = "2017")


aid2018 <- aid2018 %>% rename(killed = Killed.2018..as.of.June.2018.,
                              Kidnapped = Kidnapped.2018...as.of.June.2018.,
                              Arrested = Arrested.2018...as.of.June.2018.) %>% 
  gather(killed:Arrested, key = "Incident", value = "Frequency") %>% 
  select(-Date) %>%   
  mutate(Date = "2018") %>% 
  mutate(Sector = "Aid Workers") %>% 
  rename(country = Country)


aid_work_2017 <- full_join(arrest17, kidnapped2017)

aid_work_2017$Frequency <- as.numeric(aid_work_2017$Frequency)

aid2018$Frequency <- as.numeric(aid2018$Frequency)

aidwork1718 <- full_join(aid_work_2017, aid2018)

aidwork1718$Frequency <- as.integer(aidwork1718$Frequency)

write.csv(aidwork1718, 'aidwork1718.csv')

healthedu_personnel_data <- healthedu_personnel_data %>% mutate(Date = "2017")

healthedu_personnel_data <- full_join(healthedu_personnel_data, aid_work_2017)


facilities_health <- read.csv("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/Insecurity Insight/shcc-healthcare-dataset-2017-v-may-2018.csv")

facilities_health_iso <- facilities_health %>% 
  select(X,Total.health.facility.destroyed: Health.transportation.Stolen.Highjacked) %>% 
  filter(X != '#country +name')

facilities_health_iso$Total.health.facility.destroyed <- as.numeric(as.character(facilities_health_iso$Total.health.facility.destroyed))
facilities_health_iso$Total.health.facility.damage <- as.numeric(as.character(facilities_health_iso$Total.health.facility.damage))
facilities_health_iso$Health.transportation.destroyed <- as.numeric(as.character(facilities_health_iso$Health.transportation.destroyed))
facilities_health_iso$Health.transportation.damaged <- as.numeric(as.character(facilities_health_iso$Health.transportation.damaged))
facilities_health_iso$Health.transportation.Stolen.Highjacked <- as.numeric(as.character(facilities_health_iso$Health.transportation.Stolen.Highjacked))

group <- sample(2:5)
facilities_health_iso$Total <- rowSums(facilities_health_iso[,"Total.health.facility.destroyed": "Health.transportation.Stolen.Highjacked"], na.rm = TRUE)


facilities_health <- facilities_health_iso %>%
  mutate(sum = select(., Total.health.facility.destroyed: Health.transportation.Stolen.Highjacked) %>% apply(1, sum, na.rm=TRUE)) %>% 
  select(X, sum) %>% 
  rename(country = X,
         Frequency = sum) %>% 
  mutate(Sector = "Health",
         Incident = "Infastructure Damage",
         Date = 2017)

healthedu_personnel_data <- full_join(facilities_health, healthedu_personnel_data)


write.csv(healthedu_personnel_data, "combined_aid_edu_health.csv")
