df_full<-readRDS(file = "data/full_dataframe.R")
df_lr<-readRDS(file = "data/lr_dataframe.R")

top_names<-df_full%>%
  filter(rank<100)%>%
  select(name,sex)%>%
  distinct()%>%
  pull(name)


# Centurians
centNames<-df_lr%>%
  group_by(name,sex)%>%
  summarise(n=n())%>%
  ungroup()%>%
  filter(n==max(n))%>%
  pull(name)

#rising stars
b_risStarts<-df_full%>%
  filter(sex=="boy")%>%
  arrange(year)%>%
  group_by(name)%>%
  filter(mean(count[year%in%c(1996:2000)],na.rm=TRUE)>70)%>%
  filter(rank[year==2019]<=100)%>%
  mutate(rollrank=rollmean(rank,3,na.pad = TRUE,align = c("right")))%>%
  mutate(change=rollrank-lag(rollrank,15))%>%
  filter(year==2019)%>%
  arrange(change)%>%
  head(8)%>%pull(name)

g_risStarts<-df_full%>%
  filter(sex=="girl")%>%
  arrange(year)%>%
  group_by(name)%>%
  filter(mean(count[year%in%c(1996:2000)],na.rm=TRUE)>70)%>%
  filter(rank[year==2019]<=100)%>%
  mutate(rollrank=rollmean(rank,3,na.pad = TRUE,align = c("right")))%>%
  mutate(change=rollrank-lag(rollrank,15))%>%
  filter(year==2019)%>%
  arrange(change)%>%
  head(8)%>%pull(name)


df_small<-df_full%>%
  filter(rank<500)%>%
  group_by(name,sex)%>%
  filter(n()>20)

