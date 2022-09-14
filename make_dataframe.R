raw_b_rank <- read_excel("data/babynames1996to2020.xlsx",
                         sheet = "boys_rank",na = ":",skip=1)%>%
                        gather(key=year,value=rank,-name)%>%
                        mutate(sex="boy")

raw_b_count <- read_excel("data/babynames1996to2020.xlsx",
                          sheet = "boys_count",na = ":",skip=1)%>%
                          gather(key=year,value=count,-name)%>%
                          mutate(sex="boy")

raw_g_rank <- read_excel("data/babynames1996to2020.xlsx",
                         sheet = "girls_rank",na = ":",skip=1)%>%
                          gather(key=year,value=rank,-name)%>%
                          mutate(sex="girl")


raw_g_count <- read_excel("data/babynames1996to2020.xlsx",
                          sheet = "girls_count",na = ":",skip=1)%>%
                          gather(key=year,value=count,-name)%>%
                          mutate(sex="girl")


df_boy<-full_join(raw_b_count,raw_b_rank,by=c("name","year","sex"))%>%
  select(name,year,sex,count,rank)%>%
  mutate(count=as.numeric(count),rank=as.numeric(rank),year=as.numeric(year))


df_girl<-full_join(raw_g_count,raw_g_rank,by=c("name","year","sex"))%>%
  select(name,year,sex,count,rank)%>%
  mutate(count=as.numeric(count),rank=as.numeric(rank),year=as.numeric(year))

df_full<-bind_rows(df_girl,df_boy)%>%
  mutate(name=str_replace_all(name,":","-"))%>%
  mutate(name=str_to_title(name))


saveRDS(df_full,file="data/full_dataframe.R")



######### Make Long run dataframe ##########
lr_boy<-read_excel("data/long-run-trends.xlsm",sheet="Boys",skip=1)%>%
  gather(key=year,value=name,-rank)%>%
  mutate(sex="boy")


lr_girl<-read_excel("data/long-run-trends.xlsm",sheet="Girls",skip=1)%>%
  gather(key=year,value=name,-rank)%>%
  mutate(sex="girl")


df_lr<-bind_rows(lr_boy,lr_girl)%>%
  mutate(name=str_replace_all(name,":","-"))%>%
  mutate(name=str_to_title(name))%>%
  filter(!is.na(rank))%>%
  filter(!is.na(name))%>%
  mutate(rank=as.numeric(rank),year=as.numeric(year))%>%
  mutate(count=NA)%>%
  bind_rows(df_full)%>%
  filter(rank<100)%>%
  select(-count)

saveRDS(df_lr,file="data/lr_dataframe.R")

