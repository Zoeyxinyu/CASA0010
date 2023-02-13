# Libraries--------------------------
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(ggplot2)
library(RColorBrewer)
library(classInt)
library(rgeos)
library(rgdal)
library(tidyverse)
library(janitor)
library(geojsonR)
library(dplyr)

library(reshape2)
library(openxlsx)
library(vegan)

#Read data-------------------------------------------------
# shops points London 
shop <- st_read("shopwithstreet.geojson") %>% 
  st_transform(.,27700) %>% 
  clean_names()

# high street boundaries
HSboundaries <- st_read("GLA_High_Street_boundaries.gpkg") %>% 
  clean_names() %>% 
  st_transform(.,27700) %>% 
  mutate(hs_area=st_area(.))

# some are calculate more then once. -
# hs_many <- HSboundaries %>% 
#   st_drop_geometry() %>% 
#   select(highstreet_id) %>% 
#   group_by(highstreet_id) %>% 
#   count(highstreet_id) %>% 
#   filter(n>1)
#  

 # London ward boundary
 ward <- st_read(here::here("ESRI","London_Ward_CityMerged.shp"))%>%
   st_transform(.,27700) %>% 
   clean_names()
 
 
 # calculate overlap areas
 hs_inward<- st_intersection(HSboundaries,ward) %>%
   mutate(intersect_area=st_area(.)) %>% 
   st_drop_geometry()
   


# Find chains-----------------
shop <- shop %>% 
   mutate(name=case_when(str_detect(name,regex("Tesco",ignore_case = T))~"Tesco",
                         str_detect(name, regex("Co-op",ignore_case = T))~"Co-op",
                         str_detect(name,regex("M&S",ignroe_case = T))~"M&S",
                         str_detect(name,regex("Marks and Spencer",ignore_case = T))~"M&S",
                         str_detect(name,regex("Marks & Spencer",ignore_case = T))~"M&S",
                         str_detect(name,regex("Sainsbury's",ignore_case = T))~"Sainsbury's",
                         str_detect(name,regex("ALDI",ignore_case = T))~"Aldi",
                         str_detect(name,regex("Asda",ignore_case = T))~"Asda",
                         str_detect(name,regex("Morrisons"))~"Morrisons",
                         str_detect(name,regex("Lidl"))~"Lidl",
                         str_detect(name,regex("Boots"))~"Boots",
                         str_detect(name,regex("Gail",ignore_case = T))~"Gail's",
                         str_detect(name,regex("Aesop",ignore_case = T))~"Aesop",
                         str_detect(name,regex("Aēsop"))~"Aesop",
                         str_detect(name,regex("Age UK"))~"Age UK",
                         str_detect(name,regex("Albemarle Bond"))~"Albemarle Bond",
                         str_detect(name,regex("Albermarle Bond"))~"Albemarle Bond",
                         str_detect(name,regex("B&Q"))~"B&Q",
                         str_detect(name,regex("Balfe's Bikes"))~"Balfe's Bikes",
                         str_detect(name,regex("Barnardo's"))~"Barnardo's",
                         str_detect(name,regex("Best-one",ignore_case = T))~"Best-one",
                         str_detect(name,regex("British Heart Foundation",ignore_case = T))~"British Heart Foundation",
                         str_detect(name,regex("British Red Cross",ignore_case = T))~"British Red Cross",
                         str_detect(name,regex("CEX"))~"CeX",
                         str_detect(name,regex("Mary's Living",ignore_case = T))~"Mary's Living & Giving",
                         str_detect(name,regex("Mleczko"))~"Mleczko",
                         str_detect(name,regex("nisa local",ignore_case = T))~"Nisa Local",
                         str_detect(name,regex("One Stop Shop"))~"One Stop",
                         str_detect(name,regex("One-Stop"))~"One Stop",
                         str_detect(name,regex("Oxfam"))~"Oxfam",
                         str_detect(name,regex("Princess Alice"))~"Princess Alice",
                         str_detect(name,regex("Reiss",ignore_case = T))~"Reiss",
                         str_detect(name,regex("Royal Trinity",ignore_case = T))~"Royal Trinity Hospice",
                         str_detect(name,regex("RSPCA"))~"RSPCA",
                         str_detect(name,"Rush")~"Rush Hair",
                         str_detect(name,"^Ryman")~"Ryman",
                         str_detect(name,"^Nisa")~"Nisa Local",
                         str_detect(name,"Best One")~"Best-one",
                         str_detect(name,"claire's")~"Claire's",
                         str_detect(name,"Kwik-Fit")~"Kwik Fit",
                         TRUE ~ name))
 
# find all chain stores in london
# chains <- shop %>%
#   count(name) %>%
#   filter(n>=5)
#  st_write(chains,dsn = "chains(over_5).geojson",driver = "geojson")

 
# 在伦敦数量超过5家的店铺被定义为chain
chains <- st_read("chains(over_5).geojson") %>% 
   st_transform(.,27700)
 
# drop geometry for join
chains1 <- chains %>% 
  st_drop_geometry()

# list of chain stores
chains_list <- chains$name

# 加一列显示是否是chain
shop <- shop %>% 
  mutate(chain=case_when(name %in% chains_list ~ "yes",
                         TRUE ~ "no"))


# Plot-----------
# 书里找的代码

# all shops in London
tm1 <- tm_shape(ward)+
  tm_polygons("#EFEFEF")+
  # add shop points
  tm_shape(shop)+
    tm_dots(col = "#607EAA",size = 0.05, alpha=0.5)+
  # tm_add_legend(type = "symbol",col = "#FB6A4A",labels = "Shops")+
  tm_layout(frame = F,legend.position = c(0.8,0.1),legend.show = T)
tm1

# location of high streets
tm2 <- tm_shape(ward)+
  tm_polygons("#EFEFEF")+
  tm_shape(HSboundaries)+
    tm_borders(col = "#850E35")+
  # tm_add_legend(type = "fill",col = "#0d3b66",labels = "High Street Boundaries")+
  tm_layout(frame = F,legend.position = c(0.8,0.1),legend.show = T)
tm2



# shops + high street + ward
tm3 <- tm_shape(ward)+
  tm_polygons("#EFEFEF")+
  # high street
  tm_shape(HSboundaries)+
  tm_borders(col = "#850E35")+
  # add shop points
  tm_shape(shop)+
  tm_dots(col = "#607EAA",size = 0.05, shape = 1, alpha=0.5)+
tm_layout(frame = F)
tm3



# Join shop to street------------
shop_join_hs <- shop %>% 
  st_join(.,HSboundaries)



# Shops on or not on the high street----------
# 高街商店
hs_shop <- shop_join_hs%>% 
  filter(!is.na(highstreet_id)) %>% 
  drop_na(name)


# 高街之外的商店（被认为不在高街上）
non_hs_shop <- shop_join_hs %>% 
  filter(is.na(highstreet_id)) %>% 
  drop_na(name)



#Plot shops (not) on high street-----------
# shop on hs, hs, ward
tm4 <- tm_shape(ward)+
  tm_polygons("#EFEFEF")+
  tm_shape(HSboundaries)+
    tm_borders(col = "#850E35")+
    # add shop points
    tm_shape(hs_shop)+
    # tm_dots(col = "chain",palette="Accent",size = 0.01,shape = 1,alpha=0.5)+
    tm_dots(col = "#607eAA",size = 0.05, shape = 1, alpha=0.5)+
  # legend 还是P上去吧，不然很难调整
    # tm_add_legend(type = "symbol",col = "#FB6A4A",labels = "Shops on High Street")+
     tm_layout(frame = F)
tm4



# 高街和高街商店
data1 <- hs_shop %>% 
  group_by(highstreet_id) %>% 
  count(shop) %>% 
  st_drop_geometry() %>% 
  rename(type_total=n)# 计算每条街上每种类型一共有多少
  

# 高街不同类型商店计数
hs_shop <- hs_shop %>%
  left_join(.,data1,by=c("highstreet_id","shop"))

# 顺便计算下高街上的总商店数量
count_hs_shop <- hs_shop %>% 
  count(highstreet_id) %>% 
  st_drop_geometry() %>% 
  rename(total_shop=n)
# add total numbers of shops on high street
# join 
hs_shop <- hs_shop %>% 
  left_join(.,count_hs_shop,by="highstreet_id") %>%
  filter(total_shop>=15)
# 只考虑商店数量大于15的街道


# 高街上的chain store number
data2 <- hs_shop %>% 
  group_by(highstreet_id) %>% 
  count(chain) %>% 
  st_drop_geometry() %>% 
  filter(chain=="yes") %>% 
  rename(total_chain=n) %>% 
  select(highstreet_id,total_chain)


# join number of chain store to hs_shop
hs_shop <- hs_shop%>% 
  left_join(.,data2,by=c("highstreet_id"))

hs_shop$total_chain[is.na(hs_shop$total_chain)] <- 0




# # for calculate
# cal_hs_shop <- hs_shop %>% 
#   select(highstreet_id,shop,chain,type_total,total_chain,total_shop) 
# # pivot table
# data_wider1 <- cal_hs_shop %>%
#   select("highstreet_id","shop","type_total") %>% 
#   st_drop_geometry() %>% 
#   distinct() %>% 
#   pivot_wider(names_from = shop,
#               values_from = type_total)
# data_wider1[is.na(data_wider1)] <- 0
# # sort data by highstreet_id
# data_wider2 <- data_wider1[order(data_wider1$highstreet_id),]
# data_wider2$index <- 1:nrow(data_wider2)
# 
# matrix_shannon <- data_wider2[2:444]
# # Shannon diversity index
# shannon =list(diversity(matrix_shannon, index = "shannon"))
# 
# df <- data.frame(matrix(unlist(shannon), ncol =length(shannon), byrow = F))
# df$index <- 1:nrow(df)
# # join shannon to shops on high street
# data_shannon <- data_wider2 %>% 
#   left_join(.,df,by=("index")) %>% 
#   clean_names() %>% 
#   rename(shannon_index=matrix_unlist_shannon_ncol_length_shannon_byrow_f) %>% 
#   select(highstreet_id,shannon_index)


final_hs_shop <- hs_shop %>% 
  # calculate proportion of chain stores
  mutate(pro_chain = round(total_chain/total_shop *100,3)) %>% 
  mutate(pro_type=round(type_total/total_shop*100,3))
  
 

# # shannon index和数量之间有什么关系
# ggplot(data=final_hs_shop,aes(x=total_shop,y=shannon_index))+
#   geom_point()

forjoin <- final_hs_shop %>% 
  select(highstreet_id,
         pro_chain,total_shop,total_chain) %>% 
  st_drop_geometry() %>% 
  distinct()

HSboundaries_join <- HSboundaries %>% 
  left_join(.,forjoin,by="highstreet_id") %>% 
  mutate_at(c('pro_chain',
              "total_shop","total_chain"), ~replace_na(.,0)) %>% 
  filter(total_shop>=15) 
# why choose 15
# https://link.springer.com/article/10.1057/rlp.2008.25 






# plot proportion of chain stores-------
tm4 <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HSboundaries_join)+
  tm_polygons("pro_chain", title="Proportion of Chain Stores on High Street (%)", 
              palette = "Reds",
              legend.hist = T) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6
            )
tm4

# # relationship between diversity and proportion of chain stores(linear regression)
# # correlation between diversity and chain proportion-
# 
# cor_data <- HSboundaries_join %>% 
#   select(shannon_index,pro_chain) %>% 
#   st_drop_geometry()
# 
# library(ggpubr)
# # scatter
# tm8 <- ggplot(cor_data) +
#   aes(x = shannon_index, y = pro_chain) +
#   geom_point(colour = "#0c4c8a",size=0.8) +
#   theme_minimal()
# tm8
# 
# tm9 <- ggscatter(cor_data, x = "shannon_index", y = "pro_chain", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Shannon Diversity Index", ylab = "Proportion of Chains")
# tm9
# # pearson correlation-------
# cor <- cor(cor_data$shannon_index,cor_data$pro_chain,method = "pearson")
# cor
# res <- cor.test(cor_data$shannon_index,cor_data$pro_chain, 
#                 method = "pearson")
# res




# # find specialised street
# specialised_street<- final_hs_shop %>% 
#   filter(total_shop>=15) %>%
#   select(highstreet_id,highstreet_name,shop,pro_type,type_total) %>% 
#   st_drop_geometry() %>% 
#   distinct() %>% 
#   arrange(pro_type,desc=T) %>%
#   tail(10)
# # streets have dominate business
# dominate_type <- final_hs_shop %>% 
#   filter(total_shop>=15) %>%
#   select(highstreet_id,highstreet_name,shop,pro_type,type_total) %>% 
#   st_drop_geometry() %>% 
#   distinct() %>% 
#   filter(pro_type>=50)
#   
# specialised_join <- HSboundaries_join %>% 
#   left_join(.,specialised_street,by="highstreet_id") %>% 
#   filter(!is.na(shop))
# 
# 
# # plot streets have a main shop type
# tm12 <- tm_shape(ward) +
#   tm_polygons(col = NA)+
#   tm_shape(specialised_join)+
# 
#   tm_polygons("shop",palette="RdYlBu" ,title = "Shop Types", size=1) +
#   # tm_text(text = "pro_type", 
#   #         size = 0.5,
#   #         col = "black",) +
#   tm_layout(frame = F,legend.position = c(0.8,0.1))
# tm12
  
# # plot streets have a type accounts for >50% 
# # 如果一个类型的商店在所在的街道上占比超过了50%（why 50%），我们就认为这个
# # 类型是该街道的一个主要类型
# # 找到了一条街，它的conveninence 达到了半数
# # where is it
# 
# dominate_join <- HSboundaries_join %>% 
#   left_join(.,dominate_type,by="highstreet_id") %>% 
#   filter(!is.na(shop))

# tm13 <- tm_shape(ward)+
#   tm_polygons()+
#   tm_shape(dominate_join)+
#   tm_polygons("pro_type",palette="RdYlBu",
#               title="Shop types make up 30% or more of the high street")+
#     tm_layout(frame = F,legend.position = c(0.8,0.1),legend.show = T)
# tm13

# location quotient=========

group <- shop %>% 
  group_by(shop) %>% 
  count(shop) %>% 
  st_drop_geometry() %>% 
  rename(all_type_count=n)
# 全伦敦类型最多多商店类型
# top10：convenience,hairdresser,clothes,beauty,supermarket,dry_cleaning
# bookmaker,newsagent,charity,bakery

# join data to the final hs shop
final_hs_shop <- final_hs_shop %>% 
  left_join(.,group,by="shop")

shop_top10 <- list("convenience","hairdresser","clothes","bakery",
                   "beauty","supermarket","dry_cleaning","bookmaker",
                   "newsagent","charity")

# 区位熵反映地区层面的产业聚集程度。现在我要看的是每条街道上这十种类型分别
# 的聚集程度如何

# 为啥要选top10，因为某些类型的商店数量太少，算出来的值很大，但没什么意义。

LQ_data<- final_hs_shop %>%
  st_drop_geometry() %>% 
  select("highstreet_id","shop","type_total",
         "total_shop","all_type_count") %>%
  distinct() %>%
  mutate(local_concentration=type_total/total_shop) %>% 
  mutate(national_concentration=all_type_count/32589) %>% 
  mutate(location_quotient=round(local_concentration/national_concentration,3))


LQ_wider <- LQ_data %>% 
  select("highstreet_id","location_quotient","shop") %>% 
  pivot_wider(names_from = shop,
              values_from = location_quotient) %>% 
  select("highstreet_id","convenience","hairdresser","clothes","bakery",
         "beauty","supermarket","dry_cleaning","bookmaker",
         "newsagent","charity")

LQ_wider[is.na(LQ_wider)] <- 0

st_write(LQ_wider,dsn = "LQ_result.csv",driver="csv")

# join 到高街数据上，为了可视化
HS_join_lq <- HSboundaries_join %>% 
  left_join(.,LQ_wider,by="highstreet_id")

# plot LQ==============

## 哪些高街具有更高的克隆风险， 从克隆零售业类别来看
lq_result <- read_csv("LQ_result.csv") %>% 
  select(highstreet_id,count_lq_cluster)

HS_join_lq <- HS_join_lq %>% 
  left_join(.,lq_result,by="highstreet_id") 


tm_lq <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("count_lq_cluster", 
  title = "Number of varieties of retail clustered on high street", 
              palette = "Purples",
              ) + 
  tm_layout(frame = F,legend.position = c(0.8,0.1))
tm_lq

## convenience--------------
tm_convenience <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("convenience", title = "Location Quotient of Convenience Shop", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,5)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)

tm_convenience

## hairdresser--------------
tm_hairdresser <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("hairdresser", title = "Location Quotient of Hairdresser", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,5)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)

tm_hairdresser


## clothes--------
tm_clothes <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("clothes", title = "Location Quotient of Clothes Shop", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,6)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_clothes


## bakery--------------
tm_bakery <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("bakery", title = "Location Quotient of Bakery Shop", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,15)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_bakery

## beauty--------------
tm_beauty <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("beauty", title = "Location Quotient of Beauty Shop", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,10)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_beauty



## supermarket------------
tm_supermarket <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("supermarket", title = "Location Quotient of Supermarket", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,10)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_supermarket

##dry_cleaning---------
tm_dry_cleaning <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("dry_cleaning", title = "Location Quotient of Dry Cleaning Store", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,10)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_dry_cleaning


##bookmaker---------
tm_bookmaker <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("bookmaker", title = "Location Quotient of Bookmaker", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,10)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_bookmaker


# "convenience","hairdresser","clothes","bakery",
# "beauty","supermarket","dry_cleaning","bookmaker",
# "newsagent","charity"

##newsagent---------
tm_newsagent <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("newsagent", title = "Location Quotient of Newsagent", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,10)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_newsagent



##charity---------
tm_charity <- tm_shape(ward) +
  tm_polygons(col = "#EFEFEF")+
  tm_shape(HS_join_lq)+
  tm_polygons("charity", title = "Location Quotient of Charity Store", 
              palette = "Reds",
              legend.hist = T,
              breaks=c(0,1,10)) + 
  tm_layout(frame = F,legend.outside = TRUE, 
            legend.outside.position = "right",
            legend.hist.height = 0.5,
            legend.hist.width = 0.6)
tm_charity

# regression------------------

## read ward csv data--------
WardData <- read_csv(here("ward-profiles-excel-version.csv"),
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
  clean_names()

# calculate intersection
hs_inward1 <- hs_inward %>% 
  mutate(intersect_area=as.numeric(intersect_area)/10000) %>% 
  mutate(pro_intersect=round(intersect_area/area_ha,4)) %>% 
  select(highstreet_id,gss_code,pro_intersect) %>% 
  left_join(.,WardData,by=c("gss_code"="new_code")) %>% 
  select(highstreet_id,gss_code,pro_intersect,
         median_household_income_estimate_2012_13,
         population_density_persons_per_sq_km_2013,
         percent_bame_2011,
         median_house_price_2014,
         average_public_transport_accessibility_score_2014) %>% 
  mutate(median_household_income_estimate_2012_13=median_household_income_estimate_2012_13*pro_intersect,
         population_density_persons_per_sq_km_2013=population_density_persons_per_sq_km_2013*pro_intersect,
         percent_bame_2011=percent_bame_2011*pro_intersect,
         median_house_price_2014=median_house_price_2014*pro_intersect,
         average_public_transport_accessibility_score_2014=average_public_transport_accessibility_score_2014*pro_intersect) %>% 
  group_by(highstreet_id) %>% 
  summarise(median_household_income=sum(median_household_income_estimate_2012_13),
            population_density=sum(population_density_persons_per_sq_km_2013),
            percent_bame=sum(percent_bame_2011),
            median_house_price=sum(median_house_price_2014),
            public_transport=sum(average_public_transport_accessibility_score_2014))
  


# join to high street boundaries

HSboundaries_all <- HSboundaries_join %>% 
  left_join(.,hs_inward1,by="highstreet_id") %>% 
  select(-c(objectid,area_ha)) %>% 
  st_drop_geometry() %>% 
  left_join(.,LQ_wider,by="highstreet_id")

sub_data <- HSboundaries_all %>% 
  select(pro_chain,median_household_income,
         population_density,percent_bame,median_house_price,
         public_transport,"convenience","hairdresser","clothes","bakery",
         "beauty","supermarket","dry_cleaning","bookmaker",
         "newsagent","charity")

ggplot(data=sub_data,aes(x=median_house_price,y=pro_chain))+
  geom_point()

# 调整回归模型
model1 <- lm(convenience~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model1)

model2 <- lm(hairdresser~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model2)

model3 <- lm(clothes~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model3)


model4 <- lm(bakery~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model4)

model5 <- lm(beauty~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model5)


model6 <- lm(supermarket~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model6)

model7 <- lm(dry_cleaning~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model7)



model8 <- lm(bookmaker~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model8)

model9 <- lm(newsagent~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model9)

model10 <- lm(charity~ median_house_price
             +median_household_income
             +population_density+percent_bame,
             data = sub_data)
summary(model10)

model11 <- lm(pro_chain~ median_house_price
              +median_household_income
              +population_density+percent_bame,
              data = sub_data)
summary(model11)


# 最开始是根据街道商店的type做的余弦相似度，但是问题在于，type的种类太多
# 去掉一些type也不合理。围绕clone town 和连锁店，应该探讨的是那些大型连锁店造成的
# 相似街道，这个也符合clone town的核心

# 这一部分不用
# # which types are the most common
# group <- hs_shop %>% 
#   group_by(shop) %>% 
#   count(shop)
# maintype <- group %>% 
#   filter(n>=50) %>% 
# #设置50是否合理？感觉应该再小点
# #如果一个类型的商店在全伦敦只有不到50家，那就认为是小众类型，不纳入功能相似的考虑范围
#   st_drop_geometry()
# 
# # list of main types
# main_type_list <- maintype$shop
# 
# 
# 
# data_wider3 <- hs_shop %>%
#   filter(shop %in% main_type_list) %>% 
#   select("highstreet_id","shop","type_total") %>% 
#   st_drop_geometry() %>% 
#   distinct() %>% 
#   pivot_wider(names_from = shop,
#               values_from = type_total)
# 
# data_wider3[is.na(data_wider3)] <- 0
# 
# st_write(data_wider3,"for_similarity.csv","csv")
# # # sort data by highstreet_id
# # data_wider2 <- data_wider1[order(data_wider1$highstreet_id),]
# 
# 
# 
# 
# # 加一列显示是否是chain
# shop <- shop %>% 
#   mutate(chain=case_when(name %in% chains_list ~ "yes",
#                          TRUE ~ "no"))


# 首先选择我要的大型连锁店
# 合并连锁店品牌，例如Tesco Express, Tesco Metro 均为Tesco

# combine_chain <- hs_shop %>% 
#   # filter(str_detect(name,"Tesco")) %>% 
#   mutate(name=case_when(str_detect(name,regex("Tesco",ignore_case = T))~"Tesco",
#                         str_detect(name, regex("Co-op",ignore_case = T))~"Co-op",
#                         str_detect(name,regex("M&S",ignroe_case = T))~"M&S",
#                         str_detect(name,regex("Marks and Spencer",ignore_case = T))~"M&S",
#                         str_detect(name,regex("Marks & Spencer",ignore_case = T))~"M&S",
#                         str_detect(name,regex("Sainsbury's",ignore_case = T))~"Sainsbury's",
#                         str_detect(name,regex("ALDI",ignore_case = F))~"Aldi",
#                         str_detect(name,regex("Asda",ignore_case = T))~"Asda",
#                         str_detect(name,regex("Morrisons"))~"Morrisons",
#                         str_detect(name,regex("Lidl"))~"Lidl",
#                         str_detect(name,regex("Boots"))~"Boots",
#                         TRUE ~ name))

# leading_chain <- list('Tesco','Co-op','M&S',"Sainsbury's",'Aldi','Asda','Morrisons','Lidl','Boots')

# leading retailers on high street
# leading_chain1 <- combine_chain %>% 
#   filter(name %in% leading_chain)


# Cosine similarity===========
# 想办法计算余弦相似度
data_wider3 <- final_hs_shop %>% 
  filter(chain=="yes") %>% 
  filter(pro_chain>=30) %>% 
  group_by(highstreet_id) %>% 
  count(name) %>% 
  rename(count_chain=n) %>% 
    select("highstreet_id","name","count_chain") %>%
    st_drop_geometry() %>%
    pivot_wider(names_from = name,
                values_from = count_chain)

data_wider3[is.na(data_wider3)] <- 0

# st_write(data_wider3,dsn = "chain_matrix(pro>30).csv",driver="csv")

# 思考：是应该计算所有街道的余弦相似度，还是说应该计算那些连锁店达到一定比例的街道
# 的余弦相似度？如果不考虑连锁店的比例，那么一条有100家店只有一家Tesco的街道，和另一条
# 条10家店只有一家Tesco的街道余弦相似度就很高，但是并不能说明他们两是相似的。


# 考虑pro_chain，只考虑连锁店比例大于30的街道

name <- HSboundaries %>% 
  select(highstreet_id,highstreet_name) %>% 
  st_drop_geometry()


cosine_simi <- read_csv("cosine_simi.csv") %>% 
  left_join(.,name,by=c("High Street ID"="highstreet_id")) %>% 
  rename("High Street Name"="highstreet_name") %>% 
  left_join(.,name,by=c("ID of Similar High Street"="highstreet_id")) %>% 
  rename("Name of Similar High Street"="highstreet_name")
st_write(cosine_simi,dsn="cosine_simi_addname.csv",driver="csv")
