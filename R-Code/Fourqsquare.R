library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)
library(chron)
library(maps)
library(mapdata)
library(ggmap)
library(plotly)
library(leaflet)
library(scales)
library(osmdata)
library(sitools)
library(arules)
library(RColorBrewer)
library(arulesViz)
library(geosphere)
library(networkD3)
library(tibble)
library(tidyverse)
library(factoextra)
library(clValid)
library(revgeo)
library(dbscan)


##################### initial data preparation #####################
setwd("C:\\Users\\Ismail\\Desktop\\OVGU\\DKE Subjects\\Data Science with R\\Foursquare project\\Global-scale Check-in Dataset")

con <- dbConnect(odbc::odbc(),  .connection_string = 'driver={SQL Server};server=localhost;database=Foursquare_Global_Data;trusted_connection=true')

# create objects pointing to database tables
checkin <- tbl(src = con, 'Checkins')
poi <- tbl(src = con, 'POIs')
city <- tbl(src = con, 'City') %>% collect()

# joining checkins and pois (Points of Intrest or venues)
chk_poi <- inner_join (x = checkin, y= poi, by= 'Venue_ID')

# checkins data of the needed country
chk_poi_country <- chk_poi %>%
  filter(Country_Code == 'JP')%>%
  collect()

# create object for reading venue categories hierarchy
venue_sub_cat <- read.csv("venue_subcategories.csv")

# handle old categories that are not present anymore in foursquare API (names of the new replacement)
fixed_venue <- read.csv("venue_subcategories_complementary.csv")

# hanlde main categoies (present only as parent)
main_venue <- venue_sub_cat %>% filter(level == 0) %>% select(venue_category) %>% unique()
main_venue<- add_column(main_venue, venue = main_venue$venue_category, .before = "venue_category")
main_venue$level <- 0

venue_sub_cat <- rbind(venue_sub_cat,main_venue)

# hanlde level one categoies (referring to themselves for level one column lookup)
`%notin%` = Negate(`%in%`)

temp_venue_list <- venue_sub_cat %>% filter(!level == 0)
temp_venue_list_1 <- venue_sub_cat %>% filter(level == 0, venue_sub_cat$venue %notin% temp_venue_list$venue,venue_sub_cat$venue %notin% main_venue$venue) %>%  select(venue)
temp_venue_list_1 <- temp_venue_list_1 %>% mutate(venue_category = ifelse(grepl("Restaurant",venue,ignore.case = TRUE),"Restaurant",as.character(venue)),level = 1)

lvl_one_venue <- venue_sub_cat %>% filter(level == 1) %>% select(venue_category) %>% unique()
lvl_one_venue<- add_column(lvl_one_venue, venue = lvl_one_venue$venue_category, .before = "venue_category")
lvl_one_venue$level <- 1

lvl_one_venue <- rbind(lvl_one_venue, temp_venue_list_1)
lvl_one_venue <- lvl_one_venue %>% distinct()

venue_sub_cat <- rbind(venue_sub_cat,lvl_one_venue)

# replace old non-existing categories with new fixed categories
chk_poi_country <- left_join (x = chk_poi_country, y= fixed_venue, by = c("Venue_Category_Name" = "venue"))

chk_poi_country <- chk_poi_country %>%
  mutate(Venue_Category_Name = ifelse(is.na(as.character(fixed_venue)), as.character(Venue_Category_Name),as.character(fixed_venue)))

# look up level one categories 
chk_poi_country <- left_join (x = chk_poi_country, y= venue_sub_cat %>%
                    filter(level == 1), by = c("Venue_Category_Name" = "venue"))

colnames(chk_poi_country)[colnames(chk_poi_country)=="venue_category"] <- "venue_category_lvl_one"

# look up level zero categories
chk_poi_country <- left_join (x = chk_poi_country,
                              y= venue_sub_cat %>% filter(level == 0),
                              by = c("Venue_Category_Name" = "venue"))

# prepare both data sets to have same columns to be binded together
chk_poi_country$fixed_venue <- NULL
chk_poi_country$level.x <- NULL
chk_poi_country$level.y <-NULL

# add columns calculating the difference between consecutive checkins in terms of time (min) and distance (km)
chk_poi_country <- chk_poi_country %>%
  group_by(User_ID) %>% 
  arrange(Chk_time) %>% 
  mutate(nxt_chk_time = lead(Chk_time),
         IC_Time = (nxt_chk_time - Chk_time) / 60,
         nxt_chk_lon = lead(Longitude),
         nxt_chk_lat = lead(Latitude),
         IC_Dist = distGeo(matrix(c(Longitude, Latitude), ncol = 2),matrix(c(nxt_chk_lon, nxt_chk_lat), ncol = 2))/1000,
         nxt_venue_cat_name = lead(Venue_Category_Name),
         nxt_venue_cat = lead(venue_category),
         nxt_venue_cat_lvl_one = lead(venue_category_lvl_one)
        )

chk_poi_country <- ungroup(chk_poi_country)


# How many checkins happend in the country
num_checkins <- nrow(chk_poi_country)

# How many users in the country
num_user <- length(unique(chk_poi_country$User_ID))

# How many venues in the country
num_venue <- length(unique(chk_poi_country$Venue_ID))

# country name
country_lookup <- unique(city %>% select(Country_Code,Country_Name))
country_code <- unique(chk_poi_country$Country_Code)
country_name <- country_lookup %>% filter(Country_Code == country_code) %>% select(Country_Name)


##################### simple Analysis #####################

# a Histogram  for checkins
checkins_hist <- ggplot(chk_poi_country, aes(x = date(Chk_time))) +
  geom_histogram(bins = 18, col="red", aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "red", high = "green") +
  ggtitle(paste("Checkins Histogram for", country_name)) +
  scale_x_date(labels = date_format("%Y-%b"),date_breaks = "3 months") +
#, breaks = seq.Date(floor_date(min(date(chk_poi_country$Chk_time)), "month"),ceiling_date(max(date(chk_poi_country$Chk_time)), "month") - ddays(1),"months"))+
  scale_y_continuous(labels = scales::comma) + 
  ylab("Count Checkins") + 
  xlab("Year and Month") +
  theme_bw()


# list of top categories
Top_cats <- chk_poi_country %>%
  count(venue_category, sort = TRUE)

# list of top sub-categories
Top_subcats <- chk_poi_country %>%
  count(venue_category_lvl_one, sort = TRUE) %>%
  head(n = 7)

# counts of top categories
Top_cat_data <- chk_poi_country %>%
  count(Country_Code, venue_category,sort = TRUE)

# counts of top 10 sub-categories
Top10_subcat_data <- chk_poi_country %>%
  filter(!is.na(venue_category_lvl_one)) %>%
  count(Country_Code, venue_category_lvl_one,venue_category,sort = TRUE) %>%
  head(n = 10)


# Bar Chart for top categoies ordered by count
Top_cat_bar <- ggplot(Top_cat_data, aes(x = reorder(venue_category, -n), y=n)) +
  geom_bar(stat = 'identity', fill = "Blue") +
  xlab('Venue_Category') +
  ylab('Check-ins count')+
  ggtitle(paste("Top Categories in", country_name)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma)
  


# Bar Chart for top sub-categoies ordered by count
Top10_subcat_bar <- ggplot(Top10_subcat_data, aes(x = reorder(venue_category_lvl_one, -n), y=n)) +
  geom_bar(stat = 'identity', fill = "Blue")+
  xlab('Venue_SubCategory') +
  ylab('checkin_count')+
  ggtitle(paste("Top-10 Sub-categories in", country_name)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma)

# a cumulative density graph based on number of checkins by user
User_chk_count_data <- chk_poi_country %>%
  group_by(User_ID) %>%
  summarize(count = n()) %>%
  arrange(count)

User_chk_count_data$cumsum <- cumsum(User_chk_count_data$count)
User_chk_count_data$prob <- cumsum(User_chk_count_data$count) / sum(User_chk_count_data$count)
User_chk_count_cdf <- ggplot(data = User_chk_count_data, aes(x= seq_along(User_ID),y = prob)) +
  geom_point() +
  ggtitle(paste("Distribution of checkins by users in", country_name)) +
  xlab('Number of users') +
  ylab('Distribution') +
  geom_hline(yintercept = 0.5,linetype="dotted", size = 2, color = "red")

## a cumulative density graph based on number of checkins for each venue category 
# Venue_cat_chk_count_data <- chk_poi_country %>%
#   group_by(venue_category) %>%
#   summarize(count = n()) %>%
#   arrange(count)
# 
# Venue_cat_chk_count_data$cumsum <-cumsum(Venue_cat_chk_count_data$count)
# Venue_cat_chk_count_data$prob <- cumsum(Venue_cat_chk_count_data$count) / sum(Venue_cat_chk_count_data$count)
# Venue_cat_chk_count_cdf <- ggplot(data = Venue_cat_chk_count_data, aes(x= seq_along(venue_category), y = prob)) +
#   geom_point() +
#   xlab('number of venue categories') +
#   ylab('probability')
  ##scale_y_continuous(labels = scales::comma)

# Multi-bar chart for the distribution of top sub-categories over the week days
Top_subcat_distr_week_data <- chk_poi_country %>%
  filter(chk_poi_country$venue_category_lvl_one %in% Top_subcats$venue_category_lvl_one) %>%
  count(venue_category_lvl_one,checkin_day= wday(Chk_time, label = TRUE))

Top_subcat_distr_week_multibar <- ggplot(Top_subcat_distr_week_data, aes(x= checkin_day,y= n, fill= venue_category_lvl_one)) +
  geom_bar(stat = 'identity', position = "dodge") +
  geom_smooth(method = "lm") +
  ylab("Count Checkins") +
  xlab("Checkin Day") +
  scale_y_continuous(labels = scales::comma)

# Top_cat_distr_week_data <- chk_poi_country %>%
#   filter(chk_poi_country$venue_category
#          %in% Top_cats$venue_category) %>%
#   count(venue_category,checkin_day= wday(Chk_time, label = TRUE))
# 
# Top_cat_distr_week_multibar <- ggplot(Top_cat_distr_week_data, aes(x= checkin_day,y= n, fill= venue_category)) +
#   geom_bar(stat = 'identity', position = "dodge") +
#   geom_smooth(method = "lm")

# Density chart for distribution of top sub-categories over hours of the day
Top_subcat_distr_hours_data <- chk_poi_country %>%
  filter(chk_poi_country$venue_category_lvl_one %in%
           Top_subcats$venue_category_lvl_one) %>%
  mutate(time = hour(Chk_time) + minute(Chk_time)/60+ seconds(Chk_time)/3600)

Top_subcat_distr_hours_area <- ggplot(Top_subcat_distr_hours_data, aes(x= time, fill= venue_category_lvl_one)) +
  geom_area(stat = "bin",alpha = 0.6) +
  scale_x_continuous(name="Hours of the day", limits=c(0, 24), breaks = c(0:24))

Top_subcat_distr_hours_density <- ggplot(Top_subcat_distr_hours_data, aes(x= time, y=..density.. ,fill= venue_category_lvl_one)) +
  geom_density(position = "stack", alpha = 0.6) +
  scale_x_continuous(name="Hours of the day", limits=c(0, 24), breaks = c(0:24))


# Top_cat_distr_hours_data <- chk_poi_country %>%
#   filter(chk_poi_country$venue_category %in%
#            Top_cats$venue_category) %>%
#   mutate(time = hour(Chk_time) + minute(Chk_time)/60+ seconds(Chk_time)/3600)
# 
# Top_cat_distr_hours_area <- ggplot(Top_cat_distr_hours_data, aes(x= time, fill= venue_category)) +
#   geom_area(stat = "bin",alpha = 0.6) +
#   scale_x_continuous(name="Hours of the day", limits=c(0, 24), breaks = c(0:24)) +
#   scale_y_continuous(labels = scales::comma)
# 
# Top_cat_distr_hours_density <- ggplot(Top_cat_distr_hours_data, aes(x= time, y=..density.. ,fill= venue_category)) +
#   geom_density(position = "stack", alpha = 0.6) +
#   scale_x_continuous(name="Hours of the day", limits=c(0, 24), breaks = c(0:24))+
#   scale_y_continuous(labels = scales::comma)


# for distribution of top sub-categories over months of the year
Top_subcat_distr_month_data <- chk_poi_country %>%
  filter(chk_poi_country$venue_category_lvl_one %in%
           Top_subcats$venue_category_lvl_one) %>%
  count(venue_category_lvl_one, DT= date(paste(year(Chk_time), "-", month(Chk_time), "-01", sep="")))

Top_subcat_distr_month_density <- ggplot(Top_subcat_distr_month_data, aes(x= DT, y= n,fill= venue_category_lvl_one)) +
  geom_area(stat = "identity", alpha = 0.6) +
  scale_x_date(labels = date_format("%Y-%b"), date_breaks ="3 months") +
  #scale_y_continuous(labels = "comma") +
  xlab("Year and Month") +
  ylab("Count Checkins") +
  guides(fill = guide_legend(title = "venue category")) +
  theme_bw()

# Top_cat_distr_month_data <- chk_poi_country %>%
#   filter(chk_poi_country$venue_category %in%
#            Top_cats$venue_category) %>%
#   count(venue_category, DT= date(paste(year(Chk_time), "-", month(Chk_time), "-01", sep="")))
# 
# Top_cat_distr_month_density <- ggplot(Top_cat_distr_month_data, aes(x= DT, y= n,fill= venue_category)) +
#   geom_area(stat = "identity", alpha = 0.6) +
#   scale_x_date(labels = date_format("%Y-%b"), date_breaks ="1 month") +
#   #scale_y_continuous(labels = "comma") +
#   xlab("Year and Month") +
#   ylab("Count Checkins") +
#   theme_bw()

##################### Frequent Pattern Analysis #####################

# Combine all places visited by user in each day in a cell (userid,date,categories)
Freq_pattern_data <- chk_poi_country %>%
  filter(!is.na(nxt_venue_cat_lvl_one)
         #!(venue_category == nxt_venue_cat & date(Chk_time) == date(nxt_chk_time))
         ) %>%
  group_by(User_ID,Chk_time = date(Chk_time)) %>%
  arrange(Chk_time) %>%
  summarise(visited  = paste(venue_category_lvl_one, collapse =","))

#Freq_pattern_data$Trn <- seq.int(nrow(Freq_pattern_data))

Freq_pattern_data$User_ID <- NULL
Freq_pattern_data$Chk_time <-NULL
Freq_pattern_data %>% ungroup()

# Freq_pattern_data$visited <- as.factor(Freq_pattern_data$visited)
# Freq_pattern_trn <- as(Freq_pattern_data, "transactions")

write.csv(Freq_pattern_data,"Trn.csv", quote = FALSE, row.names = FALSE)

Freq_pattern_trn <- read.transactions(file = "Trn.csv", format = 'basket', sep=',', rm.duplicates = FALSE, skip = 1)

summary(Freq_pattern_trn)

#itemFrequencyPlot(Freq_pattern_trn,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#itemFrequencyPlot(Freq_pattern_trn,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

start.time <- Sys.time()
association.rules <- eclat(Freq_pattern_trn, parameter = list(supp=0.0001, minlen=3))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
association.rules <- apriori(Freq_pattern_trn, parameter = list(supp=0.00001, conf=0.01, minlen = 3))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# remove duplicate rules
#subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1)

#subset.rules <- subset(association.rules, subset = rhs %in% C("Burger Joint"))
#c("Road","Bus Station","Home (private)","Gas Station","Parking","States & Municipalities","Office"))

# sort (subset.rules, by="lift", decreasing=TRUE)
# 

# find rules of activities done before gym
Burger_rules <- apriori(Freq_pattern_trn, parameter =  list(supp=0.00001,conf=0, maxlen=3),appearance = list(default="lhs",rhs = "Burger Joint"))

inspectDT(Burger_rules)
 
# # find rules of activities following gym
# Sport.association.rules <- apriori(Freq_pattern_trn, parameter = list(supp=0.00001, conf=0.5),appearance = list(default="rhs",lhs="Athletics & Sports"))

# create a graphical chart for top 10 rules and items
Top_Burger_rules <- head(Burger_rules, n= 10 , by = "lift")

plot(Top_Burger_rules, method = "graph",  engine = "htmlwidget")

##################### Sequence Pattern Analysis #####################

# create a Sankey diagram to show frequent movement from one sub-category to the other
# sankey for level one categories
total <- chk_poi_country %>%
  filter(!is.na(nxt_venue_cat_lvl_one),
         !( venue_category_lvl_one == nxt_venue_cat_lvl_one)) %>%
  group_by(venue_category_lvl_one,nxt_venue_cat_lvl_one) %>%
  summarize(Count = n()) %>%
  ungroup()

supp <- 0.0002
supp_count <- sum(total$Count) * supp

supp_1 <- 0.0004
supp_count_1 <- sum(total$Count) * supp_1

supp_2 <- 0.0007
supp_count_2 <- sum(total$Count) * supp_2

Sequence_chk_agregated_level_one <- chk_poi_country %>%
  filter(!is.na(nxt_venue_cat_lvl_one),
         !( venue_category_lvl_one == nxt_venue_cat_lvl_one)) %>%
  group_by(venue_category_lvl_one,nxt_venue_cat_lvl_one) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  filter(Count >= supp_count) %>%
  top_n(20,-Count)

Sequence_chk_agregated_level_one_1 <- chk_poi_country %>%
  filter(!is.na(nxt_venue_cat_lvl_one),
         !( venue_category_lvl_one == nxt_venue_cat_lvl_one)) %>%
  group_by(venue_category_lvl_one,nxt_venue_cat_lvl_one) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  filter(Count >= supp_count_1) %>%
  top_n(15,-Count)

Sequence_chk_agregated_level_one_2 <- chk_poi_country %>%
  filter(!is.na(nxt_venue_cat_lvl_one),
         !( venue_category_lvl_one == nxt_venue_cat_lvl_one)) %>%
  group_by(venue_category_lvl_one,nxt_venue_cat_lvl_one) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  filter(Count >= supp_count_2) %>%
  top_n(10,-Count)

Sequence_chk_agregated_level_one <- rbind(Sequence_chk_agregated_level_one,Sequence_chk_agregated_level_one_1,Sequence_chk_agregated_level_one_2)

Sequence_chk_agregated_level_one$venue_category_lvl_one <- as.character(Sequence_chk_agregated_level_one$venue_category_lvl_one)
Sequence_chk_agregated_level_one$nxt_venue_cat_lvl_one <- as.character(Sequence_chk_agregated_level_one$nxt_venue_cat_lvl_one)

# create a list of unique activities and assign IDs to them (we combine both venue and next venue
# in case a venue was always last visited it will be then excluded)
activities_id_level_one <- (1:length(unique(c(Sequence_chk_agregated_level_one$venue_category_lvl_one, Sequence_chk_agregated_level_one$nxt_venue_cat_lvl_one)))) - 1
# set the names value of IDs to be the name of the venue
names(activities_id_level_one) <- unique(c(Sequence_chk_agregated_level_one$venue_category_lvl_one, Sequence_chk_agregated_level_one$nxt_venue_cat_lvl_one))

# add column source and target to 
Sequence_chk_agregated_level_one$source <- activities_id_level_one[Sequence_chk_agregated_level_one$venue_category_lvl_one]
Sequence_chk_agregated_level_one$target <- activities_id_level_one[Sequence_chk_agregated_level_one$nxt_venue_cat_lvl_one]

nodes_level_one <- data.frame(node=activities_id_level_one, name=names(activities_id_level_one))
links_level_one <- data.frame(source=Sequence_chk_agregated_level_one$source, target=Sequence_chk_agregated_level_one$target, value=Sequence_chk_agregated_level_one$Count)


networkD3::sankeyNetwork(Links = links_level_one, Nodes = nodes_level_one, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'count', 
                         nodePadding = 5,
                         width = 1500,
                         height = 1000,
                         fontSize = 20)

# Sequence_chk_agregated <- chk_poi_country %>%
#   filter(!is.na(nxt_venue_cat),
#          !(venue_category == nxt_venue_cat)) %>%
#           #& date(Chk_time) == date(nxt_chk_time))) %>%
#   group_by(venue_category,nxt_venue_cat) %>%
#   summarize(Count = n()) %>%
#   ungroup() %>%
#   top_n(-20,Count)
# 
# Sequence_chk_agregated$venue_category <- as.character(Sequence_chk_agregated$venue_category)
# Sequence_chk_agregated$nxt_venue_cat <- as.character(Sequence_chk_agregated$nxt_venue_cat)
# 
# # create a list of unique activities and assign IDs to them (we combine both venue and next venue
# # in case a venue was always last visited it will be then excluded)
# activities_id <- (1:length(unique(c(Sequence_chk_agregated$venue_category, Sequence_chk_agregated$nxt_venue_cat)))) - 1
# # set the names value of IDs to be the name of the venue
# names(activities_id) <- unique(c(Sequence_chk_agregated$venue_category, Sequence_chk_agregated$nxt_venue_cat))
# 
# # add column source and target to 
# Sequence_chk_agregated$source <- activities_id[Sequence_chk_agregated$venue_category]
# Sequence_chk_agregated$target <- activities_id[Sequence_chk_agregated$nxt_venue_cat]
# 
# nodes <- data.frame(node=activities_id, name=names(activities_id))
# links <- data.frame(source=Sequence_chk_agregated$source, target=Sequence_chk_agregated$target, value=Sequence_chk_agregated$Count)
# 
# 
# networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
#                          Source = 'source', 
#                          Target = 'target', 
#                          Value = 'value', 
#                          NodeID = 'name',
#                          units = 'count', 
#                          nodePadding = 5,
#                          width = 1500,
#                          height = 1000,
#                          fontSize = 20)

# IC_distance vs IC_Time curve
# IC_Time_vs_Dist <- chk_poi_country %>%
#   filter(IC_Time <= 1200 & IC_Dist <= 80 ) %>% 
#   group_by(User_ID, date(Chk_time)) %>% 
#   summarise(median_IC_Dist = median(IC_Dist, na.rm=T), median_IC_Time = median(IC_Time, na.rm=T)) %>%
#   ungroup() %>% 
#   group_by(User_ID) %>% 
#   summarise(median_IC_Dist = median(median_IC_Dist, na.rm=T), median_IC_Time = median(median_IC_Time, na.rm=T)) %>%
#   #filter(median_IC_Time <= 1200 & median_IC_Dist <= 80 ) %>%
#   ggplot(aes(x=median_IC_Dist, y= median_IC_Time)) + geom_point()



##################### Spatial Clustering Analysis #####################

# checkins data of the needed country
JP_chk <- chk_poi %>%
  filter(Country_Code == 'JP')%>%
  collect()

dbs<-JP_chk %>% filter(State == 'Hiroshima') %>% select (Venue_Category_Name,Venue_ID,Latitude,Longitude)
dbs<-unique(dbs)

kNNdistplot(dbs[,3:4], k = 5 ) 
abline(h = 0.016, lty = 2)
clusters <- dbscan(select(dbs, 'Latitude', 'Longitude'), eps = 0.016)
dbs$cluster <- clusters$cluster

groups  <- dbs %>% filter(cluster != 0)
groups2 <- groups
noise  <- dbs %>% filter(cluster == 0)

dbscan_plot<- fviz_cluster(clusters, data = dbs[,3:4], stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic(),main = 'DBSCAN plot')
#map
pal <- colorFactor(palette = 'Dark2',domain = groups$cluster)
dbscan_map<- leaflet() %>% addTiles() %>%addCircleMarkers(data = groups,lng = groups$Longitude, lat = groups$Latitude, weight = 1, color = "black", fillColor = ~pal(groups$cluster),fillOpacity = 1,radius = 5)

#kmeans
groups_km<-groups2
km <- kmeans(groups_km[,3:4],centers = 3, nstart = 5)
groups_km$cluster <-km$cluster
pal2 <- colorFactor(
  palette = 'Dark2',
  domain = groups_km$cluster
)

k_means_map<- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = groups_km,lng = groups_km$Longitude, lat = groups_km$Latitude, weight = 1,
                   color = "black",
                   fillColor = ~pal2(groups_km$cluster),
                   fillOpacity = 1,
                   radius = 5
  ) %>%
  addCircleMarkers(data = groups_km ,lng = km$centers[,2], lat = km$centers[,1],weight = 1,
                   color = "black", fillColor = "grey",fillOpacity = 0.2,
                   radius = ~sqrt(km$size)*4)

k_means_plot<- fviz_cluster(km, data = groups_km[,3:4], stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic(),main = 'k-means plot')

# 
# country_venues <- poi %>% filter(Country_Code == 'US') %>% collect()
# country_venues <- country_venues %>% mutate(city = find_pref(as.numeric(Longitude),as.numeric(Latitude))) %>% head(n = 10000)
# 
# chk_poi_country %>%
#   head() %>%
#   mutate(city = find_pref(as.numeric(Longitude),as.numeric(Latitude))) %>% 
#   group_by(city) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count))
# 
# suppressWarnings(warning("testit"))
# 
# # Map for the places of 
# x <- chk_poi_country %>%
#   group_by(Longitude,Latitude) %>%
#   summarize(count = n()) %>%
#   arrange(desc(count))
# 
# 
# m <- leaflet(x) %>%
#   addTiles() %>%
#   addMarkers(lat = ~ Latitude, lng = ~ Longitude, clusterOptions = markerClusterOptions())
#addCircles(lat = ~ Latitude, lng = ~ Longitude)

#x <- chk_poi_country %>% head(1000) %>%mutate(City_Name = find_pref(Longitude, Latitude)) %>% group_by(City_Name) %>% summarize(count = n()) %>% arrange(desc(count))

# joining t
#x <- inner_join (x = x, y= city, by= 'City_Name')

# p <- x %>%
#   plotmapbox(lat = ~Latitude, lon = ~Longitude,
#              size=~count,
#              mode = 'scattermapbox', hoverinfo='name') %>%
#   layout(title = 'Meteorites by Class',
#          font = list(color='white'),
#          plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
#          mapbox = list(style = 'dark'),
#          legend = list(orientation = 'h',
#                        font = list(size = 8)),
#          margin = list(l = 25, r = 25,
#                        b = 25, t = 25,
#                        pad = 2))
# 
# p <- ggplotly(ggplot(x))
# 
# p
# x
# nrow(x)
# 
# # compute the bounding box
# bc_bbox <- make_bbox(lat = Latitude, lon = Longitude, data = x)
# get_map(bc_bbox,source = "osm")


##################### Temporal clustering Analysis #####################
#select the TOP N categories
Top_cats_JP <- JP_chk %>%
  count(Venue_Category_Name, sort = TRUE) %>%
  head(n = 30)

#grouping by category & hour of the checkins (instead of hour another time level can be chosen)
time_cat_group<-JP_chk %>%
  filter(JP_chk$Venue_Category_Name %in% Top_cats_JP$Venue_Category_Name) %>%
  count(Venue_Category_Name,time=hour(Chk_time))

pivot_time_cat <- spread(time_cat_group,key='time',value='n')

#set the category as index
pivot_time_cat_c <- pivot_time_cat
rownames(pivot_time_cat) <- pivot_time_cat$Venue_Category_Name
#column_to_rownames(pivot_time_cat,Venue_Category_Name)
pivot_time_cat$Venue_Category_Name<-NULL

#assign 0 to value of checkins which are NULL
pivot_time_cat[is.na(pivot_time_cat)]<-0

#change the dataframe to matrix
time_cat_matrix<-as.matrix(pivot_time_cat)

"scale the matrix based on rows - standardazation
The values in each row are standardized by subtracting their minimum and dividing by their range
scale data to mean=0, sd=1 and convert to matrix"
time_cat_matrix_scaled <- t(scale(t(time_cat_matrix))) ## Centers and scales matrix row-wise
apply(time_cat_matrix_scaled, 1, sd) #to check sd of rows which should be 1
#heatmap
palette <- colorRampPalette(c('#f0f3ff','#0033BB'))(256)
heatmap(time_cat_matrix_scaled,Colv=NA, scale='none',col=palette,xlab = "Hours", ylab = "Category")

#Correlation-based distance matrix
time_cat_cor <- cor(t(time_cat_matrix_scaled), method="pearson")
time_cat_cor_dist <- as.dist(1-time_cat_cor)
#check data
#as.matrix(time_cat_cor)[1:4,1:4]

#euclidean distance
time_cat_eucl <- dist(time_cat_matrix_scaled, method = "euclidean")
#round(as.matrix(time_cat_eucl)[1:3, 1:3], 1)
#visualization-needed library: factoextra
fviz_dist(time_cat_eucl, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

"validity test- validating clustering results and comparing clustering algorithms
Partitioning algorithms: kmeans, PAM (Partitioning Around Medoids)
Hierarchical algorithms: agglomerative clustering
needed library: library(clValid))"
valid_test<-clValid(time_cat_matrix_scaled,nClust = 2:10,clMethods = c("hierarchical","kmeans","pam"),validation =c("internal","stability"))
summary(valid_test)


#based on test result we will choose between hierarchical and kmeans

####k-means:
#calculating number of clusters,using following statistics

# Elbow method
fviz_nbclust(time_cat_matrix_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(time_cat_matrix_scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123)
fviz_nbclust(time_cat_matrix_scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

"choosing between possible values for K - we focus on silhouette but any other
validation measures can be chosen as well"
km.5 <- eclust(time_cat_matrix_scaled, "kmeans", k = 5, nstart = 25, graph = FALSE)
km.4 <- eclust(time_cat_matrix_scaled, "kmeans", k = 4, nstart = 25, graph = FALSE)
fviz_silhouette(km.4, palette = "jco", ggtheme = theme_classic())
fviz_silhouette(km.5, palette = "jco", ggtheme = theme_classic())
set.seed(123)
time_cat_kmeans<- kmeans(time_cat_matrix_scaled, 5, nstart = 25)

time_cat_group_clus<-left_join(time_cat_group,select(cbind(pivot_time_cat_c, cluster = time_cat_kmeans$cluster),Venue_Category_Name,cluster),by='Venue_Category_Name')
#visualize clusters in timeseries format
kmeans_ts<-ggplot(time_cat_group_clus , aes(x=time, y=n, colour=Venue_Category_Name)) +geom_line() +scale_x_continuous(name="Hours of the day", limits=c(0, 24), breaks = c(0:24)) + ggtitle("K-means based clusters")
kmeans_ts+facet_grid(cluster ~ .,scales = "free_y")

#plot clusters based on first two principal components coordinates
fviz_cluster(time_cat_kmeans, data=time_cat_matrix_scaled,geom = c("point","text"))

####hierarchical clustering
#clustering methods comparision
time_cat_hclust_complete <- hclust(time_cat_cor_dist, method = "complete", members=NULL)
time_cat_hclust_single <- hclust(time_cat_cor_dist, method = "single", members=NULL)
time_cat_hclust_average <- hclust(time_cat_cor_dist, method = "average", members=NULL)
time_cat_hclust_ward <- hclust(time_cat_cor_dist, method = "ward.D2", members=NULL)
#values above 0.75 are considered good - in this case avg was the best
#number of clusters, change diss measure based on similarity measure
coph_single<-cophenetic(time_cat_hclust_single)
cor(time_cat_cor_dist,coph_single)
coph_average<-cophenetic(time_cat_hclust_average)
cor(time_cat_cor_dist,coph_average)
coph_complete<-cophenetic(time_cat_hclust_complete)
cor(time_cat_cor_dist,coph_complete)
coph_ward<-cophenetic(time_cat_hclust_ward)
cor(time_cat_cor_dist,coph_ward)

#choosing the cluster number based on silhouette
fviz_nbclust(time_cat_matrix_scaled, hcut, method = "silhouette") + theme_classic()
k_hclust<-5
#res<-NbClust(time_cat_matrix_scaled, diss=dist.eucl, distance = NULL, min.nc=2, max.nc=8,method = "average", index = "ch")
#cluster plot
fviz_cluster(list(data = time_cat_matrix_scaled, cluster = cutree(time_cat_hclust_average,k_hclust)))
#cluster dendogram - Cut tree into k groups
fviz_dend(time_cat_hclust_average, cex = 0.5, k = k_hclust, color_labels_by_k = TRUE,rect=TRUE, hang = -1)

#details
time_cat_hclust=cutree(time_cat_hclust_average,k_hclust)
table(time_cat_hclust)
unique(time_cat_hclust)
sapply(unique(time_cat_hclust),function(g)pivot_time_cat_c$Venue_Category_Name[time_cat_hclust == g])

#visualize clusters in timeseries format
time_cat_group_clus<-left_join(time_cat_group,select(cbind(pivot_time_cat_c, cluster = time_cat_hclust),Venue_Category_Name,cluster),by='Venue_Category_Name')

#visualizing clusters
hclust_ts<-ggplot(time_cat_group_clus , aes(x=time, y=n, colour=Venue_Category_Name)) +geom_line() +scale_x_continuous(name="Hours of the day", limits=c(0, 24), breaks = c(0:24)) + ggtitle("H_Clust based clusters")
hclust_ts+facet_grid(cluster ~ .,scales = "free_y")

