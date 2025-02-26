##########################################
#      TATA: ONLINE RETAIL DATASET       #
##########################################
#clearing R environment
rm(list = ls())

#reading in R libraries
library(tidyverse)
library(dplyr)
library(ggpubr) #used to output table
library(patchwork) #package to create layout of multiple figures on single page
library(scales) #package used to add commas to text in ggplot
library(maps) #package used for mapping results
library(mapproj) #package used for mapping results

#################################
#       DATA CLEANING           #
#################################

#reading in data via csv file
retail_data<-read.csv("/Users/josephrios/Desktop/Data Science Portfolio Projects/Retail Data/Online Retail Data.csv")
str(retail_data)
#converting time from character to timestamp
#! timing function is not appropriately working, so will use substr() function to extraction information by position
#retail_data$Date<-as.POSIXlt(retail_data$InvoiceDate,"%d-%m-%Y %HH:%MM",tz="America/Los_Angeles")
retail_data$Day<-as.numeric(substr(retail_data$InvoiceDate,1,2))
retail_data$Month<-as.numeric(substr(retail_data$InvoiceDate,4,5))
retail_data$Year<-as.numeric(substr(retail_data$InvoiceDate,7,10))
retail_data$Hour<-as.numeric(substr(retail_data$InvoiceDate,12,13))
retail_data$Date<-as.Date(paste0(retail_data$Day,"-",retail_data$Month,"-",retail_data$Year),format="%d-%m-%Y")
retail_data$Month_Year<-factor(format(retail_data$Date,"%m-%Y"),levels=c("12-2010","01-2011","02-2011","03-2011","04-2011","05-2011","06-2011","07-2011","08-2011","09-2011","10-2011","11-2011","12-2011")) #

#date descriptives
min_time<-min(retail_data$Date)
max_time<-max(retail_data$Date)
days_in_data<-as.numeric(difftime(max_time,min_time,units="auto")) #dataset consists of purchases over 373 days


#creating total revenue by stock code and invoice no
retail_data$total<-as.numeric(retail_data$Quantity*retail_data$UnitPrice)

#################################
# TOTAL REVENUE ACROSS 373 DAYS #
#################################
#total revenue
total_revenue<-sum(retail_data$total) #$9,747,748

#revenue by day
revenue_by_day<-sum(total_revenue)/days_in_data #$26,133.37


##########################################
#     TOTAL REVENUE BY MONTH             #
##########################################
monthly_revenue <- retail_data %>%
  group_by(Month_Year) %>%
  summarize(total_purchases=as.numeric(sum(Quantity,na.rm=TRUE))) %>%
  ungroup() #ungrouping data for next analysis


#plotting revenue by month
monthly_revenue_plot<-ggplot(monthly_revenue[-13,], aes(x = Month_Year,y = as.numeric(total_purchases))) + #dropping last month because it is not complete and will skew results
  geom_point(size=2.5,color="#00B3C1") + 
  geom_line(size=1,group=1,color="#003542") +
  geom_text(aes(label=paste0("$",comma(total_purchases))),hjust=.5,vjust=-2,size=2,color="black") +
  scale_x_discrete(labels = c("Dec 2023","Jan 2024", "Feb 2024", "Mar 2024", "Apr 2024","May 2024","Jun 2024","Jul 2024","Aug 2024","Sep 2024","Oct 2024","Nov 2024")) +
  scale_y_continuous(limits=c(200000,800000),breaks = c(200000,250000, 300000,350000,400000,450000,500000,550000,600000,650000,700000,750000,800000),
                     labels = c("$200,000","$250,000", "$300,000","$350,000","$400,000","$450,000","$500,000","$550,000","$600,000","$650,000","$700,000","$750,000","$800,000")) +
  labs(title = "Monthly Revenue",
       x = "Month and Year",
       y = "Total Revenue") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=7.5,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering


#month-over-month revenue
month_over_month_revenue <- monthly_revenue %>%
  mutate(mom_change = round((total_purchases - lag(total_purchases))/lag(total_purchases)*100),2)  # Difference in time (just in case there are gaps)


#plotting revenue by month
mom_change_plot<-ggplot(month_over_month_revenue[-13,], aes(x = Month_Year,y = as.numeric(mom_change))) + #dropping last month because it is not complete and will skew results
  geom_point(size=2.5,color="#00B3C1") + 
  geom_line(size=1,group=1,color="#003542") +
  geom_text(aes(label=paste0(mom_change,"%")),hjust=.5,vjust=-2,size=2,color="black") +
  scale_x_discrete(labels = c("Dec 2023","Jan 2024", "Feb 2024", "Mar 2024", "Apr 2024","May 2024","Jun 2024","Jul 2024","Aug 2024","Sep 2024","Oct 2024","Nov 2024")) +
 scale_y_continuous(limits=c(-70,50),breaks = c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50),
                     labels = c("-70%","-60%","-50%","-40%","-30%","-20%","-10%","0%","10%","20%","30%","40%","50%")) +
  labs(title = "Month-Over-Month Revenue Change",
       x = "Month and Year",
       y = "Percent Change") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=7.5,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

combined_revenue_plot<-monthly_revenue_plot/mom_change_plot

# Save the plot as a high-quality PNG
png("/Users/josephrios/Desktop/Data Science Portfolio Projects/Retail Data/Plot - monthly revenue and mom change.png",
    width = 1600,      # Width in pixels
    height = 1600,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(combined_revenue_plot)        # Draw the plot
dev.off()             # Close the grap

##########################################
#.          REPEAT CUSTOMERS             #
##########################################
repeat_customers<-retail_data %>%
  group_by(CustomerID) %>%
  summarize(total_purchases=n_distinct(InvoiceNo),
            total_revenue=sum(total)) %>%
  mutate(repeat_customer=ifelse(total_purchases>1 & total_revenue>0,1,0)) %>%
  ungroup()

#percentage of repeat customers
100*(sum(repeat_customers$repeat_customer)/nrow(repeat_customers)) #69.42%

#median revenue across all customers
median_revenue_per_customer<-median(repeat_customers$total_revenue) 

#median revenue of repeat customers
repeat_customers2<-repeat_customers %>%
  filter(repeat_customer==1) %>%
  summarize(median_repeat_customer_revenue=median(total_revenue))

#median revenue of non-repeat customers
no_repeat_customers<-repeat_customers %>%
  filter(repeat_customer==0) %>%
  summarize(median_repeat_customer_revenue=median(total_revenue))











###########################################
#  WHAT ARE THE MOST POPULAR ITEMS SOLD  #
###########################################

total_items <- retail_data %>%
  mutate(total_sales = Quantity * UnitPrice) %>% # Calculate total sales first
  group_by(StockCode,Description) %>%
  summarize(total_purchases_by_stockcode=sum(Quantity,na.rm=TRUE),
            total_revenue = sum(total_sales, na.rm = TRUE)) %>%
  ungroup() #ungrouping data for next analysis

#rank order
rank_item_popularity <- total_items %>%
  mutate(most_purchased_rank = dense_rank(desc(as.numeric(total_purchases_by_stockcode))),
         highest_grossing_rank = dense_rank(desc(as.numeric(total_revenue))),
         least_purchased_rank = dense_rank(as.numeric(total_purchases_by_stockcode))) %>% #descending order
  arrange(highest_grossing_rank)

# Function to lowercase all but the first letter of each word
lower_case_except_first <- function(x) {
  str_replace_all(x, "\\b(\\w)(\\w*)", function(m) {
    paste0(str_to_upper(substr(m, 1, 1)), str_to_lower(substr(m, 2, nchar(m))))
  })
}

#top 10 most purchased items
top_10_purchased_items <- rank_item_popularity %>%
  filter(most_purchased_rank <=10) %>% select(Description,total_purchases_by_stockcode,total_revenue) %>%
  mutate(Description = lower_case_except_first(Description), total_purchases_by_stockcode=comma(total_purchases_by_stockcode),total_revenue=comma(total_revenue)) %>%
  mutate(total_revenue=paste0("$",total_revenue)) %>%
  rename('Item Description'= Description,'Total Purchases' = total_purchases_by_stockcode, 'Total Revenue' = total_revenue) 

table_most_purchased_items <- ggtexttable(top_10_purchased_items, 
                                          rows = NULL,
                                          theme = ttheme(padding = unit(c(15, 5), "mm"),
                                                         colnames.style=colnames_style(color="white",fill="#003542")
                                          ))  %>%
  tab_add_title(text = "Top 10 Most Purchased Items", face = "bold", padding = unit(0.5, "line"))

# Save the plot as a high-quality PNG
png("/Users/josephrios/Desktop/Data Science Portfolio Projects/Retail Data/Table - top 10 most purchased items.png",
    width = 2000,      # Width in pixels
    height = 1200,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(table_most_purchased_items)        # Draw the plot
dev.off()             # Close the graph


####################################
#  Top 10 Highest Grossing Items.  #
####################################

top_10_grossing_items <- rank_item_popularity %>%
  filter(highest_grossing_rank <=10) %>% select(Description,total_revenue,total_purchases_by_stockcode) %>%
  mutate(Description = lower_case_except_first(Description),total_revenue=comma(total_revenue), total_purchases_by_stockcode=comma(total_purchases_by_stockcode)) %>%
  mutate(total_revenue=paste0("$",total_revenue)) %>%
  rename('Item Description'= Description,'Total Revenue' = total_revenue,'Total Purchases' = total_purchases_by_stockcode) 



table_highest_grossing_items <- ggtexttable(top_10_grossing_items, 
                                        rows = NULL,
                                        theme = ttheme(padding = unit(c(15, 5), "mm"),
                                        colnames.style=colnames_style(color="white",fill="#003542")
                                        ))  %>%
  tab_add_title(text = "Top 10 Highest-Grossing Items", face = "bold", padding = unit(0.5, "line"))

# Save the plot as a high-quality PNG
png("/Users/josephrios/Desktop/Data Science Portfolio Projects/Retail Data/Table - top 10 highest grossing items.png",
    width = 2000,      # Width in pixels
    height = 1200,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(table_highest_grossing_items)        # Draw the plot
dev.off()             # Close the graph



#######################################
#       TOP REASONS FOR RETURNS       #
#######################################


#top 10 reasons for returns
top_10_reasons_returns <- rank_item_popularity %>%    arrange(least_purchased_rank) %>% filter(Description !="") %>%
  slice_head(n=10) %>% select(Description,total_purchases_by_stockcode) %>%
  mutate(Description = lower_case_except_first(Description), total_purchases_by_stockcode=comma(total_purchases_by_stockcode*-1)) %>%
  mutate(Description = str_replace_all(Description, "\\.", ""))  %>% #removing periods from descriptions
  rename('Item Description'= Description,'Total Returns' = total_purchases_by_stockcode) 

table_top_reasons_returns <- ggtexttable(top_10_least_purchased_items, 
                                          rows = NULL,
                                          theme = ttheme(padding = unit(c(15, 5), "mm"),
                                                         colnames.style=colnames_style(color="white",fill="#003542")
                                          ))  %>%
  tab_add_title(text = "Top 10 Reasons for Returned Items", face = "bold", padding = unit(0.5, "line")) %>%
  tab_add_footnote(text = "Note: Total returns are reflective of distinctive stock codes.", size = 10)
                

# Save the plot as a high-quality PNG
png("/Users/josephrios/Desktop/Data Science Portfolio Projects/Retail Data/Table - top 10 reasons for returns.png",
    width = 2000,      # Width in pixels
    height = 1200,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(table_top_reasons_returns)        # Draw the plot
dev.off()             # Close the graph




#########################################
#   top performing geographic areas     #
#########################################

#group by invoice and include country values
revenue_invoice_country <- retail_data %>%
  group_by(InvoiceNo,Country) %>%
  summarize(total_purchases=as.numeric(sum(total,na.rm=TRUE))) %>%
  ungroup() #ungrouping data for next analysis

#calculate total revenue by country
revenue_country <- revenue_invoice_country %>%
  group_by(Country) %>%
  summarize(total_revenue=as.numeric(sum(total_purchases,na.rm=TRUE)),
  .groups = "drop") %>%
  mutate(
    country_revenue_percent=as.numeric(format(round((total_revenue/sum(total_revenue))*100,1),nsmall=2)),
    Country= recode(Country, RSA="South Africa",EIRE="Ireland",'United Kingdom'="UK")
    ) 
    
#loading world map
world_map<-map_data("world")

#joining data with the map data
map_data<-left_join(world_map,revenue_country,by=c("region"="Country"))

#creating map
revenue_map<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = total_revenue)) +
  geom_polygon() +
  scale_fill_gradient(low = "lightblue", high = "#da4554", na.value = "gray90", breaks=c(2000000,4000000,6000000,8000000),
                      labels=c("$2,000,000","$4,000,000","$6,000,000","$8,000,000")) +  # Customize color scale
  coord_map("mercator") +  # Choose a map projection
  labs(title = "Total Revenue by Country", fill = "Total Revenue") +
  theme_void() + # Remove background elements
  theme(plot.title = element_text(face="bold", hjust=.5,size=14,vjust=3.5))
   

# Save the plot as a high-quality PNG
png("/Users/josephrios/Desktop/Data Science Portfolio Projects/Retail Data/Figure - map of highest-grossing countries.png",
    width = 2000,      # Width in pixels
    height = 1400,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(revenue_map)        # Draw the plot
dev.off()             # Close the graph



#creating histogram
country_revenue_plot<-ggplot(data = revenue_country, aes(x = Country,y=total_revenue)) + # Need to put data in a dataframe
  geom_col(fill="#003542",color="#003542") +
 geom_text(aes(label=paste0(country_revenue_percent,"%")),hjust=.5,vjust=-1.5,size=2,color="black") +
 
  scale_y_continuous(limits=c(0,8500000),breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000,4500000,5000000,5500000,6000000,6500000,7000000,7500000,8000000,8500000),
 labels= c("$0","$500,000","$1,000,000","$1,500,000","$2,000,000","$2,500,000","$3,000,000","$3,500,000","$4,000,000","$4,500,000","$5,000,000","$5,500,000","$6,000,000","$6,500,000","$7,000,000","$7,500,000","$8,000,000","$8,500,000")) +
  
   #                  labels = c("0","25,000","50,000","75,000","100,000","125,000","150,000")) + 
  labs(title = "Total Revenue by Country",
       x = "Country",
       y = "Total Revenue") + 
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10,angle=90),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))

# Save the plot as a high-quality PNG
png("/Users/josephrios/Desktop/Data Science Portfolio Projects/Retail Data/Figure - histogram of highest-grossing countries.png",
    width = 2400,      # Width in pixels
    height = 1800,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(country_revenue_plot)        # Draw the plot
dev.off()             # Close the graph


