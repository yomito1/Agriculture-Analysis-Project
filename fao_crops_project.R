#analysis and prediction of gross agriculture product

#load libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(ggplot2)
library(stringr)
library(scales)



crops_data <- read.csv(file.choose(), header=T) %>% as_tibble()

fertilizer_data <- read.csv(file.choose(), header=T) %>% as_tibble()

forest_data <- read.csv(file.choose(), header=T) %>% as_tibble()


land_data <-  read.csv(file.choose(), header=T) %>% as_tibble()

production_data <- read.csv(file.choose(), header=T) %>% as_tibble()

levels(as.factor(production_data$unit))


#data manipulation

tail(production_data)

agric_production_data<- production_data %>% filter (element_code != "Footnote")  %>% 
  rename(
    Region     = country_or_area,
    Production = element,
    Year       = year,
    Unit       = unit,
    Price      = value,
    Category   = category
    ) %>%
  select("Region", "Production", "Year", "Unit", "Price","Category")


glimpse(agric_production_data)


agric_production_data_formatted <- agric_production_data%>%
  filter(!is.na(Price)) %>%
  mutate(Region     = as_factor(Region),
         Year       = as.Date(as.character(Year), format = "%Y"),
         #Year       = strptime(Year, format = "%Y"),
         Category   = as_factor(Category),
         Production = case_when(Production =="Gross Production 1999-2001 (1000 I$)" ~ "Gross Production",
                                Production =="Net Production 1999-2001 (1000 I$)" ~ "Net Production",
                                Production == "Gross PIN (base 1999-2001)" ~ "Gross PIN",
                                Production == "Grs per capita PIN (base 1999-2001)" ~ "Grs per capita PIN",
                                Production == "Net PIN (base 1999-2001)" ~ "Net PIN",
                                Production == "Net per capita PIN (base 1999-2001)" ~ "Net per capita PIN",
                                TRUE ~ Production),
         Production = as_factor(Production)
         )


glimpse(agric_production_data_formatted )
summary(agric_production_data_formatted)

gross_agric_prod <- agric_production_data_formatted %>%
  filter(Production == "Gross Production") %>%
  select("Region", "Year", "Price") %>%
  arrange(Year) %>%
  #mutate(price = Price *1000) %>%
 
  # pivot_wider(names_from   = Year,
  #              values_from = Total_Gross,
  #              values_fn   = list)%>%
  # unnest(cols = everything()) %>%
  drop_na() %>%                     #drop na values
  filter(!str_detect(Region, ' +'))  #drop regions with '+' to avoid double dipping in the data


  
# generate list of top 10 countries with highest gross agric production
 gross_agric_prod_top_10 <- gross_agric_prod  %>% 
   group_by(Region) %>%
   summarise(Gross_Prod   = sum(Price)) %>%
   arrange(desc(Gross_Prod)) %>%
   select(-Gross_Prod) %>%
   slice(1:10) %>%
   left_join(gross_agric_prod)
 
  #top_10 <- left_join(gross_agric_prod_top_10, gross_agric_prod)
 
   
 levels(gross_agric_prod_top_10$Region)
 summary(gross_agric_prod_top_10)
 glimpse(gross_agric_prod_top_10)
 
 #visualization

  
#GAP Chart for the top 10 countries
gross_agric_prod_top_10 %>%
  ggplot(aes(x = Year, y = Price, group=Region, color = Region, fill = Region)) +
  geom_area() +
  #geom_line() +
  scale_colour_discrete(name='Region') +
  scale_fill_discrete(name='Region') +
  #scale_y_continuous() +
  #scale_y_continuous(labels = comma) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  theme_tq()+
  labs(
    title = "Gross Agricultural Producing Top 10 Countries from 1961 - 2007",
    y     = "Gross Agricultural Product ('000)"
  ) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

  
#Box plot of GAP for top 10 countries

boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",
        xlab="Number of Cylinders", ylab="Miles Per Gallon")


  boxplot(Price ~ Region,
          data = gross_agric_prod_top_10, 
          main = "Gross Argricultural Producing Top 10 Countries from 1961 - 2007",
          xlab = "Year",
          ylab = "Gross Agricultural Product ('000)")
  



         