
# https://docs.google.com/presentation/d/1UUjYqBhn2ORoO26vm6V1Y7m4OTYHJ_a563JVxwK7pa0/edit#slide=id.p


library(MCOE)
library(tidyverse)


con <- mcoe_sql_con()


elems <- c("Greenfield Union Elementary", 
           "Salinas City Elementary", 
           "Alisal Union", 
           "King City Union", 
           "Bradley Union Elementary", 
           "Lagunita Elementary", 
           "Mission Union Elementary", 
           "Chualar Union", 
           "Graves Elementary", 
           "San Antonio Union Elementary", 
           "San Ardo Union Elementary", 
           "San Lucas Union Elementary", 
           "Santa Rita Union Elementary", 
           "Spreckels Union Elementary", 
           "Washington Union Elementary")


caaspp <- tbl(con, "CAASPP") %>% 
    filter(County_Code == "27",
        # DistrictCode == "10272",
        Test_Year >= "2022",
        Subgroup_ID == 1,
        Grade == 13) %>%
    head(2000) %>%
    collect()





dash <- tbl(con,"DASH_ALL") %>%
    filter( countyname == "Monterey",
            (charter_flag == "Y" | rtype == "D"),
           indicator %in% c( "ELA","MATH"),
           studentgroup == "ALL"
           #  rtype == "D"
    )  %>%
    head(2000) %>%
    collect () 



dash2 <- dash %>% 
    mutate(reportingyear = as.factor(reportingyear)) %>%
    mutate(LEA = if_else(is.na(schoolname),districtname,schoolname)) %>%
    select(reportingyear, LEA, charter_flag, currstatus, indicator) %>%
    mutate(LEAlabel = if_else(reportingyear == 2023 & charter_flag == "Y", LEA, NA)) # To only labe lthe charters 



dash2 %>%
    filter(indicator == "MATH") %>% # Choose MATH or ELA
  #  filter(LEA %in% elems | charter_flag == "Y") %>% # To limit to only elementary schools for comparison and the charters 
    ggplot(aes(x = reportingyear,
               y = currstatus, 
               group = LEA, 
               color = charter_flag, 
               label = LEAlabel)) +
    geom_line(size = 1) +
    geom_label()





### Selecting Comp Counties --------

# TA < 6000,
# TA > 1000,
# SD_perc < .6,
# EL_perc > .09,


census.counties.graph <- census2  %>%
    filter(aggregate_level == "C") %>%
    mutate(county.color = if_else(county_name %in% c("Monterey","Solano","Ventura","Yolo" ), "red", "lightgrey")
    )


library(scales)

census.counties.graph %>%
    ggplot(aes(
        x = county_name,
           y = (TA),
           color =  county.color,
        label = county_name
           )) +
    geom_point(size = 3) + 
    geom_label() +
    scale_y_log10(10) +
    scale_color_identity() +
    mcoe_theme


census.counties.graph %>%
    ggplot(aes(
        x = county_name, # reorder(county_name, EL_perc),
        y = (SD_perc),
        color =  county.color,
        label = county_name
    )) +
    geom_point(size = 3) + 
    geom_label() +
#    scale_y_log10(10) +
    scale_color_identity() +
    mcoe_theme +
    theme(axis.title.x=element_blank(),        axis.text.x=element_blank(),    axis.ticks.x=element_blank())
#    scale_x_discrete(guide = guide_axis(angle = 30))
#    coord_flip()





### Comp Counties -------


dash.comp <- tbl(con,"DASH_ALL") %>%
    filter( countyname %in% c("Monterey","Solano","Ventura","Yolo" ),
            (charter_flag == "Y" | rtype == "D"),
            indicator %in% c( "ELA","MATH"),
            studentgroup == "ALL"
            #  rtype == "D"
    )  %>%
#    head(2000) %>%
    collect () 


dash.comp2 <- dash.comp %>% 
    mutate(reportingyear = as.factor(reportingyear)) %>%
    mutate(LEA = if_else(is.na(schoolname),districtname,schoolname)) %>%
    select(countyname, reportingyear, LEA, charter_flag, currstatus, indicator) %>%
    mutate(LEAlabel = if_else(reportingyear == 2023 & charter_flag == "Y", LEA, NA)) # To only labe lthe charters 



# Shows how charters compare to traditional LEA.  Big take away is that similar across counties. 

dash.comp2 %>%
    filter(indicator == "MATH") %>% # Choose MATH or ELA
    #  filter(LEA %in% elems | charter_flag == "Y") %>% # To limit to only elementary schools for comparison and the charters 
    ggplot(aes(x = reportingyear,
               y = currstatus, 
               group = LEA, 
               color = charter_flag, 
               label = LEAlabel)) +
    geom_line(size = 1) +
    facet_grid(~countyname) +
    mcoe_theme +
 #   geom_label() +
    labs(title = "Charter Schools Across Comparitive Counties")




# Show rank order by test scores.  Bid take away is it is a mix and our county looks like the comp counties 

dash.comp2 %>%
    filter(charter_flag == "Y",
        indicator == "MATH",
        reportingyear == 2023) %>% # Choose MATH or ELA
    #  filter(LEA %in% elems | charter_flag == "Y") %>% # To limit to only elementary schools for comparison and the charters 
    ggplot(aes(x = reorder(LEA,currstatus ),
               y = currstatus, 
               group = LEA, 
               fill = countyname, 
               label = LEAlabel)) +
    geom_col() +
    coord_flip() +
 #   geom_line(size = 1) +
  #  facet_grid(~countyname) +
    mcoe_theme +
    #   geom_label() +
    labs(title = "Charter Schools Across Comparitive Counties")



