

library(janitor)


dash.comp <- tbl(con,"DASH_ALL") %>%
    filter(# countyname == "Monterey",
            (charter_flag == "Y" ),
            indicator %in% c( "ELA","MATH"),
            studentgroup == "ALL",
            reportingyear == 2023
            #  rtype == "D"
    )  %>%
    head(3000) %>%
    collect () 

count.tab <- tabyl(dash.comp$countyname)

comp.counties <- c("Madera", "Solano", "Ventura", "Monterey")


enr <- tbl(con,"CENROLLMENT") %>%
    filter(# countyname == "Monterey",
        charter == "Yes" ,
        academic_year == "2022-23",
   #     county_name %in% comp.counties
        # indicator %in% c( "ELA","MATH"),
        # studentgroup == "ALL",
        # reportingyear == 2023
        #  rtype == "D"
    )  %>%
 #   head(3000) %>%
    collect () 


enr2 <- enr %>% 
    pivot_wider(names_from = reporting_category, values_from = cumulative_enrollment)





census <- tbl(con,"CENSUS") %>%
    filter(# countyname == "Monterey",
         charter == "Y" ,
        # academic_year == "2022-23",
        #     county_name %in% comp.counties
        # indicator %in% c( "ELA","MATH"),
        # studentgroup == "ALL",
        # reportingyear == 2023
        #  rtype == "D"
    )  %>%
 #      head(3000) %>%
    collect () 

census2 <- census %>% 
    select(aggregate_level, county_name, district_name, school_name, reporting_category, ends_with("code")  ,total_enr) %>%
    pivot_wider(names_from = reporting_category, values_from = total_enr) %>%
    mutate(EL_perc = SG_EL/TA,
           SD_perc = SG_SD/TA,
           DS_perc = SG_DS/TA, 
           H_perc = RE_H/TA,
           TA)


census3 <- census2  %>%
    filter(TA < 6000,
           TA > 1000,
           SD_perc < .6,
           EL_perc > .09,
           DS_perc > .1,
           aggregate_level == "C")



census.comp.schools <- census2 %>%
    filter(county_name %in% c("Monterey","Solano","Ventura","Yolo"),
           aggregate_level %in% c("C", "S")
           )
