library(tidyverse)
library(magrittr)
library(ggthemes)
library(scales)


#projected high school graduates#

years <- 2001:2032

grand_totals <- c(2569200, 2621534, 2719947, 2759889, 2799250, 2813412, 2893045, 3001337, 3039015, 3128022, 3144100, 3149185, 3169257, 3152269, 3135026, 3138860, 3118893, 3198033, 3203028, 3166150, 3184963, 3195867, 3216522, 3270509, 3317313, 3280993, 3190662, 3086407, 3047076, 3047544, 3033544, 3076509)


byrace <- data.frame(
             hispanic_any_race = c(296776, 314122, 338416, 359401, 380736, 387257,
                                    404958, 449346, 481698, 545518, 583907,
                                    608726, 640413, 654254, 672840, 691661, 698626,
                                    739495, 767254, 777906, 800815, 822484,
                                    856276, 894471, 917776, 902729, 865793, 818084,
                                    794047, 785378, 779977, 791157),
                          white = c(1782495, 1800226, 1855842, 1856119, 1851095, 1852128,
                                    1871929, 1902881, 1889673, 1884694,
                                    1873458, 1850484, 1838951, 1807917, 1776322,
                                    1773222, 1757322, 1769885, 1754284, 1720563,
                                    1724512, 1719195, 1704187, 1711952, 1724972,
                                    1690414, 1648372, 1609589, 1584150, 1575820,
                                    1572108, 1586896),
                          african_american = c(336176, 345430, 358387, 371972, 384728, 391122,
                                    408750, 431944, 452820, 475306, 480976,
                                    478929, 474247, 466231, 464405, 459572, 450543,
                                    465040, 461072, 450629, 441955, 438763,
                                    445157, 457765, 471323, 472457, 460941, 445577,
                                    434638, 436117, 436682, 440374),
                american_indian = c(26138, 26901, 27391, 28331, 30456, 29185, 30598,
                                    32062, 32357, 34352, 33444, 33224, 31947,
                                    30877, 30099, 30167, 29300, 29050, 28139,
                                    27458, 26662, 26268, 25878, 25711, 25399, 26744,
                                    26106, 24817, 23785, 23618, 23481, 22860),
                        asian_pacific_islander = c(126852, 132043, 135096, 137812, 142555, 150747,
                                    153826, 159646, 167392, 168951, 172300,
                                    177804, 183686, 184913, 185255, 183077, 183383,
                                    196796, 196622, 198353, 206196, 209399,
                                    208632, 207925, 209494, 220736, 219501, 214918,
                                    215234, 230599, 226830, 241214)
          )

midwest <- c(696343, 704729, 726939, 734257, 726502, 733592, 753435, 772095, 767652, 776820, 768067, 765972, 762280, 743597, 739674, 734066, 726056, 738805, 735338, 721119, 719086, 723437, 716335, 724826, 732563, 719371, 701232, 678668, 671060, 669611, 667192, 672917)

northeast <- c(536680, 544118, 563470, 576523, 586806, 605543, 622114, 639941, 641902, 647036, 640631, 640417, 638882, 630159, 622380, 611531, 603414, 610619, 605739, 596839, 600008, 598593, 593303, 603739, 612637, 598047, 587408, 574727, 570654, 566561, 558830, 562466)

west <- c(666730, 685038, 707835, 710628, 736341, 719433, 737622, 769867, 772322, 813358, 820323, 827781, 830996, 831548, 819994, 816451, 808871, 825595, 824370, 819514, 830692, 833075, 840180, 862031, 855852, 857361, 824051, 795663, 782761, 785135, 777687, 789092)

south <- c(950253, 976790, 1020990, 1038523, 1045769, 1056943, 1082933, 1133534, 1166072, 1203477, 1217247, 1218627, 1234777, 1236737, 1238679, 1249349, 1244581, 1281267, 1286103, 1266957, 1266869, 1264926, 1280329, 1314251, 1352638, 1337391, 1302059, 1254353, 1238781, 1241920, 1244006, 1268731)


#BUILD DATA FRAME#

d <- data.frame(years,grand_totals,byrace,midwest,northeast,west,south)
str(d)

d %<>% filter(., years >= 2018)

#HS GRADS TOTAL

p <- ggplot(data = d,aes(x = years,y = grand_totals)) +
        geom_line(size=2) +
        scale_y_continuous(labels = comma) +
        labs(title = "Projected Public High School Graduates in U.S.") +
        expand_limits(y = c(3000000, 3400000)) +
        ggthemes::theme_fivethirtyeight()
        

p

filename <- "hsgrads"

ggsave(p,filename = paste0("images/",filename,Sys.Date(),".png"),width = 10.67,height = 4.5,units = "in",dpi = 300)


##HS GRADS BY REGION##

e <- d %>%
        select(years,midwest:south) %>%
        pivot_longer(-years, names_to = "race_ethnicity", values_to = "count") %>%
        filter(.,race_ethnicity != "grand_totals") %>%
        group_by(years) %>%
        mutate(total_grads = sum(count)) %>%
        group_by(race_ethnicity) %>%
        mutate(pct_of_total = count/total_grads) %>%
        ungroup() %>%
        group_by(race_ethnicity) %>%
        arrange(race_ethnicity,years) %>%
        mutate(pct_change = pct_of_total - lag(pct_of_total,n = 3)) %>%
        as.data.frame()

head(e)

p <- ggplot(data = e,aes(x = years,y = pct_of_total)) +
        geom_line(size=2) +
        labs(title = "Projected Changes in Proportion of  Public H.S. Grads in U.S.",
             subtitle = "by Region (Floating Y Axis)") +
        facet_wrap(race_ethnicity ~ .,scales = "free_y") +
        #facet_wrap(race_ethnicity ~ .) +
        ggthemes::theme_fivethirtyeight()

p

filename <- "hsgradsbyregion"

ggsave(p,filename = paste0("images/",filename,Sys.Date(),".png"),width = 10.67,height = 4.5,units = "in",dpi = 300)


####HS GRADS BY ETHNICITY IN SOUTH





# e <- d %>%
#         select(years:asian_pacific_islander) %>%
#         pivot_longer(-years, names_to = "race_ethnicity", values_to = "count") %>%
#         filter(.,race_ethnicity != "grand_totals") %>%
#         group_by(years) %>%
#         mutate(total_grads = sum(count)) %>%
#         group_by(race_ethnicity) %>%
#         mutate(pct_of_total = count/total_grads) %>%
#         ungroup() %>%
#         group_by(race_ethnicity) %>%
#         arrange(race_ethnicity,years) %>%
#         mutate(pct_change = pct_of_total - lag(pct_of_total,n = 3)) %>%
#         as.data.frame()
# 
# 
# head(e)
# 
# p <- ggplot(data = e,aes(x = years,y = pct_of_total)) +
#         geom_line(size=2) +
#         labs(title = "Projected Changes in Proportion of H.S. Grads in U.S.",
#              subtitle = "by Race/Ethnicity (Floating Y Axis)") +
#         facet_wrap(race_ethnicity ~ .,scales = "free_y") +
#         #facet_wrap(race_ethnicity ~ .) +
#         ggthemes::theme_fivethirtyeight()
# 
# p

# filename <- "hsgradsbyethnicity"
# 
# ggsave(p,filename = paste0("images/",filename,Sys.Date(),".png"),width = 10.67,height = 4.5,units = "in",dpi = 300)


####HS GRADS BY ETHNICITY SOUTH ONLY

years <- years

grand_totals <- c(950253, 976790, 1020990, 1038523, 1045769, 1056943, 1082933, 1133534, 1166072, 1203477, 1217247, 1218627, 1234777, 1236737, 1238679, 1249349, 1244581, 1281267, 1286103, 1266957, 1266869, 1264926, 1280329, 1314251, 1352638, 1337391, 1302059, 1254353, 1238781, 1241920, 1244006, 1268731)

byrace <- data.frame(hispanic_any_race = c(98428, 106694, 116854, 124874, 128004,
                                                   131714, 136956, 152270,
                                                   168553, 191575, 206058, 212196,
                                                   229230, 234794, 245202, 258701,
                                                   263275, 281405, 296515,
                                                   299947, 310780, 319002, 333274,
                                                   348464, 365816, 362461, 351316,
                                                   328598, 317117, 315651, 319399,
                                                   327985),
                                         white = c(542065, 549628, 571826, 572648,
                                                   570044, 570855, 577583,
                                                   590795, 593246, 593970, 594284,
                                                   590790, 590382, 589323, 584080,
                                                   584743, 582762, 590653,
                                                   586555, 575471, 573796, 571209,
                                                   572050, 574261, 584588, 573439,
                                                   558570, 544418, 540175, 540898,
                                                   539900, 548971),
                     african_american = c(193932, 200598, 205972, 211915,
                                                   216100, 216521, 223866,
                                                   238441, 252630, 264353, 270775,
                                                   268904, 267961, 265319, 264988,
                                                   265552, 261106, 271403,
                                                   270079, 264649, 259085, 256582,
                                                   262304, 270780, 281785, 281018,
                                                   273031, 263032, 257109, 258136,
                                                   259052, 262315),
                     american_indian = c(8865, 8966, 9322, 9706, 10168, 10069,
                                                   10343, 10820, 11413, 12401,
                                                   12395, 12185, 12138, 11552,
                                                   11367, 11587, 11163, 11130,
                                                   10549, 10166, 9717, 9294, 9057,
                                                   9071, 8764, 9536, 9352, 9089,
                                                   8553, 8515, 8508, 8405),
                     asian_pacific_islander = c(23267, 24538, 25756, 26511, 27642,
                                                   29411, 30938, 31995, 33435,
                                                   35439, 35886, 37307, 39242,
                                                   41181, 42220, 42595, 43103,
                                                   46950, 48954, 49763, 52521, 54235,
                                                   55265, 56155, 58079, 60758,
                                                   62032, 61368, 62841, 66724,
                                                   65485, 70628)
          )


f <- data.frame(years,grand_totals,byrace)
str(f)

g <- f %>%
        #select(years:asian_pacific_islander) %>%
        pivot_longer(-years, names_to = "race_ethnicity", values_to = "count") %>%
        group_by(years) %>%
        mutate(total_grads = count[race_ethnicity =="grand_totals"]) %>%
        filter(.,race_ethnicity != "grand_totals") %>%
        #group_by(years,race_ethnicity) %>%
        mutate(pct_of_total = count/total_grads) %>%
        group_by(race_ethnicity) %>%
        arrange(race_ethnicity,years) %>%
        mutate(pct_change = pct_of_total - lag(pct_of_total,n = 3)) %>%
        as.data.frame(.)
        


head(g)

p <- ggplot(data = g,aes(x = years,y = pct_of_total)) +
        geom_line(size=2) +
        labs(title = "Projected Changes in Proportion of Public H.S. Grads in South",
             subtitle = "by Race/Ethnicity (Floating Y Axis)") +
        facet_wrap(race_ethnicity ~ .,scales = "free_y") +
        #facet_wrap(race_ethnicity ~ .) +
        ggthemes::theme_fivethirtyeight()

p

filename <- "hsgradsbyethnicitysouth"

ggsave(p,filename = paste0("images/",filename,Sys.Date(),".png"),width = 10.67,height = 4.5,units = "in",dpi = 300)



##IMPORT DATA FROM IPEDS

ipeds <- read.csv("Data_1-9-2020.csv",stringsAsFactors = F)
library(janitor)

ipeds %<>% janitor::clean_names(.,case = c("snake"))
head(ipeds)

i <- select(ipeds,
            unit_id,
            institution_name,
            grand_total_ef2017a_rv_full_time_students_undergraduate_degree_certificate_seeking_first_time,
            hispanic_total_ef2017a_rv_full_time_students_undergraduate_degree_certificate_seeking_first_time,
            3:7)

newnames <- c("unitid",
              "institution_name",
              "ftft_2017",
              "hispanic_ftft_2017",
              "average_net_price_0_30k",
              "average_net_price_30_48k",
              "average_net_price_48_75k",
              "average_net_price_75_110k",
              "average_net_price_over_100K")

length(newnames)==length(i)

names(i) <- newnames

summary(i)

i %<>% mutate(pct_hispanic = hispanic_ftft_2017/ftft_2017) %>%
        as.data.frame(.)

summary(i)

##create pseudonymns

data("crime")

streets <- crime$street
set.seed(5280)

streets <- sample(unique(streets),size = nrow(i),replace = F)

length(unique(streets))
duplicated(streets)

i$pseudonymn <- streets

head(i)

#add faux state line#

fs <- c(999999,"Faux State University",1,1,5500,7000,11000,17000,185000,.011,"faux state")

#bind to i

i <- rbind(i,fs)

#convert column

i$pct_hispanic %<>% as.numeric(.)

#create highlight column

i$tohighlight <- "no"
i$tohighlight[i$pseudonymn == "faux state"] <- "yes"

##BARCHART OF PERCENTAGE OF HISPANIC STUDENTS##

library(LaCroixColoR)
library(forcats)

colpal <- lacroix_palettes$KeyLime

p <- ggplot(i,aes(x=fct_reorder(.f = pseudonymn,.x = pct_hispanic,.desc = F), y= pct_hispanic,fill=tohighlight)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values = c("yes" = colpal[1], "no" =  colpal[2]),guide = F) +
        coord_flip() +
        ggthemes::theme_fivethirtyeight() +
        labs(title = "Pct Hispanic (Any Race) Students at IHE in Kentucky",
             subtitle = "Pseudonymns Randomly Selected") 
        

p


filename <- "coakentucky"

ggsave(p,filename = paste0("images/",filename,Sys.Date(),".png"),width = 10.67,height = 4.5,units = "in",dpi = 300)


##COA

coa <- c(11550, 14886, 29500, 13295, 40750, 25760, 21550, 25400, 40500, 9020, 15000, 9366, NA, 37160, 19256, 7760, 8184, 25070, 24246, 23950, 8950, 8820, 9744, 5310, 24000, 25950, 30270, 37290, 26080, 11942, 11264, 20338, 23000, 10202,39255)

i$coa <- coa

tail(i)

##BARCHART OF COA##

library(LaCroixColoR)
library(forcats)

colpal <- lacroix_palettes$KeyLime

p <- ggplot(i,aes(x=fct_reorder(.f = pseudonymn,.x = coa,.desc = F), y= coa,fill=tohighlight)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values = c("yes" = colpal[5], "no" =  colpal[6]),guide = F) +
        coord_flip() +
        ggthemes::theme_fivethirtyeight() +
        labs(title = "Cost of Attendance for KY Institutions 2017-2018",
             subtitle = "Pseudonymns Randomly Selected") 


p


filename <- "coaky"

ggsave(p,filename = paste0("images/",filename,Sys.Date(),".png"),width = 10.67,height = 4.5,units = "in",dpi = 300)

# install.packages("ggmap")
# library(ggmap)
# 
# #KY	Kentucky	-89.571509	36.497129	-81.964971	39.147458
# 
# ky <- c(left = 	-89.571509,
#             bottom = 36.497129,
#             right = -81.964971,
#             top = 39.147458)
# 
# 
# ky_stamen <- ggmap::get_stamenmap(bbox = ky,
#                                 zoom = 11)
# 
# ggmap(ky_stamen)
# 
# names(ipeds)
