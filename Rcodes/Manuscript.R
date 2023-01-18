# -----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Who has been saved more? Trends in gender differences in mortality
# through the revivorship approach.
# author: Vanessa di Lego
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# Vordere Zollamtstraße 3, 1030 Vienna, Austria
# -----------------------------------------------------------------------------------------------------#


# loading necessary packages

library(MortalityLaws)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(purrr)
library(styler)
library(forcats)
library(broom)
library(here)
library(ggpubr)
library(ggExtra)
library(ggthemes)
library(ggrepel)
library(Hmisc)
#install_github("alburezg/suffrager")   # this is for using the suffragette palette : )
library(suffrager)
library(devtools)
library(forcats)
library(wesanderson)
library(ggthemes)
library(colorspace)
library(cowplot)
library(grid)
library(magrittr)

# Loading useful functions into environment
source(here("Rcodes","Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved

lt.folder  <- here("Data","lifetables")
tau.folder  <- here("Data","tau")
figs.folder <- here("Figures")
figs.app.folder <- here("Appendix","Figures")

# make in-out directories

dir.create(lt.folder , showWarnings = FALSE, recursive = TRUE)
dir.create(tau.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)
dir.create(figs.app.folder, showWarnings = FALSE, recursive = TRUE)


# Reading in the life tables in single ages, by single-year periods.
# Choosing countries with the longest and varied series available


# females
LT_fem<- ReadHMD(what = "LT_f",
                 countries = NULL,
                 interval = "1x1",
                 username = "email",
                 password = "password",save = FALSE)$data

# saving life tables to make it easier later

  write.table(LT_fem, here("Data","lifetables","lt_fem.csv"), sep=",",row.names = F)


# males
LT_men<- ReadHMD(what = "LT_m",
                 countries = NULL,
                 interval = "1x1",
                 username = "email",
                 password = "password",save = FALSE)$data

# saving life tables to make it easier later

write.table(LT_men, here("Data","lifetables","lt_men.csv"),sep=",", row.names = F)


# Reading life tables that we saved



lt_f<- fread(here("Data","lifetables","lt_fem.csv"))

lt_m<-fread(here("Data","lifetables","lt_men.csv"))


# Combining both lifetables

lt_f$Sex<-"f"
lt_m$Sex<-"m"

# # Life table closing at 90+ for Women

lt_f_90<-lt_f%>%
  group_by(country,Year,Sex) %>%
  mutate(deaths=case_when(Age>=90~sum(dx[Age>=90]),TRUE~dx),
         exposure=case_when(Age>=90~sum(Lx[Age>=90]),TRUE~Lx),
         nmx=deaths/exposure)%>%
  filter(Age<91) %>%
  ungroup() %>%
  arrange(country,Year,Age) %>%
  group_by(country,Year, Sex) %>%
  group_modify(~life.table(.x$nmx, sex="f"), .keep=T) %>%
  mutate(Age=0:90, Sex="f")%>%
  relocate(Age, .after = Year) %>%
  ungroup()

lt_m_90<-lt_m%>%
  group_by(country,Year,Sex) %>%
  mutate(deaths=case_when(Age>=90~sum(dx[Age>=90]),TRUE~dx),
         exposure=case_when(Age>=90~sum(Lx[Age>=90]),TRUE~Lx),
         nmx=deaths/exposure)%>%
  filter(Age<91) %>%
  ungroup() %>%
  arrange(country,Year,Age) %>%
  group_by(country,Year, Sex) %>%
  group_modify(~life.table(.x$nmx, sex="m"), .keep=T) %>%
  mutate(Age=0:90, Sex="m")%>%
  relocate(Age, .after = Year) %>%
  ungroup()

#lt_f_90_ex_check<- lt_f_90 %>%
#  group_by(country,Year,Sex) %>%
#  filter(Age%in%0) %>%
#  mutate(ex=round(ex,2)) %>%
#  select(1,2,4,12)


#lt_m_90_ex_check<- lt_m_90 %>%
#  group_by(country,Year,Sex) %>%
#  filter(Age%in%0) %>%
# mutate(ex=round(ex,2)) %>%
#  select(1,2,4,12)


# selecting out some populations. Here we kept only England and Wales, instead of the UK,
# because of the historical data available for use.

lt_t<-rbind(lt_f,lt_m) %>%
  filter(!country%in%c("FRACNP","NZL_MA","NZL_NM","GBR_NP",
                       "GBRCENW","GBR_SCO","GBR_NIR"))



# renaming countries
lt_t$country <- factor(lt_t$country,
                          labels = c("Australia", "Austria", "Belgium", "Bulgaria",
                                     "Belarus", "Canada", "Switzerland", "Chile",
                                     "Czechia","East Germany", "Germany",
                                     "West Germany" , "Denmark", "Spain"  ,
                                     "Estonia"  ,  "Finland",  "France",
                                     "England & Wales","Greece", "Hong Kong",
                                      "Croatia"   , "Hungary"   , "Ireland" ,
                                      "Iceland" ,   "Israel" ,   "Italy" ,
                                      "Japan" , "Korea" ,"Lithuania"  ,
                                       "Luxembourg" , "Latvia"  ,  "Netherlands"  ,
                                        "Norway"  ,  "New Zealand", "Poland",
                                        "Portugal", "Russia" , "Slovakia",
                                         "Slovenia" ,"Sweden",  "Taiwan"  ,
                                          "Ukraine"  ,  "USA"  ))

lt_t$Sex<- factor(lt_t$Sex, labels = c("Women", "Men"))


# Creating the groups by transition status: pioneers, laggards, east countries and
# fast-paced countries

lt_t_trans<-lt_t  %>%
  mutate(Transition=fct_collapse(country,
                                 Pioneers=c("France",
                                            "Denmark",
                                            "Sweden",
                                            "Australia",
                                            "Belgium",
                                            "Canada",
                                            "Switzerland",
                                            "Norway",
                                            "England & Wales",
                                            "Finland",
                                            "New Zealand"),

                                 Laggards=c("Portugal",
                                            "USA",
                                            "Germany",
                                            "West Germany",
                                            "Greece",
                                            "Chile",
                                            "Spain",
                                            "Luxembourg",
                                            "Italy",
                                            "Israel",
                                            "Ireland",
                                            "Iceland",
                                            "Austria",
                                            "Taiwan",
                                            "Netherlands"),

                                 `East Transition`=c("Croatia",
                                                       "Hungary",
                                                       "Poland",
                                                       "Russia",
                                                       "Slovakia",
                                                       "Slovenia",
                                                       "Ukraine",
                                                       "Lithuania",
                                                       "Latvia",
                                                       "Czechia",
                                                       "East Germany",
                                                       "Estonia",
                                                       "Bulgaria",
                                                       "Belarus"),

                                 `Fast-Paced`=c("Japan",
                                               "Hong Kong",
                                               "Korea")))


# selecting only at birth
lt_ex0_trans<-lt_t_trans %>%
  filter(Age%in%0)


# graphing ex0 trajectories by country and transition status. Selecting the most
# notable representative of each transition group

lt_ex0_trans_p1<-lt_ex0_trans %>%
  filter(country%in%c("Sweden",
                      "Czechia",
                      "Italy",
                      "Japan") & Year>1800) %>%
  droplevels()

p1<-ggplot(lt_ex0_trans %>%  filter(Year>1800), aes(Year,ex,group=country))+
           facet_grid(.~Sex)+
           geom_line(size=0.8, color="grey70", alpha=0.5)+
  geom_line(data=lt_ex0_trans_p1,
            aes(Year,ex, color=country,group=country), size=1.2)+
  facet_grid(.~Sex)+
  theme_minimal(base_size = 24)+
  theme(legend.position  = c(0.07, 0.15),
                legend.title = element_blank(),
                legend.key.width=unit(1,"cm"),
                legend.key.height =unit(0.3,"cm"),
                legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y  = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "grey40"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "grey40"))+
 # scale_color_brewer(type="div")+
 # scale_color_brewer(type="div",palette = "RdBu")+
  scale_colour_manual(values = c("#CA0020","#F4A582","#92C5DE","#0571B0" ),
                      breaks = c("Czechia","Japan","Italy","Sweden") )+
 geom_vline(xintercept = 1950, size=1, color="grey60")+
  geom_vline(xintercept = 1900, size=1, color="grey60")+
  geom_vline(xintercept = 1960, size=1, color="#CA0020",linetype="dashed")+
  geom_vline(xintercept = 1980, size=1, color="#CA0020",linetype="dashed")+
  geom_vline(xintercept = 2000, size=1, color="#CA0020",linetype="dashed")+
  annotate("rect", xmin=1985, xmax=1989, ymin=10, ymax=Inf, alpha=0.1, fill="red")+
 # labs(y=expression(paste("Life Expectancy at birth", ~ (e[0]))))+
  scale_y_continuous(breaks = c(10,30,50,70,90))+
  guides(color = guide_legend(override.aes = list(size = 3.2)))+
  annotate("text", x = 1963, y = 35,
           colour = "red", size = 4, label="Stagnation East-Transition",angle = 90)+
  annotate("text", x = 1985, y = 35,
           colour = "red", size = 4, label="Gorbachev’s anti-alcohol",angle = 90)+
  annotate("text", x = 2005, y = 35,
           colour = "red", size = 4, label="Continued increases in e0",angle = 90)


# graphing by group

p2<-ggplot(lt_ex0_trans%>% filter(Year>1800), aes(Year,ex,group=country, color=Transition))+
  facet_grid(.~Sex)+
  geom_line(size=0.8, color="grey70", alpha=0.5)+
  geom_line(data=lt_ex0_trans %>%
              filter(Transition%in%c("Pioneers") & Year >1800),
            aes(Year,ex, color=Transition,group=country), size=1.2)+
  geom_line(data=lt_ex0_trans %>%
              filter( Transition%in%c("Laggards") & Year >1800),
            aes(Year,ex, color=Transition,group=country), size=1.2)+
  geom_line(data=lt_ex0_trans %>%
              filter(Transition%in%c("East Transition") & Year >1800),
            aes(Year,ex, color=Transition,group=country), size=1.2)+
  geom_line(data=lt_ex0_trans %>%
              filter(Transition%in%c("Fast-Paced") & Year >1800),
            aes(Year,ex, color=Transition,group=country), size=1.2)+
  theme_minimal(base_size = 24)+
  theme(legend.position  = c(0.07, 0.15),
         legend.title = element_blank(),
        legend.key.width=unit(1,"cm"),
        legend.key.height =unit(0.3,"cm"),
        strip.placement = "inside",
        legend.text = element_text(size=10),
        panel.grid.minor = element_blank(),
        axis.title.x  = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "grey40"))+
 # scale_color_manual(values=c("#CA0020", "magenta", "#92C5DE", "#0571B0"))+
 scale_color_brewer(type="div",palette = "RdBu")+
  geom_vline(xintercept = 1950, size=1, color="grey60")+
  geom_vline(xintercept = 1900, size=1, color="grey60")+
  geom_vline(xintercept = 1960, size=1, color="#CA0020",linetype="dashed")+
  geom_vline(xintercept = 1980, size=1, color="#CA0020",linetype="dashed")+
  geom_vline(xintercept = 2000, size=1, color="#CA0020",linetype="dashed")+
  annotate("rect", xmin=1985, xmax=1989, ymin=10, ymax=Inf, alpha=0.1, fill="red")+
  #labs(y=expression(paste("Life Expectancy at birth", ~ (e[0]))))+
  scale_y_continuous(breaks = c(10,30,50,70,90))+
  guides(color = guide_legend(override.aes = list(size = 3.2)))+
  annotate("label", x = 1850, y = 85,
           colour = "#0571B0", size = 4, label="Pioneers")+
  annotate("label", x = 1920, y = 85,
           colour = "#92C5DE", size = 4, label="Laggards")+
  annotate("label", x = 1973, y = 45,
           colour = "#CA0020", size = 4, label="East-Transition")+
  annotate("label", x = 1970, y = 85,
           colour = "#F4A582", size = 4, label="Fast-Paced")

# Figure 1 in the main text

fig1<-ggarrange(p2,p1, common.legend = F,nrow=2,font.label=list(color="black",size=16))
fig1<-annotate_figure(fig1, left = textGrob(expression(paste("Life Expectancy at birth", ~ (e[0]))), rot = 90,
                                      vjust = 0.3, gp = gpar(cex = 1.5)))
ggsave(here("Figures","fig1_ex.png"), width = 14, height = 8, units = "in", dpi=800)

# saving also as pdf

ggsave(here("Figures","fig1_ex.pdf"), width = 14, height = 8, units = "in", dpi=800)


# ----------------------------------------------------------------------------
# Figure that go into the appendix
# after 1950, and also focusing on each country to show the patterns by group
# ----------------------------------------------------------------------------


p1.app<-ggplot(lt_ex0_trans %>% filter(Year>1950), aes(Year,ex,group=country))+
  facet_grid(Sex~.)+
  geom_line(size=0.8, color="grey70", alpha=0.5)+
  geom_line(data=lt_ex0_trans %>%
              filter(Year>1950& country%in%c("France","Sweden","Italy",
                                             "Russia", "USA","Japan","Hong Kong", "Denmark",
                                             "Belarus")),
            aes(Year,ex, color=country,group=country), size=1.2)+
  theme_pander(base_size = 20)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16))+
  #scale_color_brewer(type="div")+
  scale_colour_manual(values = c("magenta", "steelblue",
                                 "darkgreen",
                                 "cyan2",
                                 "purple",
                                 "gold",
                                 "red2",
                                 "#A75529",
                                 "black"))+
  geom_vline(xintercept = 1980, size=1, color="grey40")+
  scale_y_continuous(breaks = seq(50, 90, 10))+
  labs(y=expression(paste("Life Expectancy at birth", ~ (e[0]))))

p1.app

# east and west germany

# add this in the appendix
ggplot(lt_ex0_trans %>% filter(Year>1950), aes(Year,ex,group=country))+
  facet_grid(Sex~.)+
  geom_line(size=0.8, color="grey", alpha=0.5)+
  geom_line(data=lt_ex0_trans %>%
              filter(Year>1950& country%in%c("West Germany","East Germany","Germany")),
            aes(Year,ex, color=country,group=country), size=1.5)+
  facet_grid(Sex~.)+
  theme_pander(base_size = 20)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))+
  scale_colour_manual(values = c("darkred", "steelblue",
                                 #"darkgreen",
                                 #"gold",
                                 #"brown",
                                 #"purple"))#,
                                 #"grey",
                                 "khaki4"))+
  labs(y=expression(paste("Life Expectancy at birth", ~ (e[0]))))
#scale_colour_manual(values = wes_palette("GrandBudapest1", n = 3))
#scale_colour_manual(values = suf_palette("hanwell"))


#as it being an insect plot - do for men and women separately
p4.1<-ggplot(lt_ex0_trans%>% filter(Year>1950 & Sex%in% "Women"),
           aes(Year,ex,group=country, color=Transition))+
  geom_line(size=0.8, color="grey70", alpha=0.5)+
  geom_line(data=lt_ex0_trans %>%
              filter(Transition%in%c("Pioneers") & Year >1950 & Sex%in% "Women"),
            aes(Year,ex, color=Transition,group=country), size=1)+
  geom_line(data=lt_ex0_trans %>%
              filter( Transition%in%c("Laggards") & Year >1950 & Sex%in% "Women"),
            aes(Year,ex, color=Transition,group=country), size=1)+
  geom_line(data=lt_ex0_trans %>%
              filter(Transition%in%c("East Transitioner") & Year >1950 & Sex%in% "Women"),
            aes(Year,ex, color=Transition,group=country), size=1)+
  geom_line(data=lt_ex0_trans %>%
              filter(Transition%in%c("Fast Pace") & Year >1950 & Sex%in% "Women"),
            aes(Year,ex, color=Transition,group=country), size=1)+
  facet_grid(Sex~.)+
  theme_minimal(base_size = 12)+
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x.bottom = element_blank())+
  scale_color_brewer(type="div", palette = "RdBu")+
  labs(y=expression(paste("Life Expectancy at birth", ~ (e[0]))))+
 # geom_vline(xintercept = 1980, size=1, color="grey40")+
  ylim(60,90)



# fast-trasition
ggplot(lt_ex0_trans %>% filter(Year>1950), aes(Year,ex,group=country))+
  facet_grid(Sex~.)+
  geom_line(size=0.8, color="grey70", alpha=0.5)+
  geom_line(data=lt_ex0_trans %>%
              filter(Year>1950& country%in%c("Hong Kong","Korea","Japan","Luxembourg",
                                             "Israel","Iceland","Ireland")),
            aes(Year,ex, color=country,group=country), size=1.2)+
  facet_grid(Sex~.)+
  theme_pander(base_size = 20)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        strip.text.y = element_blank())+
  #scale_color_brewer(type="div")+
  scale_colour_manual(values = c("magenta", "steelblue",
                                 "darkgreen",
                                 "cyan2",
                                 "purple",
                                 "gold",
                                 "red2",
                                 "#A75529",
                                 "#8B0069"))+
#  geom_vline(xintercept = 1950, size=1, color="grey40")+
 # geom_vline(xintercept = 1900, size=1, color="grey40")+
  labs(y=expression(paste("Life Expectancy at birth", ~ (e[0]))))


# ----------------------------------------------------------------------------------------------------------#
# first performing analysis by sex separately Countries have different years of availability.

# ----------------------------------------------------------------------------------------------------------#

# ----------------------------------------------------------------------------------------------------------#
# Pioneer and laggard country are split into 50-year intervals, while fast-paced countries are from 1950-2000
# in 25-years and east-transition from 1960-2000 in 20 years.  There is also a final analysis with all
# countries together from 2000-2018, to focus on the last decade. After 2019 data is not used due to covid.
# ----------------------------------------------------------------------------------------------------------#

# ----------------------------------------------------------------------------------------------------------#
# estimating lambdas for all countries: using the formulas in Vaupel and Yashin (1987,a): ln(lx_new/lx_old)
# ----------------------------------------------------------------------------------------------------------#


lt_t_year<-lt_t_trans %>%
  filter(Year<=2017)



# pioneers - from 1850 to 2000, by 50 years.

lt_pioneer<- lt_t_year %>%
  filter(Transition%in%"Pioneers" & Year%in% c(1850,1900,1950,2000,2017)) %>%
  droplevels() %>%
group_by(country,Sex) %>%
  arrange(country,Year) %>%
  ungroup() %>%
  group_by(country,Age,Sex) %>%
  mutate(lambda=log(lx/lag(lx)),
         lambda=coalesce(lambda,0))%>%
  ungroup()%>%
  group_by(country,Age,Sex) %>%
  mutate(Year=as.numeric(Year),
         Year_lab=as.character(paste(shift(Year),Year,sep="-")))


ggplot(lt_pioneer %>%
         filter(lambda!=0 &Age%in%c(10,30,50,70,99)),
       aes(Age,lambda,  color=Sex, group=Sex))+
  facet_grid(Year_lab~country)+
  geom_line(aes(linetype=Sex), size=1.8)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  labs(y=expression(paste("Intensity of lifesaving ", ~ (lambda[i]))), x="Year")+
  theme_bw()+
  geom_hline(yintercept = 0, size=1)+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))


## estimating the revivorship

# -----------------------------------------------------------------------------------------#
# estimating the i times revivorship function for averting deaths up to 10 times
# -----------------------------------------------------------------------------------------#

setDT(lt_pioneer)

# applying formula for i states of revivorship (replicating the paper, until 10th time)

lt_pioneer2<-lt_pioneer[ , lx_0:= res_all(lx,lambda,0), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_1:= res_all(lx,lambda,1), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_2:= res_all(lx,lambda,2), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_3:= res_all(lx,lambda,3), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_4:= res_all(lx,lambda,4), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_5:= res_all(lx,lambda,5), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_6:= res_all(lx,lambda,6), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_7:= res_all(lx,lambda,7), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_8:= res_all(lx,lambda,8), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_9:= res_all(lx,lambda,9), by=.(country,Sex,Age)]
lt_pioneer2<-lt_pioneer[ , lx_10:= res_all(lx,lambda,10), by=.(country,Sex,Age)]



lt_pioneer2<-lt_pioneer2 %>%
  mutate(lx_New=rowSums(lt_pioneer2[, c(16:26)], na.rm=TRUE))


lt_pioneer3<-lt_pioneer2 %>%
  mutate(lx_3=rowSums(lt_pioneer2[, c(19:26)], na.rm=TRUE))%>%
  select(-c(20:26)) %>%
  mutate(lx_Old=coalesce(lx_0,0),
         lx_1=coalesce(lx_1,0),
         lx_2=coalesce(lx_2,0),
         lx_3=coalesce(lx_3,0),
         res_abs=lx_New-lx_0)


# putting everything in long format


res_long_pioneer<-lt_pioneer3 %>%
  group_by(country,Year,Sex) %>%
  pivot_longer(cols = c(lx_1:lx_3),
               names_to = "lx_res",
               values_to = "res")%>%
 separate(lx_res, into=c("lx_res","i"), sep="_") %>%
  group_by(country,Year,Sex,i) %>%
  mutate(res_prop=(res/lx_New)*100)%>%
 # drop_na() %>%
  ungroup() %>%
  group_by(Year,Age,Sex,i) %>%
  mutate(res_prop_avg=mean(res_prop))


res_long_pioneer$i<-factor(res_long_pioneer$i, levels = c("1", "2", "3"),
                           labels = c("1", "2", "3+"))



# ----------------------------------------------------------------------------------------------------#
# plot revivorship and survivorship differences for each country, then combining similar-transitions
# ----------------------------------------------------------------------------------------------------#

# pioneer countries

#revivorship
ggplot()+
  geom_line(data=res_long_pioneer%>% filter( country=="Denmark"& Age%in% seq(1,80,10) & Year<=2000),
            aes(Age,(res_prop),color=Sex, group=Sex), size=1.6)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))+
  geom_vline(xintercept=50)


# all countries in the background - sweden highlighted

ggplot()+
  geom_jitter(data=res_long_pioneer%>% filter(Age<=100 & Year<=2000),
              aes(Age,(res_abs),color=Sex, group=country), size=3, alpha=0.1, color="grey80")+


  geom_line(data=res_long_pioneer%>% filter(country%in%c("Sweden")
                                                & !Year%in%2018 & Age<=100 & Year<=2000),
            aes(Age,(res_abs),color=Sex, group=Sex), size=1.5)+


  scale_color_manual(name="Sex",values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(.~Year_lab, scales="free")+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12)) #+
 # scale_y_continuous(trans = pseudolog10_trans)


# now with the prportions of res


ggplot()+
  geom_jitter(data=res_long_pioneer%>%
                filter(Age<=100 & lambda>0 & Year<=2000),
              aes(Age,(res_prop),color=Sex, group=Sex), size=3, alpha=0.1, color="grey80")+


  geom_line(data=res_long_pioneer%>%
              filter(country%in%c("Sweden") & !Year%in%2018 & Age<=100 & lambda>0 & Year<=2000),
            aes(Age,(res_prop),color=Sex, group=Sex), size=1.5)+
  scale_color_manual(name="Sex",values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))


#Figure for the main text pioneers countries

Fig2<-ggplot()+
  geom_jitter(data=res_long_pioneer%>%
                filter(Age<=89 & lambda>0),
              aes(Age,(res),color=i, group=country), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_long_pioneer%>%
              filter(country%in%c("Sweden","Italy") & Age<=89 & lambda>0),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(Transition~Year_lab)+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))

# adding lambdas

table_pioneer<-lt_pioneer3 %>%
  filter(country%in%"Sweden" & Age%in%c(10,30,40,50,60,70,90)) %>%
  drop_na()


l_swe<-ggplot(table_pioneer, aes(Age,(lambda)))+
  geom_line(aes(linetype=Sex),size=2)+
  facet_grid(.~Year_lab)+
  labs(y=expression(paste("Intensity of Lifesaving", ~ (Lambda[x]))))+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        strip.text.x = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))


ggarrange(Fig2,l_swe, common.legend = F, ncol = 1)

png(here(figs.folder, "Fig2.png"),"Fig2.png", units="in", width=12, height=10, res=300)
ggarrange(Fig2,l_swe, common.legend = F, ncol = 1)
dev.off()

table_pioneer_80<-lt_pioneer3 %>%
  filter(country%in%"Sweden" & Age%in%c(10,30,40,50,60,70,80)) %>%
  drop_na()


# Figure with all countries for the appendix

Fig2_AP<-ggplot()+
  geom_line(data=res_long_pioneer%>%
              filter(!Year%in%2018 & Age<=89 & lambda>0),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(Year_lab~country)+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        strip.text.x = element_text(size = 13),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))

Fig2_AP

ggsave(here("Appendix","Figures","Fig2_AP.png"), width=45, height=20, units = "cm", dpi=400)



####################################################

# laggards

lt_laggards<- lt_t_year %>%
  filter(Transition%in%"Laggards" & Year%in% c(1900,1950,2000,2017)) %>%
  droplevels() %>%
  group_by(country,Sex) %>%
  arrange(country,Year) %>%
  ungroup() %>%
  group_by(country,Age,Sex) %>%
  mutate(lambda=log(lx/lag(lx)),
         lambda=coalesce(lambda,0))%>%
  ungroup()%>%
  group_by(country,Age,Sex) %>%
  mutate(Year=as.numeric(Year),
         Year_lab=as.character(paste(shift(Year),Year,sep="-")))


ggplot(lt_laggards %>%
         filter(lambda!=0 &Age%in%c(10,30,50,70,90,100)),
       aes(Age,lambda,  color=Sex, group=Sex))+
  facet_grid(country~Year_lab, scales = "free")+
  geom_line(aes(linetype=Sex), size=1.8)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  labs(y=expression(paste("Intensity of lifesaving ", ~ (lambda[i]))), x="Year")+
  theme_bw()+
  geom_hline(yintercept = 0, size=1)+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))




## estimating the revivorship

# -----------------------------------------------------------------------------------------#
# estimating the i times revivorship function for averting deaths up to 10 times
# -----------------------------------------------------------------------------------------#

setDT(lt_laggards)


# applying formula for i states of revivorship (replicating the paper, until 10th time)

lt_laggards2<-lt_laggards[ , lx_0:= res_all(lx,lambda,0), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_1:= res_all(lx,lambda,1), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_2:= res_all(lx,lambda,2), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_3:= res_all(lx,lambda,3), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_4:= res_all(lx,lambda,4), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_5:= res_all(lx,lambda,5), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_6:= res_all(lx,lambda,6), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_7:= res_all(lx,lambda,7), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_8:= res_all(lx,lambda,8), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_9:= res_all(lx,lambda,9), by=.(country,Sex,Age)]
lt_laggards2<-lt_laggards[ , lx_10:= res_all(lx,lambda,10), by=.(country,Sex,Age)]



lt_laggards2<-lt_laggards2 %>%
  mutate(lx_New=rowSums(lt_laggards2[, c(16:26)], na.rm=TRUE))


lt_laggards3<-lt_laggards2 %>%
  mutate(lx_3=rowSums(lt_laggards2[, c(19:26)], na.rm=TRUE))%>%
  select(-c(20:26)) %>%
  mutate(lx_Old=coalesce(lx_0,0),
         lx_1=coalesce(lx_1,0),
         lx_2=coalesce(lx_2,0),
         lx_3=coalesce(lx_3,0),
         res_abs=lx_New-lx_0)

# putting everything in long format


res_long_lt_laggards<-lt_laggards3 %>%
  group_by(country,Year,Sex) %>%
  pivot_longer(cols = c(lx_1:lx_3),
               names_to = "lx_res",
               values_to = "res")%>%
  separate(lx_res, into=c("lx_res","i"), sep="_") %>%
  group_by(country,Year,Sex,i) %>%
  mutate(res_prop=(res/lx_New)*100)%>%
  #drop_na() %>%
  ungroup() %>%
  group_by(Year,Age,Sex,i) %>%
  mutate(res_prop_avg=mean(res_prop))


res_long_lt_laggards$i<-factor(res_long_lt_laggards$i, levels = c("1", "2", "3"),
                           labels = c("1", "2", "3+"))


# ----------------------------------------------------------------------------------------------------#
# plot revivorship and survivorship differences for each country, then combining similar-transitions
# ----------------------------------------------------------------------------------------------------#

# laggards countries

#revivorship
ggplot()+
  geom_line(data=res_long_lt_laggards%>% filter( country=="Italy"& Age%in% seq(1,80,10)&
                                                   Year<=2000 ),
            aes(Age,(res_prop),color=Sex, group=Sex), size=1.6)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))+
  geom_vline(xintercept=50)



ggplot()+
  geom_jitter(data=res_long_lt_laggards%>% filter(Age%in% seq(1,90,10)& Year<=2000 & Sex%in%"Women" & Year!=2018 ),
              aes(Age,(res_abs),color=Sex, group=country), size=5, alpha=0.3, color="grey70")+
  geom_jitter(data=res_long_lt_laggards%>% filter(Age%in% seq(1,90,10) & Sex%in%"Men" & Year!=2078 ),
              aes(Age,(res_abs),color=Sex, group=country), size=5, alpha=0.3,color="grey70")+


  geom_line(data=res_long_lt_laggards%>% filter(country%in%c("Italy")
                                            & Age%in% seq(1,90,10) & Year!=2017 & Year<=2000  ),
            aes(Age,(res_abs),color=Sex, group=Sex, linetype=country), size=1.5)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  scale_linetype_manual(values = c("solid","dotted"))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))



ggplot()+
  geom_line(data=res_long_lt_laggards%>% filter( country=="Italy"& Age%in% seq(1,80,10) & Year<=2000),
            aes(Age,log10(res_prop),color=Sex, group=Sex), size=1.6)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))+
  geom_vline(xintercept=50)


# similar graphs to pioneers

# now with the prportions of res


ggplot()+
  geom_jitter(data=res_long_lt_laggards%>%
                filter( Age<=89 & lambda>0& Year<=2000),
              aes(Age,(res_prop),color=Sex, group=Sex), size=3, alpha=0.1, color="grey80")+


  geom_line(data=res_long_lt_laggards%>%
              filter(country%in%c("Italy") & Age<=89 & lambda>0& Year<=2000 ),
            aes(Age,(res_prop),color=Sex, group=Sex), size=1.5)+


  scale_color_manual(name="Sex",values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))


# All laggards with Italy highlighted

Fig3<-ggplot()+
  geom_jitter(data=res_long_lt_laggards%>%
                filter( Age<=89 & lambda>0),
              aes(Age,(res),color=i, group=country), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_long_lt_laggards%>%
              filter(country%in%c("Italy") & Age<=89 & lambda>0 ),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(.~Year_lab)+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))


# adding lambdas

table_lag<-lt_laggards3 %>%
  filter(country%in%"Italy" & Age%in%c(10,30,50,70,90)) %>%
  drop_na()


l_ita<-ggplot(table_lag, aes(Age,(lambda)))+
  geom_line(aes(linetype=Sex),size=2)+
  facet_grid(.~Year_lab)+
  labs(y=expression(paste("Intensity of Lifesaving", ~ (Lambda[x]))))+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        strip.text.x = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))


ggarrange(Fig3,l_ita, common.legend = F, ncol = 1)

png(here(figs.folder, "Fig3.png"),"Fig3.png", units="in", width=12, height=10, res=300)
ggarrange(Fig3,l_ita, common.legend = F, ncol = 1)
dev.off()


table_lag90<-lt_laggards3 %>%
  filter(country%in%"Italy" & Age%in%c(90)) %>%
  drop_na()

#All cases for the Appendix

Fig3_AP<-ggplot()+
  geom_line(data=res_long_lt_laggards%>%
              filter(Age<=89 & lambda>0 ),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(Year_lab~country)+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))



Fig3_AP

ggsave(here("Review","Review_2","Figures","Fig3_AP.png"), width=50, height=20, units = "cm", dpi=400)

#-----------------------------------------------------

# East-Transition countries
#-----------------------------------------------------

lt_east<- lt_t_year %>%
  filter(Transition%in%"East Transition" & Year%in% c(1960,2000,2017)) %>%
  droplevels() %>%
  group_by(country,Sex) %>%
  arrange(country,Year) %>%
  ungroup() %>%
  group_by(country,Age,Sex) %>%
  mutate(lambda=log(lx/lag(lx)),
         lambda=coalesce(lambda,0))%>%
  ungroup()%>%
  group_by(country,Age,Sex) %>%
  mutate(Year=as.numeric(Year),
         Year_lab=as.character(paste(shift(Year),Year,sep="-")))


ggplot(lt_east %>%
         filter(lambda!=0 &Age%in%c(10,30,50,70,90,100)),
       aes(Age,lambda,  color=Sex, group=Sex))+
  facet_grid(country~Year_lab, scales = "free")+
  geom_line(aes(linetype=Sex), size=1.8)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  labs(y=expression(paste("Intensity of lifesaving ", ~ (lambda[i]))), x="Year")+
  theme_bw()+
  geom_hline(yintercept = 0, size=1)+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))


# -----------------------------------------------------------------------------------------#
# estimating the i times revivorship function for averting deaths up to 10 times
# -----------------------------------------------------------------------------------------#

setDT(lt_east)

# applying formula for i states of revivorship (replicating the paper, until 10th time)

lt_east2<-lt_east[ , lx_0:= res_all(lx,lambda,0), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_1:= res_all(lx,lambda,1), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_2:= res_all(lx,lambda,2), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_3:= res_all(lx,lambda,3), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_4:= res_all(lx,lambda,4), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_5:= res_all(lx,lambda,5), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_6:= res_all(lx,lambda,6), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_7:= res_all(lx,lambda,7), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_8:= res_all(lx,lambda,8), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_9:= res_all(lx,lambda,9), by=.(country,Sex,Age)]
lt_east2<-lt_east[ , lx_10:= res_all(lx,lambda,10), by=.(country,Sex,Age)]



lt_east2<-lt_east2 %>%
  mutate(lx_New=rowSums(lt_east2[, c(16:26)], na.rm=TRUE))


lt_east3<-lt_east2 %>%
  mutate(lx_3=rowSums(lt_east2[, c(19:26)], na.rm=TRUE))%>%
  select(-c(20:26)) %>%
  mutate(lx_Old=coalesce(lx_0,0),
         lx_1=coalesce(lx_1,0),
         lx_2=coalesce(lx_2,0),
         lx_3=coalesce(lx_3,0),
         res_abs=lx_New-lx_0)

# putting everything in long format


res_long_lt_east<-lt_east3 %>%
  group_by(country,Year,Sex) %>%
  pivot_longer(cols = c(lx_1:lx_3),
               names_to = "lx_res",
               values_to = "res")%>%
  separate(lx_res, into=c("lx_res","i"), sep="_") %>%
  group_by(country,Year,Sex,i) %>%
  mutate(res_prop=(res/lx_New)*100)%>%
  #drop_na() %>%
  ungroup() %>%
  group_by(Year,Age,Sex,i) %>%
  mutate(res_prop_avg=mean(res_prop))


res_long_lt_east$i<-factor(res_long_lt_east$i, levels = c("1", "2", "3"),
                               labels = c("1", "2", "3+"))

res_long_lt_east<-res_long_lt_east %>%
  mutate(Year_lab=fct_collapse(Year_lab,
                               `1950-2000`=c("1960-2000")))


# ----------------------------------------------------------------------------------------------------#
# plot revivorship and survivorship differences for each country, then combining similar-transitions
# ----------------------------------------------------------------------------------------------------#

# east countries

#revivorship
ggplot()+
  geom_line(data=res_long_lt_east%>% filter( country=="Czechia"& Age%in% seq(1,80,10) ),
            aes(Age,(res_prop),color=Sex, group=Sex), size=1.6)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))+
  geom_vline(xintercept=50)



ggplot()+
  geom_jitter(data=res_long_lt_east%>% filter(Age%in% seq(1,90,10) & Sex%in%"Women" & Year!=2017 ),
              aes(Age,(res_abs),color=Sex, group=country), size=5, alpha=0.3, color="grey70")+
  geom_jitter(data=res_long_lt_east%>% filter(Age%in% seq(1,90,10) & Sex%in%"Men" & Year!=2017 ),
              aes(Age,(res_abs),color=Sex, group=country), size=5, alpha=0.3,color="grey70")+


  geom_line(data=res_long_lt_east%>% filter(country%in%c("Czechia")
                                                & Age%in% seq(1,90,10) & Year!=2018  ),
            aes(Age,(res_abs),color=Sex, group=Sex, linetype=country), size=1.5)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  scale_linetype_manual(values = c("solid","dotted"))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))



ggplot()+
  geom_line(data=res_long_lt_east%>% filter( country=="Czechia"& Age%in% seq(1,80,10) ),
            aes(Age,log10(res_prop),color=Sex, group=Sex), size=1.6)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))+
  geom_vline(xintercept=50)





# similar graphs to east
# now with the prportions of res


ggplot()+
  geom_jitter(data=res_long_lt_east%>%
                filter( Age<=89 & lambda>0),
              aes(Age,(res_prop),color=Sex, group=Sex), size=3, alpha=0.1, color="grey80")+


  geom_line(data=res_long_lt_east%>%
              filter(country%in%c("Czechia") & Age<=89 & lambda>0 ),
            aes(Age,(res_prop),color=Sex, group=Sex), size=1.5)+


  scale_color_manual(name="Sex",values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))



# All East with Czechia highlighted

Fig4<-ggplot()+
  geom_jitter(data=res_long_lt_east%>%
                filter( Age<=89 & lambda>0),
              aes(Age,(res),color=i, group=country), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_long_lt_east%>%
              filter(country%in%c("Czechia") & Age<=89 & lambda>0 ),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(.~Year_lab)+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))

Fig4
png(here(figs.folder, "Fig4.png"),"Fig4.png", units="in", width=14, height=7, res=300)
Fig4
dev.off()


table_east<-lt_east3 %>%
  filter(country%in%"Czechia" & Age%in%c(10,30,50,70,90)) %>%
  drop_na()


l_cza<-ggplot(table_east, aes(Age,(lambda)))+
  geom_line(aes(linetype=Sex),size=2)+
  facet_grid(.~Year_lab)+
  labs(y=expression(paste("Intensity of Lifesaving", ~ (Lambda[x]))))+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        strip.text.x = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))


ggarrange(Fig4,l_cza, common.legend = F, ncol = 1)

png(here(figs.folder, "Fig4.png"),"Fig4.png", units="in", width=12, height=10, res=300)
ggarrange(Fig4,l_cza, common.legend = F, ncol = 1)
dev.off()

#All cases for the Appendix

Fig4_AP<-ggplot()+
  geom_line(data=res_long_lt_east%>%
              filter(Age<=89 & lambda>0 ),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(Year_lab~country)+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))



Fig4_AP
ggsave(here("Review","Review_2","Figures","Fig4_AP.png"), width=50, height=20, units = "cm", dpi=400)



##############################################################################################
# Fast-paced
##############################################################################################


lt_fast<- lt_t_year %>%
  filter(Transition%in%"Fast-Paced" & Year%in% c(1950,2003,2017)) %>%
  droplevels() %>%
  group_by(country,Sex) %>%
  arrange(country,Year) %>%
  ungroup() %>%
  group_by(country,Age,Sex) %>%
  mutate(lambda=log(lx/lag(lx)),
         lambda=coalesce(lambda,0))%>%
  ungroup()%>%
  group_by(country,Age,Sex) %>%
  mutate(Year=as.numeric(Year),
         Year_lab=as.character(paste(shift(Year),Year,sep="-")))


ggplot(lt_fast %>%
         filter(lambda!=0 &Age%in%c(10,30,50,70,90,100)),
       aes(Age,lambda,  color=Sex, group=Sex))+
  facet_grid(country~Year_lab, scales = "free")+
  geom_line(aes(linetype=Sex), size=1.8)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  labs(y=expression(paste("Intensity of lifesaving ", ~ (lambda[i]))), x="Year")+
  theme_bw()+
  geom_hline(yintercept = 0, size=1)+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))


# -----------------------------------------------------------------------------------------#
# estimating the i times revivorship function for averting deaths up to 10 times
# -----------------------------------------------------------------------------------------#

setDT(lt_fast)

# applying formula for i states of revivorship (replicating the paper, until 10th time)

lt_fast2<-lt_fast[ , lx_0:= res_all(lx,lambda,0), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_1:= res_all(lx,lambda,1), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_2:= res_all(lx,lambda,2), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_3:= res_all(lx,lambda,3), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_4:= res_all(lx,lambda,4), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_5:= res_all(lx,lambda,5), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_6:= res_all(lx,lambda,6), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_7:= res_all(lx,lambda,7), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_8:= res_all(lx,lambda,8), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_9:= res_all(lx,lambda,9), by=.(country,Sex,Age)]
lt_fast2<-lt_fast[ , lx_10:= res_all(lx,lambda,10), by=.(country,Sex,Age)]



lt_fast2<-lt_fast2 %>%
  mutate(lx_New=rowSums(lt_fast2[, c(16:26)], na.rm=TRUE))


lt_fast3<-lt_fast2 %>%
  mutate(lx_3=rowSums(lt_fast2[, c(19:26)], na.rm=TRUE))%>%
  select(-c(20:26)) %>%
  mutate(lx_Old=coalesce(lx_0,0),
         lx_1=coalesce(lx_1,0),
         lx_2=coalesce(lx_2,0),
         lx_3=coalesce(lx_3,0),
         res_abs=lx_New-lx_0)

# putting everything in long format


res_long_lt_fast<-lt_fast3 %>%
  group_by(country,Year,Sex) %>%
  pivot_longer(cols = c(lx_1:lx_3),
               names_to = "lx_res",
               values_to = "res")%>%
  separate(lx_res, into=c("lx_res","i"), sep="_") %>%
  group_by(country,Year,Sex,i) %>%
  mutate(res_prop=(res/lx_New)*100)%>%
  #drop_na() %>%
  ungroup() %>%
  group_by(Year,Age,Sex,i) %>%
  mutate(res_prop_avg=mean(res_prop))


res_long_lt_fast$i<-factor(res_long_lt_fast$i, levels = c("1", "2", "3"),
                           labels = c("1", "2", "3+"))


res_long_lt_fast<-res_long_lt_fast %>%
  mutate(Year_lab=fct_collapse(Year_lab,
                               `2000-2017`=c("2003-2017"),
                               `1950-2000`=c("1950-2003")))

# ----------------------------------------------------------------------------------------------------#
# plot revivorship and survivorship differences for each country, then combining similar-transitions
# ----------------------------------------------------------------------------------------------------#


#revivorship
ggplot()+
  geom_line(data=res_long_lt_fast%>% filter( country=="Japan"& Age%in% seq(1,80,10) ),
            aes(Age,log10(res_prop),color=Sex, group=Sex), size=1.6)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))+
  geom_vline(xintercept=50)



ggplot()+
  geom_jitter(data=res_long_lt_fast%>% filter(Age%in% seq(1,90,10) & Sex%in%"Women" & Year!=2017 ),
              aes(Age,(res_abs),color=Sex, group=country), size=5, alpha=0.3, color="grey70")+
  geom_jitter(data=res_long_lt_fast%>% filter(Age%in% seq(1,90,10) & Sex%in%"Men" & Year!=2017 ),
              aes(Age,(res_abs),color=Sex, group=country), size=5, alpha=0.3,color="grey70")+


  geom_line(data=res_long_lt_fast%>% filter(country%in%c("Japan")
                                            & Age%in% seq(1,90,10) & Year!=2017  ),
            aes(Age,(res_abs),color=Sex, group=Sex, linetype=country), size=1.5)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  scale_linetype_manual(values = c("solid","dotted"))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))



ggplot()+
  geom_line(data=res_long_lt_fast%>% filter( country=="Japan"& Age%in% seq(1,80,10) ),
            aes(Age,log10(res_prop),color=Sex, group=Sex), size=1.6)+
  scale_color_manual(values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))+
  geom_vline(xintercept=50)





# similar graphs to east
# now with the prportions of res


ggplot()+
  geom_jitter(data=res_long_lt_fast%>%
                filter( Age<=89 & lambda>0),
              aes(Age,(res_prop),color=Sex, group=Sex), size=3, alpha=0.1, color="grey80")+
  geom_line(data=res_long_lt_fast%>%
              filter(country%in%c("Japan") & Age<=89 & lambda>0 ),
            aes(Age,(res_prop),color=Sex, group=Sex), size=1.5)+
  scale_color_manual(name="Sex",values = rev(suf_palette("hanwell", n = 2,type = "discrete")))+
  facet_grid(i~Year_lab, scales = "free")+
  theme_pander(base_size = 14)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12))


# All fast-paced with Japan highlighted


Fig5<-ggplot()+
  geom_jitter(data=res_long_lt_fast%>%
                filter( Age<=89 & lambda>0),
              aes(Age,(res),color=i, group=country), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_long_lt_fast%>%
              filter(country%in%c("Japan") & Age<=89 & lambda>0 ),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(.~Year_lab)+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))

# adding lambdas

table_fast<-lt_fast3 %>%
  filter(country%in%"Japan" & Age%in%c(10,30,50,70,90)) %>%
  drop_na()


l_jap<-ggplot(table_fast, aes(Age,(lambda)))+
  geom_line(aes(linetype=Sex),size=2)+
  facet_grid(.~Year_lab)+
  labs(y=expression(paste("Intensity of Lifesaving", ~ (Lambda[x]))))+
  theme_pander(base_size = 18)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        strip.text.x = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))


ggarrange(Fig5,l_jap, common.legend = F, ncol = 1)

png(here(figs.folder, "Fig5.png"),"Fig5.png", units="in", width=12, height=10, res=300)
ggarrange(Fig5,l_jap, common.legend = F, ncol = 1)
dev.off()


#All cases for the Appendix

Fig5_AP<-ggplot()+
  geom_line(data=res_long_lt_fast%>%
              filter(Age<=89 & lambda>0 ),
            aes(Age,(res),color=i, linetype=Sex), size=2)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_grid(Year_lab~country)+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))



Fig5_AP

ggsave(here("Review","Review_2","Figures","Fig5_AP.png"), width=30, height=15, units = "cm", dpi=400)


#all cases look how?

res_all_countries<-rbind(res_long_pioneer,res_long_lt_laggards,res_long_lt_east,res_long_lt_fast)

View(res_all_countries)


Fig7_AP<-ggplot()+
  geom_jitter(data=res_all_countries%>%
                filter( Age<=89 & lambda>0),
              aes(Age,(res),color=Year_lab, group=country, shape=Sex), size=2)+

  facet_grid(Transition~i)+
  #scale_color_viridis_c()
  scale_color_brewer(type="div",palette = "RdBu")+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))

ggsave(here("Review","Review_2","Figures","Fig7_AP.png"), width=30, height=25, units = "cm", dpi=400)



# survival of only last decade

res_all<-res_all_countries%>%
  filter(Year>2000)

class(res_all$Year_lab)

res_all$Year_lab<-as.factor(res_all$Year_lab)

res_all<-res_all %>%
  mutate(Year_lab=fct_collapse(Year_lab,
                                `2000-2017`=c("2003-2017")))



Fig_all<-ggplot()+
  geom_line(data=res_all%>%
              filter(Age<=89 & lambda>0 &Year_lab%in%"2000-2017"),
            aes(Age,(res),color=i, linetype=Sex), size=1.3)+
  scale_color_manual(name="Revivorship times",values=c("#67001F","#D6604D", "#F4A582"))+
  facet_wrap(country~Year_lab)+
  theme_pander(base_size = 12)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        plot.title = element_text(size = 12),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,30,60,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))
Fig_all

ggsave(here("Review","Review_2","Figures","Fig6_AP.png"), width=30, height=30, units = "cm", dpi=400)

# -----------------------------------------------------------------------------------------------------#
# now estimating the tau´s, which are the number of life years expected to be lived in each revivorship
# state. We chose to approximate the integrals through Riemanns´sum, as they yielded small errors.
# -----------------------------------------------------------------------------------------------------#

tau_all<-function (lx) {
  m<-111
  a<-0
  b<-110
  w<-(b-a)/m
  x<-seq(a+(w/2),b-(w/2),w)
  h<-lx
  ex<-sum(h*w)
  return(ex)
}

# computing taus, difference in ex0 and proportion of revived persons


res_all_long2<-res_all_countries %>%
  group_by(country,Year_lab,Sex,i) %>%
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>%
  mutate(tau=sum(res/100000, na.rm = T))

res_tau<-res_all_long2 %>%
  # filter(lx>0) %>%
  group_by(country,Year_lab,Sex,i) %>%
  summarise(tau) %>%
  slice(n()) %>%
  spread(i,tau) %>%
  ungroup()%>%
  group_by(country,Year_lab,Sex) %>%
  rowwise() %>%
  mutate(ex_diff = sum(c_across(all_of(c("1", "2", "3+"))))) %>%
 # drop_na() %>%
  filter(`1`>0)


res_ex_w<-res_all_long2 %>%
  filter(Age%in%0) %>%
  group_by(country,Year,Sex) %>%
  summarise(ex) %>%
  slice(n()) %>%
  pivot_wider(id_cols = c(country,Year), names_from = Sex, values_from = ex)


res_tau_main<-res_tau %>%
  filter(country%in%c("Sweden","Italy","Japan","Czechia"))


res_ex_main<-res_all_long2 %>%
  filter(Age%in%0 & country%in%c("Sweden","Italy","Japan","Czechia")) %>%
  group_by(country,Year,Sex) %>%
  summarise(ex) %>%
  slice(n()) %>%
  pivot_wider(id_cols = c(country,Year), names_from = Sex, values_from = ex)

res_ex_main_long<-res_all_long2 %>%
  filter(Age%in%0 & country%in%c("Sweden","Italy","Japan","Czechia")) %>%
  group_by(country,Year,Sex) %>%
  summarise(ex) %>%
  slice(n())

write.table(res_tau, here("Review","Review_2","Tables","res_tau.csv"), sep=",", row.names = F) # info for building Table 1.

write.table(res_tau_main, here("Review","Review_2","Tables","res_tau_main.csv"), sep=",", row.names = F)
write.table(res_ex_w, here("Review","Review_2","Tables","res_ex_birth.csv"), sep=",", row.names = F) # info for building Table 1.

write.table(res_ex_main, here("Review","Review_2","Tables","res_ex_main.csv"), sep=",", row.names = F) # info for building Table 1.
write.table(res_ex_main_long, here("Review","Review_2","Tables","res_ex_main_long.csv"), sep=",", row.names = F)

##--------------------------------------------------------------------------------------------------------------------#
# Estimating e-dagger and entropy for revivorship states. I compared my function with Alyson's formulae in
# https://github.com/alysonvanraalte/LifeIneq. See source codes for how I adapted the function to include
# the higher order terms with factorials. Some results were compared to Aburto, J.M., van Raalte, A.
# Lifespan Dispersion in Times of Life Expectancy Fluctuation: The Case of Central and Eastern Europe.
# Demography 55, 2071–2096 (2018). https://doi.org/10.1007/s13524-018-0729-9 and also to the historical data in
# Vaupel and Yashin (1987a) used for the US (from Faber, J. F. 1982. Life Tables for the United States: 1900-2050.).
#--------------------------------------------------------------------------------------------------------------------#


lt_H<-lt_t %>%
  group_by(country,Year,Sex) %>%
  mutate(edagg_1=round(edagg(lx,1),2),
         edagg_2=round(edagg(lx,2)*-1,2),
         edagg_3=round(edagg(lx,3),2),
         edagg_4=round(edagg(lx,4)*-1,2),
         edagg_5=round(edagg(lx,5),2),
         H_1=round(H(lx,1),2),
         H_2=round(H(lx,2)*-1,2),
         H_3=round(H(lx,3),2),
         H_4=round(H(lx,4)*-1,2),
         H_5=round(H(lx,5),2)) %>%
  ungroup()


lt_H_i<-lt_t %>%
  group_by(country,Year,Sex) %>%
  mutate(edagg_1=round(edagg(lx,1),2),
         edagg_2=round(edagg(lx,2)*-1,2),
         edagg_3=round(edagg(lx,3),2),
         edagg_4=round(edagg(lx,4)*-1,2),
         edagg_5=round(edagg(lx,5),2),
         H_1=round(H(lx,1),2),
         H_2=round(H(lx,2)*-1,2),
         H_3=round(H(lx,3),2),
         H_4=round(H(lx,4)*-1,2),
         H_5=round(H(lx,5),2)) %>%
  ungroup()


lt_H_spans<-lt_H %>%
  group_by(country,Year,Sex) %>%
  mutate(gain_1=round(ex*H_1,2),                      # this is actually equal to edagger. But I estimated to check
         gain_2=round(ex*H_2,2),
         gain_3=round(ex*H_3,2),
         gain_4=round(ex*H_4,2),
         gain_5=round(ex*H_5,2),
         span_1=round(ex+gain_1,2),
         span_2=round(span_1+gain_2,2),
         span_3=round(span_2+gain_3,2),
         span_4=round(span_3+gain_4,2),
         span_5=round(span_4+gain_5,2)) %>%
  filter(Age%in%0) %>%
  dplyr::select(-c(4:10))

write.table(lt_H_spans, here("Data","tau","lifespan.csv"), sep=",", row.names = F) # info for building
write.table(lt_H_spans, here("Review","Review_2","Tables","lifespan.csv"), sep=",", row.names = F) # info for building
# ploting edagger and entropy


# first making the data set long for inequality

lt_H_edag<-lt_H_spans %>%
  group_by(country,Year,Sex) %>%
  dplyr::select(1:10) %>%
  pivot_longer(cols=c(6:10), names_to="times",values_to="inequality") %>%
  separate(times, sep="_", into=c("times1","times2")) %>%
  dplyr::select(-6)



library(latex2exp)

X11()
ed<-ggplot(data=lt_H_edag,
       aes(ex, inequality,color=times2))+
  geom_jitter(size=4, alpha=0.4)+
  facet_grid(.~Sex)+
  scale_color_manual(values = c("#B35806", "#E08214", "#FEE0B6","#B2ABD2" ,"#8073AC"),
                     name="Revivorship times")+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Effect of repeatedly saving lives", ~  ~italic(i), "times at all ages", ~  ~ (e[i]^"\206"))),
       x=TeX('Life Expectancy at Birth ($\\e_{0}$)'))


# lifetable entropy

lt_H_h<-lt_H_spans %>%
  group_by(country,Year,Sex) %>%
  dplyr::select(1:5,11:15) %>%
  pivot_longer(cols=c(6:10), names_to="times",values_to="entropy") %>%
  separate(times, sep="_", into=c("times1","times2")) %>%
  dplyr::select(-6)


X11()
eh<-ggplot(data=lt_H_h,
       aes(ex, entropy,color=times2))+
  geom_jitter(size=4, alpha=0.4)+
  facet_grid(.~Sex)+
  scale_color_manual(values = c("#B35806", "#E08214", "#FEE0B6","#B2ABD2" ,"#8073AC"),
                     name="Revivorship times")+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Lifetable entropy for each",~  ~italic(ith), "time a life is saved", ~  ~ (H[i]))),
       x=TeX('Life Expectancy at Birth ($\\e_{0}$)'))


# let´s add the transition countries

lt_H_h_trans<-lt_H_h  %>%
  mutate(Transition=fct_collapse(country,
                                 Pioneers=c("France",
                                            "Denmark",
                                            "Sweden",
                                            "Australia",
                                            "Belgium",
                                            "Canada",
                                            "Switzerland",
                                            "Norway",
                                            "England & Wales",
                                            "Finland",
                                            "New Zealand"),

                                 Laggards=c("Portugal",
                                            "USA",
                                            "Germany",
                                            "West Germany",
                                            "Greece",
                                            "Chile",
                                            "Spain",
                                            "Luxembourg",
                                            "Italy",
                                            "Israel",
                                            "Ireland",
                                            "Iceland",
                                            "Austria",
                                            "Taiwan",
                                            "Netherlands"),

                                 `East Transition`=c("Croatia",
                                                     "Hungary",
                                                     "Poland",
                                                     "Russia",
                                                     "Slovakia",
                                                     "Slovenia",
                                                     "Ukraine",
                                                     "Lithuania",
                                                     "Latvia",
                                                     "Czechia",
                                                     "East Germany",
                                                     "Estonia",
                                                     "Bulgaria",
                                                     "Belarus"),

                                 `Fast-Paced`=c("Japan",
                                                "Hong Kong",
                                                "Korea")))

# so same plot for transitional countries

ggplot(data=lt_H_h_trans,
       aes(ex, entropy,color=times2))+
  geom_jitter(size=4, alpha=0.4)+
  facet_grid(.~Transition)+
  scale_color_manual(values = c("#B35806", "#E08214", "#FEE0B6","#B2ABD2" ,"#8073AC"),
                     name="Revivorship times")+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Lifetable entropy for each",~  ~italic(ith), "time a life is saved", ~  ~ (H[i]))),
       x=TeX('Life Expectancy at Birth ($\\e_{0}$)'))+
  geom_point(data=lt_H_h_trans %>%
               filter(country%in%c("Sweden","Italy","Czechia","Japan")&
                        Year%in%c(1970)  & Sex%in% "Women"),color= "black", size=2.1)+
  geom_text_repel(data=lt_H_h_trans %>%
                    filter(country%in%c("Sweden","Italy","Czechia","Japan")&
                             Year%in%c(1970) & times2%in%1 & Sex%in% "Women"),
                  aes(label=country), color= "black", size=4,
                  vjust = 0.5,
                  nudge_x = 0.2,
                  nudge_y = 0.2)



# life years gained by saving a person´s life the ith time.

lt_H_gain<-lt_H_spans %>%
  group_by(country,Year,Sex) %>%
  dplyr::select(1:5,16:20) %>%
  pivot_longer(cols=c(6:10), names_to="times",values_to="gains") %>%
  separate(times, sep="_", into=c("times1","times2")) %>%
  dplyr::select(-6)

View(lt_H_gain)


X11()
ggplot(data=lt_H_gain,
       aes(ex, gains,color=times2))+
  geom_jitter(size=4, alpha=0.4)+
  facet_grid(.~Sex)+
  scale_color_manual(values = c("#B35806", "#E08214", "#FEE0B6","#B2ABD2" ,"#8073AC"),
                     name="Revivorship times")+
  theme_pander(base_size = 16)+
  theme(legend.position = "bottom",
        legend.title=element_text(size=16),
        plot.title = element_text(size = 16),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Life years gained by saving a life the ith time", ~  ~ (H[i]*e[0]))),
       x=TeX('Life Expectancy at Birth ($\\e_{0}$)'))

# benefit of saving a lfe the ith time. (not including the further benefits of additional saving)
# so this is the effect of repeatedly averting death on the lifespan once and only once, twice and only twice.


lt_H_span_gain<-lt_H_spans %>%
  group_by(country,Year,Sex) %>%
  dplyr::select(1:5,21:25) %>%
  pivot_longer(cols=c(6:10), names_to="times",values_to="spans") %>%
  separate(times, sep="_", into=c("times1","times2")) %>%
  dplyr::select(-6)


X11()
ben<-ggplot(data=lt_H_span_gain,
       aes(ex, spans,color=times2))+
  geom_jitter(size=4, alpha=0.4)+
  facet_grid(.~Sex)+
 scale_color_manual(values = c("#B35806", "#E08214", "#FEE0B6","#B2ABD2" ,"#8073AC"),
                     name="Revivorship times")+
# scale_colour_brewer(palette = "Spectral",name="Revivorship times")+
  theme_pander(base_size = 14)+
  theme(legend.position = c(0.9, 0.2),
        legend.title=element_text(size=11),
        plot.title = element_text(size = 10),
        axis.text = element_text(size=12),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Lifespan benefit of saving a life the",~  ~italic(ith), "time", ~  ~ (H[i]*e[0]+e[0]))),
       x=TeX('Life Expectancy at Birth ($\\e_{0}$)'))+
  geom_point(data=lt_H_span_gain %>%
                    filter(country%in%c("Sweden","Italy","Czechia","Japan")&
                             Year%in%c(1950) & times2%in%5),color= "black", size=2)+
  geom_text_repel(data=lt_H_span_gain %>%
                    filter(country%in%c("Sweden","Italy","Czechia","Japan")&
                             Year%in%c(1950) & times2%in%5),
                  aes(label=country), color= "black", size=4,
                  vjust = 0.5,
                  nudge_x = 4,
                  nudge_y = -4)+
  geom_point(data=lt_H_span_gain %>%
               filter(country%in%c("Sweden","Italy","Czechia","Japan")&
                        Year%in%c(2010) & times2%in%5),color= "black", size=2)+
  geom_text_repel(data=lt_H_span_gain %>%
                    filter(country%in%c("Sweden","Italy","Czechia","Japan")&
                             Year%in%c(2010) & times2%in%5),
                  aes(label=country), color= "black", size=4,
                  vjust = 0.5,
                  nudge_x = -6.5,
                  nudge_y = 5)



# adding the gains

lt_h_all<-left_join(lt_H_gain, lt_H_span_gain)

  ben<-ggplot(data=lt_h_all,
         aes(ex, spans,color=times2))+
    geom_jitter(size=4, alpha=0.4)+
    scale_color_manual(values = c("#B35806", "#E08214", "#FEE0B6","#B2ABD2" ,"#8073AC"),
                         name="Revivorship times")+
   # scale_colour_brewer(palette = "Spectral",name="Revivorship times")+
    theme_pander(base_size = 14)+
    theme(legend.position = c(0.9, 0.3),
          legend.title=element_text(size=12),
          plot.title = element_text(size = 10),
          axis.text = element_text(size=12),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
    labs(y=expression(paste("Lifespan benefit of saving a life the",~  ~italic(ith), "time", ~  ~ (H[i]*e[0]+e[0]))),
         x=TeX('Life Expectancy at Birth ($\\e_{0}$)'))+
    facet_grid(.~Sex)



  mat<- ggplot(lt_h_all, aes(times2,Year, fill=gains))+
    geom_raster(interpolate = T) +
    scale_fill_gradientn(colors = hcl.colors(40, "RdYlGn"),
    name=expression(paste("Life Years gained",~  ~italic(ith), "time", ~  ~ (H[i]*e[0])))) +
    facet_grid(.~Sex)+
    theme_pander(base_size = 14)+
    theme(legend.position = "bottom",
          legend.title=element_text(size=12),
          plot.title = element_text(size = 10),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text = element_text(size=12),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
    xlab("Revivorship times")+
    coord_flip()

ggarrange(ben,mat, ncol = 1)

tab_gains<-lt_H_spans %>%
  filter(country%in%c("Sweden","Italy","Japan","Czechia")
         & Year%in%c(1850,1900,1950,2017)) %>%
  arrange(country,Year,Sex)

View(tab_gains)

write.table(tab_gains, here("Data","tau","gains.csv"), sep=",", row.names = F) # info for building
write.table(tab_gains, here("Review","Review_2","Tables","gains.csv"), sep=",", row.names = F)


# tab gains for all countries

tab_gains_all<-lt_H_spans %>%
  filter(Year%in%c(1850,1900,1950,2000,2017)) %>%
  arrange(country,Year,Sex)

View(tab_gains_all)

write.table(tab_gains, here("Data","tau","gains.csv"), sep=",", row.names = F) # info for building
write.table(tab_gains, here("Review","Review_2","Tables","gains.csv"), sep=",", row.names = F)
write.table(tab_gains_all, here("Review","Review_2","Tables","gains_all.csv"), sep=",", row.names = F)
