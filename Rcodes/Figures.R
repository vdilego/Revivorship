
library(paletteer)
library(rcartocolor)

# colors from Ilyia
pal <- paletteer_d("rcartocolor::Safe")
pal <- pal[c(3, 4, 1, 5, 2)]

pal_six <- c(
  "#084488", # [0, 1)
  "#3FB3F7", # [1,15)
  "#003737", # [15,40)
  "#268A8A", # [40,60)
  "#eec21f", # [60,80)
  "#A14500" # [80,111)
)

# theme also from Iliya
theme_custom <- theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.minor =  element_blank(),
    panel.grid.major =  element_line(size = .25),
    panel.ontop = T
  )


res_p_lag<-rbind(res_long_pioneer, res_long_lt_laggards, res_long_lt_fast, res_long_lt_east)


# saving table with res for ages 50 and 90
res_all_ap<-res_p_lag %>%
  filter(lambda>0 & Age%in%c(50,90)) %>%
  arrange(Age) %>%
  select(1:3,12:15,17:19,21:22) %>%
  pivot_wider(id_cols = c(1:10), names_from = i, values_from = res)

write.table(res_all_ap, here("Review","Review_2","Tables","res_all_50_90.csv"), sep=",", row.names = F) # info for building Table 1.


# for labels:

res_label_f<-res_p_lag %>%
  filter(Year_lab%in%c("1850-1900") & Sex%in%"Women")

res_label_m<-res_p_lag %>%
  filter(Year_lab%in%c("1850-1900")&Sex%in% "Men")

# summarizing averages for adding to the plot

res_max_age_pl<-res_p_lag %>%
#  drop_na() %>%
  group_by(Transition,Year_lab, Sex, i) %>%
  filter(lambda>0) %>%
    dplyr::summarize(max_res = max(res), age_res = Age[which(res == max(res))])

# for country

res_max_age_c<-res_p_lag %>%
  #  drop_na() %>%
  group_by(Transition,country,Year_lab, Sex, i) %>%
  filter(lambda>0 & country%in%c("Sweden","Italy","Japan","Czechia")) %>%
  dplyr::summarize(max_res = max(res), age_res = Age[which(res == max(res))])

# just selecting the age for the table

res_max_age_c_table<-res_p_lag %>%
  #  drop_na() %>%
  group_by(Transition,country,Year_lab, Sex, i) %>%
  filter(lambda>0 & country%in%c("Sweden","Italy","Japan","Czechia")) %>%
  dplyr::summarize(max_res = max(res), age_res = Age[which(res == max(res))]) %>%
  select(-6) #%>%
  pivot_wider(id_cols = c(1:3), names_from = c(Sex,i), values_from = age_res)

write.table(res_max_age_c_table, here("Review","Review_2","Tables","res_max_age.csv"), sep=",", row.names = F) # info for building Table 1.


X11()

# women max age
ggplot(data = res_max_age_c_table , aes(x = Year_lab, y = age_res, color = i)) +
 geom_point(size=6)+
  # geom_bar(stat="identity", position = "dodge") +
  coord_flip() +
 scale_color_manual(values = (pal_six  %>% rev)) +
  labs(title = 'Plot title', y = NULL, x = NULL) +
  theme(legend.position = "bottom")+
  facet_grid(Sex~country)+
theme_minimal(base_size = 24)+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    legend.title  = element_text(size=16))

 library(ggalt)

# filter

res_i<-res_max_age_c_table %>%
  filter(i%in% "1" & Sex%in%"Women")

res_i2<-res_max_age_c_table %>%
  filter(i%in%"3+" & Sex%in%"Women")

 ggplot(res_max_age_c_table %>% filter(Sex%in%"Women"))+

  geom_segment(data = res_i,
               aes(x = age_res , y = Year_lab,
                   yend = res_i$Year_lab, xend = res_i2$age_res), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 7, #Note that I sized the segment to fit the points
               alpha = .5) +
  facet_wrap(country~.)+
   geom_point(aes(x = age_res, y = Year_lab, color = i), size = 10, show.legend = F)+
   scale_color_manual(name="Revivorship times",values = (pal_six  %>% rev))+
   theme_minimal(base_size = 30)+
   theme(
     legend.position = "none",
     panel.grid.minor = element_blank(),
    # axis.title.y = element_blank(),
     #axis.text.x = element_blank(),
     axis.title.x = element_blank())+
   #  theme(legend.position = "bottom",
   #        strip.text.x = element_blank(),
   #        legend.title=element_text(size=12),
   #        plot.title = element_text(size = 12),
   #        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
   #        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
 #  guides(linetype=guide_legend(keywidth = 5, keyheight = 2))+
   scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
   labs(y="Peak Age of Revival")

 # now for males

 res_i<-res_max_age_c_table %>%
   filter(i%in% "1" & Sex%in%"Men")

 res_i2<-res_max_age_c_table %>%
   filter(i%in%"3+" & Sex%in%"Men")

 ggplot(res_max_age_c_table %>% filter(Sex%in%"Men"))+

   geom_segment(data = res_i,
                aes(x = age_res , y = Year_lab,
                    yend = res_i$Year_lab, xend = res_i2$age_res), #use the $ operator to fetch data from our "Females" tibble
                color = "#aeb6bf",
                size = 7, #Note that I sized the segment to fit the points
                alpha = .5) +
   facet_wrap(country~.)+
   geom_point(aes(x = age_res, y = Year_lab, color = i), size = 10, show.legend = F)+
   scale_color_manual(name="Revivorship times",values = (pal_six  %>% rev))+
   theme_minimal(base_size = 30)+
   theme(
     legend.position = "none",
     panel.grid.minor = element_blank(),
     # axis.title.y = element_blank(),
     #axis.text.x = element_blank(),
     axis.title.x = element_blank())+
   #  theme(legend.position = "bottom",
   #        strip.text.x = element_blank(),
   #        legend.title=element_text(size=12),
   #        plot.title = element_text(size = 12),
   #        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
   #        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
   #  guides(linetype=guide_legend(keywidth = 5, keyheight = 2))+
   scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
   labs(y="Peak Age of Revival")



# selecting the for the survival and lambdas
res_table_main_50<-res_p_lag %>%
  #  drop_na() %>%
  group_by(Transition,country,Year_lab, Sex, i) %>%
  filter(lambda>0 & country%in%c("Sweden","Italy","Japan","Czechia") & Age%in%c(50)) %>%
  select(1:3,12:15,17:19,21:23)%>%
  pivot_wider(id_cols = c(1:10), names_from = c(i), values_from = c(res,res_prop)) %>%
  arrange(country)

write.table(res_table_main_50, here("Review","Review_2","Tables","res_table_main_50.csv"), sep=",", row.names = F) # info for building Table 1.

# only age 90

res_table_main_90<-res_p_lag %>%
  #  drop_na() %>%
  group_by(Transition,country,Year_lab, Sex, i) %>%
  filter(lambda>0 & country%in%c("Sweden","Italy","Japan","Czechia") & Age%in%c(90)) %>%
  select(1:3,12:15,17:19,21:23)%>%
  pivot_wider(id_cols = c(1:10), names_from = c(i), values_from = c(res,res_prop)) %>%
  arrange(country)

write.table(res_table_main_90, here("Review","Review_2","Tables","res_table_main_90.csv"), sep=",", row.names = F) # info for building Table 1.


fig_w<-ggplot()+
  # geom_jitter(data=res_p_lag%>%
  #                filter(Age<=89 & lambda>0),
  #              aes(Age,(res),color=Sex, group=country,linetype=i), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_p_lag%>%
              filter(Age<=89 & lambda>0  & Sex %in%c("Women")),
            aes(Age,(res),color=i, group=i), size=2)+


  geom_point(data=res_max_age_c %>% filter(Sex %in%c("Women")),
             aes(age_res,(max_res),color=i,shape=country, group=Transition), size=4, color="darkblue")+

#  geom_point(data=res_p_lag%>%
#              filter(Age<=89 & lambda>0  & Sex%in%c("Women") & country%in%("Sweden")),
#            aes(Age,(res),color=i, group=i, shape=country), size=2, color="black")+
  #geom_point(data=res_p_lag%>%
  #            filter(Age<=89 & lambda>0  & Sex%in%c("Women") & country%in%("Italy")),
   #         aes(Age,(res),color=i, group=i, shape=country), size=2, color="black")+

#  geom_point(data=res_p_lag%>%
#              filter(Age<=89 & lambda>0  & Sex%in%c("Women") & country%in%("Japan")),
#            aes(Age,(res),color=i, group=i, shape=country), size=2, color="black")+
 # geom_point(data=res_p_lag%>%
#              filter(Age<=89 & lambda>0  & Sex%in%c("Women") & country%in%("Czechia")),
#            aes(Age,(res),color=i, group=i, shape=country), size=2, color="black")+

  scale_color_manual(name="Revivorship times",values = (pal_six  %>% rev))+
  # scale_color_manual(values = c("#B35806","#8073AC",  "#FEE0B6" ),
  #                       name="Revivorship times")+
  # scale_color_manual(values = rev(suf_palette("hanwell", n = 3,type = "discrete")))+
  facet_grid(Transition~Year_lab)+
  theme_minimal(base_size = 24)+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    legend.title  = element_text(size=16))+
  #  theme(legend.position = "bottom",
  #        strip.text.x = element_blank(),
  #        legend.title=element_text(size=12),
  #        plot.title = element_text(size = 12),
  #        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
  #        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
 labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 5, keyheight = 2))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90),  expand = expansion(mult = 0.1))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))+
  geom_text(x = 20,  y = 27000,
            label = "Women",
            colour = "black", data=res_label_f, size=8)#+


 # geom_text_repel(aes(age_res,max_res,label = round(age_res,1)),
     #            data = res_max_age_c %>%
     #               filter( Sex %in%c("Women")),
  #                size = 5,
  #                nudge_x = 10,
     #             nudge_y =30,
#  #              #  force=2,
  #               point.padding = 1.8,
   #               direction         = "y",
   #              lineheight = 2,
      #           color="black",
  #                box.padding = 1)


#
ggsave(here("Review","Review_2","Figures","fig_w.png"), width = 33, height = 27, units = "cm", dpi=400)
#

ggsave(here("Review","Review_2","Figures","fig_w.pdf"), width = 33, height = 27, units = "cm", dpi=400)


fig_m<-ggplot()+
  # geom_jitter(data=res_p_lag%>%
  #                filter(Age<=89 & lambda>0),
  #              aes(Age,(res),color=Sex, group=country,linetype=i), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_p_lag%>%
              filter(Age<=89 & lambda>0  & Sex %in%c("Men")),
            aes(Age,(res),color=i, group=i), size=2)+

  geom_point(data=res_max_age_c %>% filter(Sex %in%c("Men")),
             aes(age_res,(max_res),color=i,shape=country, group=Transition), size=4, color="darkblue")+

  # geom_line(data=res_p_lag%>%
  #            filter(Age<=89 & lambda>0  & Sex%in%c("Men") & country%in%("Sweden")),
#            aes(Age,(res),color=i, group=i, linetype=country), size=1.3, color="black")+
#  geom_line(data=res_p_lag%>%
#              filter(Age<=89 & lambda>0  & Sex%in%c("Men") & country%in%("Italy")),
#            aes(Age,(res),color=i, group=i, linetype=country), size=1.3, color="black")+


   scale_color_manual(name="Revivorship times",values = (pal_six  %>% rev))+
# scale_color_manual(values = c("#B35806","#8073AC",  "#FEE0B6" ),
#                       name="Revivorship times")+
  # scale_color_manual(values = rev(suf_palette("hanwell", n = 3,type = "discrete")))+
  facet_grid(Transition~Year_lab)+
  theme_minimal(base_size = 24)+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
  #  axis.title.y = element_blank(),
  #  strip.text.x = element_blank(),
    legend.title  = element_text(size=16))+
#  theme(legend.position = "bottom",
#        strip.text.x = element_blank(),
#        legend.title=element_text(size=12),
#        plot.title = element_text(size = 12),
#        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
#        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90),  expand = expansion(mult = 0.1))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))+
  geom_text(x = 13,  y = 27000,
            label = "Men",
            colour = "black", data=res_label_m, size=8)#+
#  geom_text_repel(aes(age_res,max_res,label = round(age_res,1)),
#                 data = res_max_age_c %>%
#                    filter( Sex %in%c("Men")),
#                  size = 5,
#                  nudge_x = 20,
#                  nudge_y =10,
#                   force=2,
#                  point.padding = 1,
#                  direction         = "y",
#                 lineheight = 2,
#                  color="black",
#                  box.padding = 1.6)


ggsave(here("Review","Review_2","Figures","fig_m.pdf"), width = 33, height = 27, units = "cm", dpi=400)



library(ggpubr)


fig_b<-ggpubr::ggarrange(fig_w,fig_m,common.legend = T, ncol = 1, legend = "bottom")

annotate_figure(fig_b,
                left = text_grob(expression(paste("Number of Survivors Revived ", ~ (lx[i]))),
                                 rot = 90,  size = 20),
                )

# saving with higher resolution
ggsave(here("Review","Review_2","Figures","fig_m.png"), width = 33, height = 27, units = "cm", dpi=400)




ggsave(here("Review","Review_2","Figures","fig_w.png"), width = 33, height = 27, units = "cm", dpi=400)


# now for the east transitioners and the fast-paced

res_ef_lag<-rbind(res_long_lt_fast, res_long_lt_east)

# for labels:

res_label_f_ef<-res_ef_lag %>%
  filter(Year_lab%in%c("1950-2000") & Sex%in%"Women" & Transition%in%"Fast-Paced")

res_label_m_ef<-res_ef_lag %>%
  filter(Year_lab%in%c("1950-2000")&Sex%in% "Men"& Transition%in%"Fast-Paced")

# summarizing averages for adding to the plot

res_max_age_ef<-res_ef_lag %>%
  #  drop_na() %>%
  group_by(Transition,Year_lab, Sex, i) %>%
  filter(lambda>0) %>%
  dplyr::summarize(max_res = max(res), age_res = Age[which(res == max(res))])


fig_w_ef<-ggplot()+
  # geom_jitter(data=res_p_lag%>%
  #                filter(Age<=89 & lambda>0),
  #              aes(Age,(res),color=Sex, group=country,linetype=i), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_ef_lag%>%
              filter(Age<=89 & lambda>0  & Sex %in%c("Women")),
            aes(Age,(res),color=i, group=i), size=4)+
  geom_line(data=res_ef_lag%>%
              filter(Age<=89 & lambda>0  & Sex%in%c("Women") & country%in%("Japan")),
            aes(Age,(res),color=i, group=i, linetype=country), size=1.3, color="black")+
  geom_line(data=res_ef_lag%>%
              filter(Age<=89 & lambda>0  & Sex%in%c("Women") & country%in%("Czechia")),
            aes(Age,(res),color=i, group=i, linetype=country), size=1.3, color="black")+

  scale_color_manual(name="Revivorship times",values = (pal_six  %>% rev))+
  # scale_color_manual(values = c("#B35806","#8073AC",  "#FEE0B6" ),
  #                       name="Revivorship times")+
  # scale_color_manual(values = rev(suf_palette("hanwell", n = 3,type = "discrete")))+
  facet_grid(Transition~Year_lab)+
  theme_minimal(base_size = 24)+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank() )+
  #  theme(legend.position = "bottom",
  #        strip.text.x = element_blank(),
  #        legend.title=element_text(size=12),
  #        plot.title = element_text(size = 12),
  #        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
  #        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  # labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))+
  geom_text(x = 20,  y = 25000,
            label = "Women",
            colour = "black", data=res_label_f_ef, size=8)


#

fig_m_ef<-ggplot()+
  # geom_jitter(data=res_p_lag%>%
  #                filter(Age<=89 & lambda>0),
  #              aes(Age,(res),color=Sex, group=country,linetype=i), size=3, alpha=0.1, color="grey80")+

  geom_line(data=res_ef_lag%>%
              filter(Age<=89 & lambda>0  & Sex %in%c("Men")),
            aes(Age,(res),color=i, group=i), size=4.5)+
  geom_line(data=res_ef_lag%>%
              filter(Age<=89 & lambda>0  & Sex%in%c("Men") & country%in%("Japan")),
            aes(Age,(res),color=i, group=i, linetype=country), size=1.3, color="black")+
  geom_line(data=res_ef_lag%>%
              filter(Age<=89 & lambda>0  & Sex%in%c("Men") & country%in%("Czechia")),
            aes(Age,(res),color=i, group=i, linetype=country), size=1.3, color="black")+


  scale_color_manual(name="Revivorship times",values = (pal_six  %>% rev))+
  # scale_color_manual(values = c("#B35806","#8073AC",  "#FEE0B6" ),
  #                       name="Revivorship times")+
  # scale_color_manual(values = rev(suf_palette("hanwell", n = 3,type = "discrete")))+
  facet_grid(Transition~Year_lab)+
  theme_minimal(base_size = 24)+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    strip.text.x = element_blank())+
  #  theme(legend.position = "bottom",
  #        strip.text.x = element_blank(),
  #        legend.title=element_text(size=12),
  #        plot.title = element_text(size = 12),
  #        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
  #        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  # labs(y=expression(paste("Number of Survivors Revived ", ~ (lx[i]))))+
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_continuous(breaks=c(0,15,30,45,60,75,90))+
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000,25000,30000))+
  geom_text(x = 13,  y = 25000,
            label = "Men",
            colour = "black", data=res_label_m_ef, size=8)

library(ggpubr)


fig_b_ef<-ggpubr::ggarrange(fig_w_ef,fig_m_ef,common.legend = T, ncol = 1, legend = "bottom")

annotate_figure(fig_b_ef,
                left = text_grob(expression(paste("Number of Survivors Revived ", ~ (lx[i]))),
                                 rot = 90,  size = 20),
)

# with all




fig_b_all<-ggpubr::ggarrange(fig_w,fig_m, fig_w_ef,fig_m_ef,common.legend = T, ncol = 1, legend = "bottom")

annotate_figure(fig_b_all,
                left = text_grob(expression(paste("Number of Survivors Revived ", ~ (lx[i]))),
                                 rot = 90,  size = 20),
)

# saving the tables with the ages

write.table(res_max_age_pl_wide, here("Review","Review_2","Tables","res_max_pl.csv"), sep=",", row.names = F)

write.table(res_max_age_ef_wide, here("Review","Review_2","Tables","res_max_ef.csv"), sep=",", row.names = F)


# wide format
res_max_age_pl_wide<-res_max_age_pl %>%
  pivot_wider(id_cols = c(Transition, Year_lab,Sex), names_from = i, values_from = c(max_res,age_res) ) %>%
  arrange(Sex)

res_max_age_ef_wide<-res_max_age_ef %>%
  pivot_wider(id_cols = c(Transition, Year_lab,Sex), names_from = i, values_from = c(max_res,age_res) ) %>%
  arrange(Sex)
