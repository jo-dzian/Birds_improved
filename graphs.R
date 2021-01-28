
# bar plots showing IHA indicator values during 2004-2018
#bh.gull_list_year_mean z Q_IHA_b.gull
#m.gull_list_year_mean z Q_IHA_m.gull
#tern_list_year_mean z Q_IHA_tern


library(data.table)
library(ggplot2)

all_unlist_year_mean <- bind_rows("mew gull" = m.gull_list_year_mean[ ,1:16],
                   "black headed gull" = bh.gull_list_year_mean[ ,1:16],  
                    "little tern" = tern_list_year_mean[ ,1:16], 
                         .id = "bird")


all_unlist_year_mean_1 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.1_mean_LE, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.1 mean streamflow during laying eggs") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_1a <-all_unlist_year_mean_1 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_1 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                       "all_unlist_year_mean_1.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_2 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.1_mean_Incub, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.1 mean streamflow during incubation") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_2a <-all_unlist_year_mean_2 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_2 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_2.jpg"),device = "jpg", width = 10, height = 5)
                

all_unlist_year_mean_3 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.1_mean_RC, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.1 mean streamflow during rearing chicks") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_3a <-all_unlist_year_mean_3 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_3 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_3.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_4 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day01_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.2 one day rolling mean of streamflow minimum") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_4a <-all_unlist_year_mean_4 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_4 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_4.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_5 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day03_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.2 three day rolling mean of streamflow minimum") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_5a <-all_unlist_year_mean_5 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_5 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_5.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_6 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day07_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.2 seven day rolling mean of streamflow minimum") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_6a <-all_unlist_year_mean_6 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_6 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_6.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_7 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day01_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.2 one day rolling mean of streamflow maximum") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_7a <-all_unlist_year_mean_7 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_7 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_7.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_8 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day03_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.2 three day rolling mean of streamflow maximum") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_8a <-all_unlist_year_mean_8 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_8 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_8.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_9 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.2_day07_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.2 seven day rolling mean of streamflow maximum") +
  ylab ("Streamflow m3/s")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))
all_unlist_year_mean_9a <-all_unlist_year_mean_9 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_9 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_9.jpg"),device = "jpg", width = 10, height = 5)


## same 000
all_unlist_year_mean_10 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.3_vp_min, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.3 Yearly minimum flow falling within the\n vulnerability period") +
  ylab ("1=Yes, 0=No")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
ggsave(all_unlist_year_mean_10 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_10.jpg"),device = "jpg", width = 10, height = 5)

all_unlist_year_mean_11 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.3_vp_max, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.3 Yearly maximum flow falling within the\n vulnerability period") +
  ylab ("1=Yes, 0=No")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
all_unlist_year_mean_11a <-all_unlist_year_mean_11 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_11 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_11.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_12 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_P0.95, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.4 Mean number of days during the vulnerability\n period when flows > 0.95 percentile") +
  ylab ("number of days")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
all_unlist_year_mean_12a <-all_unlist_year_mean_12 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_12 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_12.jpg"),device = "jpg", width = 10, height = 5)


all_unlist_year_mean_13 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_Q3, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.4 Mean number of days during the vulnerability\n period when flows > 0.75 percentile") +
  ylab ("number of days")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
all_unlist_year_mean_13a <-all_unlist_year_mean_13 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_13 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_13.jpg"),device = "jpg", width = 10, height = 5)



all_unlist_year_mean_14 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_Q1, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.4 Mean number of days during the vulnerability\n period when flows < 0.25 percentile") +
  ylab ("number of days")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
all_unlist_year_mean_14a <-all_unlist_year_mean_14 + theme(legend.position="none") ### remove legend
ggsave(all_unlist_year_mean_14 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_14.jpg"),device = "jpg", width = 10, height = 5)


## same 000
all_unlist_year_mean_15 <- ggplot(data=all_unlist_year_mean, aes(x= Year, y=gr.4_P0.05, fill = bird)) +
  geom_bar( position="dodge", stat="identity", width=.6) +
  ggtitle("Gr.4 Mean number of days during the vulnerability\n period when flows < 0.05 percentile")+
  ylab ("number of days")+
  scale_fill_manual(values=c("seagreen4", "orangered2", "orchid4"))+ 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 1))
ggsave(all_unlist_year_mean_15 , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_15.jpg"),device = "jpg", width = 10, height = 5)

# join all the graphs together without no.10 and 15 as they have only 000 values
library("ggpubr")
all_unlist_year_mean_plot <- ggarrange(
              all_unlist_year_mean_1a, all_unlist_year_mean_2a, all_unlist_year_mean_3a,
              all_unlist_year_mean_4a, all_unlist_year_mean_5a, all_unlist_year_mean_6a,
              all_unlist_year_mean_7a, all_unlist_year_mean_8a, all_unlist_year_mean_9a,
              all_unlist_year_mean_11a, all_unlist_year_mean_12a,
              all_unlist_year_mean_13a, all_unlist_year_mean_14, 
             nrow = 5, ncol = 3)


ggsave(all_unlist_year_mean_plot , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_unlist_year_mean_plot.jpg"),device = "jpg", width = 20, height = 10)




############################################
############################################
#*******   Absolute values ***********#
#*******    BOX PLOTS      ***********#
###########################################
############################################


############################################
#*******   Mew gull   ***********#
###########################################

#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.1 Laying eggs

m_gull_gr.1_le_absolute_list <- list(m.gull_ref_le_gr.1_unlist, m.gull_nf_4.5_le_gr.1_unlist, m.gull_ff_4.5_le_gr.1_unlist,
                                        m.gull_nf_8.5_le_gr.1_unlist, m.gull_ff_8.5_le_gr.1_unlist)

names(m_gull_gr.1_le_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.1_le_absolute <- rbindlist(m_gull_gr.1_le_absolute_list, id="id_scen")

m_gull_gr.1_le_absolute$id_scen <- factor(m_gull_gr.1_le_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.1_le_absolute_graph <- ggplot(m_gull_gr.1_le_absolute, aes(x=id_scen, y=mean_le)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("m.gull Gr.1 laying eggs")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())



#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.1 Incub

m_gull_gr.1_incub_absolute_list <- list(m.gull_ref_incub_gr.1_unlist, m.gull_nf_4.5_incub_gr.1_unlist, m.gull_ff_4.5_incub_gr.1_unlist,
                                         m.gull_nf_8.5_incub_gr.1_unlist, m.gull_ff_8.5_incub_gr.1_unlist)

names(m_gull_gr.1_incub_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.1_incub_absolute <- rbindlist(m_gull_gr.1_incub_absolute_list, id="id_scen")

m_gull_gr.1_incub_absolute$id_scen <- factor(m_gull_gr.1_incub_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.1_incub_absolute_graph <- ggplot(m_gull_gr.1_incub_absolute, aes(x=id_scen, y=mean_incub)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("m.gull Gr.1 Incub")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())


#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.1 Rear

m_gull_gr.1_rear_absolute_list <- list(m.gull_ref_rear_gr.1_unlist, m.gull_nf_4.5_rear_gr.1_unlist, m.gull_ff_4.5_rear_gr.1_unlist,
                                        m.gull_nf_8.5_rear_gr.1_unlist, m.gull_ff_8.5_rear_gr.1_unlist)

names(m_gull_gr.1_rear_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.1_rear_absolute <- rbindlist(m_gull_gr.1_rear_absolute_list, id="id_scen")

m_gull_gr.1_rear_absolute$id_scen <- factor(m_gull_gr.1_rear_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.1_rear_absolute_graph <- ggplot(m_gull_gr.1_rear_absolute, aes(x=id_scen, y=mean_rear)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("m.gull Gr.1 rear")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.2

m_gull_gr.2_absolute_list <- list(m.gull_ref_gr.2_unlist_p2, m.gull_nf_4.5_gr.2_unlist_p2,  m.gull_ff_4.5_gr.2_unlist_p2, 
                                   m.gull_nf_8.5_gr.2_unlist_p2, m.gull_ff_8.5_gr.2_unlist_p2)

names(m_gull_gr.2_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.2_absolute <- rbindlist(m_gull_gr.2_absolute_list, id="id_scen")

m_gull_gr.2_absolute$id_scen <- factor(m_gull_gr.2_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.2_absolute <- subset(m_gull_gr.2_absolute, select=-c(5,6,8,9,11,12))

m_gull_gr.2_01max_absolute_graph <- ggplot(m_gull_gr.2_absolute, aes(x=id_scen, y=day01_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("m.gull Gr.2 day_01max")+
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

m_gull_gr.2_03max_absolute_graph <- ggplot(m_gull_gr.2_absolute, aes(x=id_scen, y=day03_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("m.gull Gr.2 day_03max")+
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

m_gull_gr.2_07max_absolute_graph <- ggplot(m_gull_gr.2_absolute, aes(x=id_scen, y=day07_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("m.gull Gr.2 day_07max")+
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# Mew-Gull absolute values box_plot GR.4

m_gull_gr.4_absolute_list <- list(m.gull_ref_gr.4_p2, m.gull_nf_4.5_gr.4_p2, m.gull_ff_4.5_gr.4_p2,
                                   m.gull_nf_8.5_gr.4_p2, m.gull_ff_8.5_gr.4_p2)

names(m_gull_gr.4_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

m_gull_gr.4_absolute <- rbindlist(m_gull_gr.4_absolute_list, id="id")

m_gull_gr.4_absolute$id <- factor(m_gull_gr.4_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

m_gull_gr.4_absolute_graph <- ggplot(m_gull_gr.4_absolute, aes(x=id, y=yearly)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="days above 0.75 percentile") +
  ggtitle("m.gull Gr.4 >0.75")+
  theme(axis.title.x = element_blank())



# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

m_gull_box_plot_absolute <- ggarrange(m_gull_gr.1_le_absolute_graph, m_gull_gr.1_incub_absolute_graph, 
                                      m_gull_gr.1_rear_absolute_graph, m_gull_gr.2_01max_absolute_graph, 
                                      m_gull_gr.2_03max_absolute_graph, m_gull_gr.2_07max_absolute_graph, 
                                      m_gull_gr.4_absolute_graph,
                                       
                                       nrow = 3, ncol = 3)


ggsave(m_gull_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "m_gull_box_plot_absolute.jpg"),device = "jpg", width = 20, height = 10)




############################################
#*******   Black- headed gull   ***********#
###########################################

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.1 Incub

bh_gull_gr.1_incub_absolute_list <- list(bh.gull_ref_incub_gr.1_unlist, bh.gull_nf_4.5_incub_gr.1_unlist, bh.gull_ff_4.5_incub_gr.1_unlist,
                                   bh.gull_nf_8.5_incub_gr.1_unlist, bh.gull_ff_8.5_incub_gr.1_unlist)

names(bh_gull_gr.1_incub_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.1_incub_absolute <- rbindlist(bh_gull_gr.1_incub_absolute_list, id="id_scen")

bh_gull_gr.1_incub_absolute$id_scen <- factor(bh_gull_gr.1_incub_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.1_incub_absolute_graph <- ggplot(bh_gull_gr.1_incub_absolute, aes(x=id_scen, y=mean_incub)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("BH.gull Gr.1 Incub")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.1 Rear

bh_gull_gr.1_rear_absolute_list <- list(bh.gull_ref_rear_gr.1_unlist, bh.gull_nf_4.5_rear_gr.1_unlist, bh.gull_ff_4.5_rear_gr.1_unlist,
                                         bh.gull_nf_8.5_rear_gr.1_unlist, bh.gull_ff_8.5_rear_gr.1_unlist)

names(bh_gull_gr.1_rear_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.1_rear_absolute <- rbindlist(bh_gull_gr.1_rear_absolute_list, id="id_scen")

bh_gull_gr.1_rear_absolute$id_scen <- factor(bh_gull_gr.1_rear_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.1_rear_absolute_graph <- ggplot(bh_gull_gr.1_rear_absolute, aes(x=id_scen, y=mean_rear)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("BH.gull Gr.1 rear") +
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.2

bh_gull_gr.2_absolute_list <- list(bh.gull_ref_gr.2_unlist_p2, bh.gull_nf_4.5_gr.2_unlist_p2,  bh.gull_ff_4.5_gr.2_unlist_p2, 
                                        bh.gull_nf_8.5_gr.2_unlist_p2, bh.gull_ff_8.5_gr.2_unlist_p2)

names(bh_gull_gr.2_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.2_absolute <- rbindlist(bh_gull_gr.2_absolute_list, id="id_scen")

bh_gull_gr.2_absolute$id_scen <- factor(bh_gull_gr.2_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.2_absolute <- subset(bh_gull_gr.2_absolute, select=-c(5,6,8,9,11,12))

bh_gull_gr.2_01max_absolute_graph <- ggplot(bh_gull_gr.2_absolute, aes(x=id_scen, y=day01_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("BH.gull Gr.2 day_01max") +
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

bh_gull_gr.2_03max_absolute_graph <- ggplot(bh_gull_gr.2_absolute, aes(x=id_scen, y=day03_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("BH.gull Gr.2 day_03max") +
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

bh_gull_gr.2_07max_absolute_graph <- ggplot(bh_gull_gr.2_absolute, aes(x=id_scen, y=day07_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("BH.gull Gr.2 day_07max")+
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())


#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.3

bh_gull_gr.3_absolute_list <- list(bh.gull_ref_list_gr.3_unlist_p1, bh.gull_nf_4.5_list_gr.3_unlist_p1,
                                   bh.gull_ff_4.5_list_gr.3_unlist_p1, bh.gull_nf_8.5_list_gr.3_unlist_p1,
                                   bh.gull_ff_8.5_list_gr.3_unlist_p1)

names(bh_gull_gr.3_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.3_absolute <- rbindlist(bh_gull_gr.3_absolute_list, id="id")

bh_gull_gr.3_absolute$id <- factor(bh_gull_gr.3_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.3_absolute_graph <- ggplot(bh_gull_gr.3_absolute, aes(x=id, y=above_vp_max_period)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Max. falling within VP 1=Yes 0=No") +
  ggtitle("BH.gull Gr.3")+
  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# BH-Gull absolute values box_plot GR.4

bh_gull_gr.4_absolute_list <- list(bh.gull_ref_gr.4_p2, bh.gull_nf_4.5_gr.4_p2, bh.gull_ff_4.5_gr.4_p2,
                              bh.gull_nf_8.5_gr.4_p2, bh.gull_ff_8.5_gr.4_p2)

names(bh_gull_gr.4_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

bh_gull_gr.4_absolute <- rbindlist(bh_gull_gr.4_absolute_list, id="id")

bh_gull_gr.4_absolute$id <- factor(bh_gull_gr.4_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

bh_gull_gr.4_absolute_graph <- ggplot(bh_gull_gr.4_absolute, aes(x=id, y=yearly)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="days above 0.95 percentile") +
  ggtitle("BH.gull Gr.4 >0.95")+
  theme(axis.title.x = element_blank())



# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

bh_gull_box_plot_absolute <- ggarrange(bh_gull_gr.1_incub_absolute_graph, bh_gull_gr.1_rear_absolute_graph,
                                       bh_gull_gr.3_absolute_graph, bh_gull_gr.2_01max_absolute_graph, 
                                       bh_gull_gr.2_03max_absolute_graph, bh_gull_gr.2_07max_absolute_graph, 
                                       bh_gull_gr.4_absolute_graph,
  
  nrow = 3, ncol = 3)


ggsave(bh_gull_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "bh_gull_box_plot_absolute1.jpg"),device = "jpg", width = 20, height = 10)


############################################
#*******   Little Tern   ***********#
###########################################

#######%%%%%%%%%%
# Little tern absolute values box_plot GR.1 Laying eggs

tern_gr.1_le_absolute_list <- list(tern_ref_le_gr.1_unlist, tern_nf_4.5_le_gr.1_unlist, tern_ff_4.5_le_gr.1_unlist,
                                     tern_nf_8.5_le_gr.1_unlist, tern_ff_8.5_le_gr.1_unlist)

names(tern_gr.1_le_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.1_le_absolute <- rbindlist(tern_gr.1_le_absolute_list, id="id_scen")

tern_gr.1_le_absolute$id_scen <- factor(tern_gr.1_le_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.1_le_absolute_graph <- ggplot(tern_gr.1_le_absolute, aes(x=id_scen, y=mean_le)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("tern Gr.1 laying eggs")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())



#######%%%%%%%%%%
# Little tern absolute values box_plot GR.1 Incub

tern_gr.1_incub_absolute_list <- list(tern_ref_incub_gr.1_unlist, tern_nf_4.5_incub_gr.1_unlist, tern_ff_4.5_incub_gr.1_unlist,
                                        tern_nf_8.5_incub_gr.1_unlist, tern_ff_8.5_incub_gr.1_unlist)

names(tern_gr.1_incub_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.1_incub_absolute <- rbindlist(tern_gr.1_incub_absolute_list, id="id_scen")

tern_gr.1_incub_absolute$id_scen <- factor(tern_gr.1_incub_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.1_incub_absolute_graph <- ggplot(tern_gr.1_incub_absolute, aes(x=id_scen, y=mean_incub)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())


#######%%%%%%%%%%
# Little tern absolute values box_plot GR.1 Rear

tern_gr.1_rear_absolute_list <- list(tern_ref_rear_gr.1_unlist, tern_nf_4.5_rear_gr.1_unlist, tern_ff_4.5_rear_gr.1_unlist,
                                       tern_nf_8.5_rear_gr.1_unlist, tern_ff_8.5_rear_gr.1_unlist)

names(tern_gr.1_rear_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.1_rear_absolute <- rbindlist(tern_gr.1_rear_absolute_list, id="id_scen")

tern_gr.1_rear_absolute$id_scen <- factor(tern_gr.1_rear_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.1_rear_absolute_graph <- ggplot(tern_gr.1_rear_absolute, aes(x=id_scen, y=mean_rear)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("tern Gr.1 rear")+
  coord_cartesian( ylim = c(400, 1200))+
  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# Little tern absolute values box_plot GR.2

tern_gr.2_absolute_list <- list(tern_ref_gr.2_unlist_p2, tern_nf_4.5_gr.2_unlist_p2,  tern_ff_4.5_gr.2_unlist_p2, 
                                  tern_nf_8.5_gr.2_unlist_p2, tern_ff_8.5_gr.2_unlist_p2)

names(tern_gr.2_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.2_absolute <- rbindlist(tern_gr.2_absolute_list, id="id_scen")

tern_gr.2_absolute$id_scen <- factor(tern_gr.2_absolute$id_scen , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.2_absolute <- subset(tern_gr.2_absolute, select=-c(5,6,8,9,11,12))

tern_gr.2_01max_absolute_graph <- ggplot(tern_gr.2_absolute, aes(x=id_scen, y=day01_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_01max") +
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

tern_gr.2_03max_absolute_graph <- ggplot(tern_gr.2_absolute, aes(x=id_scen, y=day03_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_03max") +
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

tern_gr.2_07max_absolute_graph <- ggplot(tern_gr.2_absolute, aes(x=id_scen, y=day07_mean)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_07max")+
  coord_cartesian( ylim = c(900, 2200))+
  theme(axis.title.x = element_blank())

#######%%%%%%%%%%
# Little tern absolute values box_plot GR.4

tern_gr.4_absolute_list <- list(tern_ref_gr.4_p2, tern_nf_4.5_gr.4_p2, tern_ff_4.5_gr.4_p2,
                                  tern_nf_8.5_gr.4_p2, tern_ff_8.5_gr.4_p2)

names(tern_gr.4_absolute_list) <- c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5")

tern_gr.4_absolute <- rbindlist(tern_gr.4_absolute_list, id="id")

tern_gr.4_absolute$id <- factor(tern_gr.4_absolute$id , levels=c("ref","nf_4.5", "ff_4.5", "nf_8.5", "ff_8.5"))

tern_gr.4_absolute_graph <- ggplot(tern_gr.4_absolute, aes(x=id, y=yearly)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="days above 0.75 percentile") +
  ggtitle("tern Gr.4 >0.75")+
  theme(axis.title.x = element_blank())



# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

tern_box_plot_absolute <- ggarrange(tern_gr.1_le_absolute_graph, tern_gr.1_incub_absolute_graph, 
                                      tern_gr.1_rear_absolute_graph, tern_gr.2_01max_absolute_graph, 
                                      tern_gr.2_03max_absolute_graph, tern_gr.2_07max_absolute_graph, 
                                      tern_gr.4_absolute_graph,
                                      
                                      nrow = 3, ncol = 3)


ggsave(tern_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "tern_box_plot_absolute.jpg"),device = "jpg", width = 20, height = 10)

######## ALL 3 BIRDS ########
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

all_box_plot_absolute <- ggarrange(m_gull_gr.1_le_absolute_graph, m_gull_gr.1_incub_absolute_graph, 
                                   m_gull_gr.1_rear_absolute_graph, m_gull_gr.2_03max_absolute_graph,  
                                   m_gull_gr.4_absolute_graph,
                                   bh_gull_gr.1_incub_absolute_graph, bh_gull_gr.1_rear_absolute_graph,
                                   bh_gull_gr.3_absolute_graph, bh_gull_gr.2_03max_absolute_graph, 
                                   bh_gull_gr.4_absolute_graph,
                                    tern_gr.1_le_absolute_graph, tern_gr.1_incub_absolute_graph, 
                                    tern_gr.1_rear_absolute_graph, tern_gr.2_03max_absolute_graph,  
                                    tern_gr.4_absolute_graph,
                                    
                                   nrow = 3, ncol = 5)


ggsave(all_box_plot_absolute , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_absolute.jpg"),device = "jpg", width = 20, height = 10)



############################################
############################################
#*******   % CHANGE        ***********#
#*******    BOX PLOTS      ***********#
###########################################
# % increase = (Future/Original Number - 1) × 100
############################################



############################################
#*******   Mew gull   ***********#
###########################################

#######%%%%%%%%%%
# Mew-Gull percentage change in values box_plot GR.1 Laying eggs

m_gull_gr.1_le_perc_chang_in <- m.gull_ref_le_gr.1_unlist %>% 
  right_join(m.gull_nf_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_le_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
m_gull_gr.1_le_perc_chang_in <- m_gull_gr.1_le_perc_chang_in[-c(2,5,7,9,11)]

names(m_gull_gr.1_le_perc_chang_in) <- c("model", "subbasin", "ref",
                                             "abs_nf_4.5",  "abs_ff_4.5", 
                                             "abs_nf_8.5",  "abs_ff_8.5")


m_gull_gr.1_le_perc_chang_in$NF_4.5 <- (m_gull_gr.1_le_perc_chang_in$abs_nf_4.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100
m_gull_gr.1_le_perc_chang_in$FF_4.5 <- (m_gull_gr.1_le_perc_chang_in$abs_ff_4.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100
m_gull_gr.1_le_perc_chang_in$NF_8.5 <- (m_gull_gr.1_le_perc_chang_in$abs_nf_8.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100
m_gull_gr.1_le_perc_chang_in$FF_8.5 <- (m_gull_gr.1_le_perc_chang_in$abs_ff_8.5/m_gull_gr.1_le_perc_chang_in$ref - 1)*100


m_gull_gr.1_le_perc_chang <- pivot_longer(m_gull_gr.1_le_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.1_le_perc_chang <- m_gull_gr.1_le_perc_chang[-c(3:7)]

# establish an order
m_gull_gr.1_le_perc_chang$scenario <- factor(m_gull_gr.1_le_perc_chang$scenario , 
                                                 levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


m_gull_gr.1_le_perc_graph <- ggplot(m_gull_gr.1_le_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m.gull Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.1_le_perc_graph)

#######%%%%%%%%%%
# M-Gull percentage change in change in values box_plot GR.1 Incub

m_gull_gr.1_incub_perc_chang_in <- m.gull_ref_incub_gr.1_unlist %>% 
  right_join(m.gull_nf_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_incub_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
m_gull_gr.1_incub_perc_chang_in <- m_gull_gr.1_incub_perc_chang_in[-c(2,5,7,9,11)]

names(m_gull_gr.1_incub_perc_chang_in) <- c("model", "subbasin", "ref",
                                             "abs_nf_4.5",  "abs_ff_4.5", 
                                             "abs_nf_8.5",  "abs_ff_8.5")


m_gull_gr.1_incub_perc_chang_in$NF_4.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_nf_4.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100
m_gull_gr.1_incub_perc_chang_in$FF_4.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_ff_4.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100
m_gull_gr.1_incub_perc_chang_in$NF_8.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_nf_8.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100
m_gull_gr.1_incub_perc_chang_in$FF_8.5 <- (m_gull_gr.1_incub_perc_chang_in$abs_ff_8.5/m_gull_gr.1_incub_perc_chang_in$ref - 1)*100


m_gull_gr.1_incub_perc_chang <- pivot_longer(m_gull_gr.1_incub_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.1_incub_perc_chang <- m_gull_gr.1_incub_perc_chang[-c(3:7)]

# establish an order
m_gull_gr.1_incub_perc_chang$scenario <- factor(m_gull_gr.1_incub_perc_chang$scenario , 
                                                 levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


m_gull_gr.1_incub_perc_graph <- ggplot(m_gull_gr.1_incub_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m.gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.1_incub_perc_graph)

#######%%%%%%%%%%
# M-Gull percentage change in values box_plot GR.1 Rear

m_gull_gr.1_rear_perc_chang_in <- m.gull_ref_rear_gr.1_unlist %>% 
  right_join(m.gull_nf_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_rear_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
m_gull_gr.1_rear_perc_chang_in <- m_gull_gr.1_rear_perc_chang_in[-c(2,5,7,9,11)]

names(m_gull_gr.1_rear_perc_chang_in) <- c("model", "subbasin", "ref",
                                            "abs_nf_4.5",  "abs_ff_4.5", 
                                            "abs_nf_8.5",  "abs_ff_8.5")


m_gull_gr.1_rear_perc_chang_in$NF_4.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_nf_4.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100
m_gull_gr.1_rear_perc_chang_in$FF_4.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_ff_4.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100
m_gull_gr.1_rear_perc_chang_in$NF_8.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_nf_8.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100
m_gull_gr.1_rear_perc_chang_in$FF_8.5 <- (m_gull_gr.1_rear_perc_chang_in$abs_ff_8.5/m_gull_gr.1_rear_perc_chang_in$ref - 1)*100


m_gull_gr.1_rear_perc_chang <- pivot_longer(m_gull_gr.1_rear_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.1_rear_perc_chang <- m_gull_gr.1_rear_perc_chang[-c(3:7)]

# establish an order
m_gull_gr.1_rear_perc_chang$scenario <- factor(m_gull_gr.1_rear_perc_chang$scenario , 
                                                levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


m_gull_gr.1_rear_perc_graph <- ggplot(m_gull_gr.1_rear_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m.gull Gr.1 rear")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.1_rear_perc_graph)

#######%%%%%%%%%%
# M-Gull percentage change in values box_plot GR.2

m.gull_ref_gr.2_unlist_p22 <- m.gull_ref_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_nf_4.5_gr.2_unlist_p22 <- m.gull_nf_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_ff_4.5_gr.2_unlist_p22 <- m.gull_ff_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_nf_8.5_gr.2_unlist_p22 <- m.gull_nf_8.5_gr.2_unlist_p2[-c(3:5,7:9)]
m.gull_ff_8.5_gr.2_unlist_p22 <- m.gull_ff_8.5_gr.2_unlist_p2[-c(3:5,7:9)]

m_gull_gr.2_perc_chang_in <- m.gull_ref_gr.2_unlist_p22 %>% 
  right_join(m.gull_nf_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(m.gull_nf_8.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(m.gull_ff_8.5_gr.2_unlist_p22, by=c("model","subbasin"))

names(m_gull_gr.2_perc_chang_in) <- c("model", "subbasin", "ref",
                                       "abs_nf_4.5", "abs_ff_4.5", 
                                       "abs_nf_8.5", "abs_ff_8.5")


m_gull_gr.2_perc_chang_in$NF_4.5 <- (m_gull_gr.2_perc_chang_in$abs_nf_4.5/m_gull_gr.2_perc_chang_in$ref - 1)*100
m_gull_gr.2_perc_chang_in$FF_4.5 <- (m_gull_gr.2_perc_chang_in$abs_ff_4.5/m_gull_gr.2_perc_chang_in$ref - 1)*100
m_gull_gr.2_perc_chang_in$NF_8.5 <- (m_gull_gr.2_perc_chang_in$abs_nf_8.5/m_gull_gr.2_perc_chang_in$ref - 1)*100
m_gull_gr.2_perc_chang_in$FF_8.5 <- (m_gull_gr.2_perc_chang_in$abs_ff_8.5/m_gull_gr.2_perc_chang_in$ref - 1)*100


m_gull_gr.2_perc_chang <- pivot_longer(m_gull_gr.2_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
m_gull_gr.2_perc_chang <- m_gull_gr.2_perc_chang[-c(3:7)]

m_gull_gr.2_perc_chang$scenario <- factor(m_gull_gr.2_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

m_gull_gr.2_perc_graph <- ggplot(m_gull_gr.2_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m.gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.2_perc_graph)

#######%%%%%%%%%%
# Mew-Gull percentage change in values box_plot GR.4

m_gull_gr.4_perc_chang_in <- m.gull_ref_gr.4_p2 %>% right_join(m.gull_nf_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(m.gull_ff_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(m.gull_nf_8.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(m.gull_ff_8.5_gr.4_p2, by=c("Group.1","Group.2"))

names(m_gull_gr.4_perc_chang_in) <- c("model", "subbasin", "val1", "ref",
                                       "val.2", "abs_nf_4.5", "val.3", "abs_ff_4.5", 
                                       "val.4", "abs_nf_8.5", "val.5" , "abs_ff_8.5")


m_gull_gr.4_perc_chang_in$NF_4.5 <- (m_gull_gr.4_perc_chang_in$abs_nf_4.5/m_gull_gr.4_perc_chang_in$ref - 1)*100
m_gull_gr.4_perc_chang_in$FF_4.5 <- (m_gull_gr.4_perc_chang_in$abs_ff_4.5/m_gull_gr.4_perc_chang_in$ref - 1)*100
m_gull_gr.4_perc_chang_in$NF_8.5 <- (m_gull_gr.4_perc_chang_in$abs_nf_8.5/m_gull_gr.4_perc_chang_in$ref - 1)*100
m_gull_gr.4_perc_chang_in$FF_8.5 <- (m_gull_gr.4_perc_chang_in$abs_ff_8.5/m_gull_gr.4_perc_chang_in$ref - 1)*100


m_gull_gr.4_perc_chang <- pivot_longer(m_gull_gr.4_perc_chang_in, cols=13:16, names_to = "scenario", values_to = "perc_change")
m_gull_gr.4_perc_chang <- m_gull_gr.4_perc_chang[-c(3:12)]

m_gull_gr.4_perc_chang$scenario <- factor(m_gull_gr.4_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

m_gull_gr.4_perc_graph <- ggplot(m_gull_gr.4_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% m_gull Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(m_gull_gr.4_perc_graph)


# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

m_gull_box_plot_perc <- ggarrange(m_gull_gr.1_le_perc_graph, m_gull_gr.1_incub_perc_graph, 
                                m_gull_gr.1_rear_perc_graph, m_gull_gr.2_perc_graph, 
                                m_gull_gr.4_perc_graph,
                                
                                nrow = 3, ncol = 3)

plot(m_gull_box_plot_perc)

ggsave(m_gull_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "m_gull_box_plot_perc.jpg"),device = "jpg", width = 20, height = 10)


############################################
#*******   Black- headed gull   ***********#
###########################################
#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.1 Incub

bh_gull_gr.1_incub_perc_chang_in <- bh.gull_ref_incub_gr.1_unlist %>% 
  right_join(bh.gull_nf_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_incub_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
bh_gull_gr.1_incub_perc_chang_in <- bh_gull_gr.1_incub_perc_chang_in[-c(2,5,7,9,11)]

names(bh_gull_gr.1_incub_perc_chang_in) <- c("model", "subbasin", "ref",
                                       "abs_nf_4.5",  "abs_ff_4.5", 
                                       "abs_nf_8.5",  "abs_ff_8.5")


bh_gull_gr.1_incub_perc_chang_in$NF_4.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_nf_4.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100
bh_gull_gr.1_incub_perc_chang_in$FF_4.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_ff_4.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100
bh_gull_gr.1_incub_perc_chang_in$NF_8.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_nf_8.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100
bh_gull_gr.1_incub_perc_chang_in$FF_8.5 <- (bh_gull_gr.1_incub_perc_chang_in$abs_ff_8.5/bh_gull_gr.1_incub_perc_chang_in$ref - 1)*100


bh_gull_gr.1_incub_perc_chang <- pivot_longer(bh_gull_gr.1_incub_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.1_incub_perc_chang <- bh_gull_gr.1_incub_perc_chang[-c(3:7)]

# establish an order
bh_gull_gr.1_incub_perc_chang$scenario <- factor(bh_gull_gr.1_incub_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


bh_gull_gr.1_incub_perc_graph <- ggplot(bh_gull_gr.1_incub_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("BH.gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(bh_gull_gr.1_incub_perc_graph)

#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.1 Rear

bh_gull_gr.1_rear_perc_chang_in <- bh.gull_ref_rear_gr.1_unlist %>% 
  right_join(bh.gull_nf_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_rear_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
bh_gull_gr.1_rear_perc_chang_in <- bh_gull_gr.1_rear_perc_chang_in[-c(2,5,7,9,11)]

names(bh_gull_gr.1_rear_perc_chang_in) <- c("model", "subbasin", "ref",
                                             "abs_nf_4.5",  "abs_ff_4.5", 
                                             "abs_nf_8.5",  "abs_ff_8.5")


bh_gull_gr.1_rear_perc_chang_in$NF_4.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_nf_4.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100
bh_gull_gr.1_rear_perc_chang_in$FF_4.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_ff_4.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100
bh_gull_gr.1_rear_perc_chang_in$NF_8.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_nf_8.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100
bh_gull_gr.1_rear_perc_chang_in$FF_8.5 <- (bh_gull_gr.1_rear_perc_chang_in$abs_ff_8.5/bh_gull_gr.1_rear_perc_chang_in$ref - 1)*100


bh_gull_gr.1_rear_perc_chang <- pivot_longer(bh_gull_gr.1_rear_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.1_rear_perc_chang <- bh_gull_gr.1_rear_perc_chang[-c(3:7)]

# establish an order
bh_gull_gr.1_rear_perc_chang$scenario <- factor(bh_gull_gr.1_rear_perc_chang$scenario , 
                                                 levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


bh_gull_gr.1_rear_perc_graph <- ggplot(bh_gull_gr.1_rear_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("BH.gull Gr.1 rear")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(bh_gull_gr.1_rear_perc_graph)


#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.2

bh.gull_ref_gr.2_unlist_p22 <- bh.gull_ref_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_nf_4.5_gr.2_unlist_p22 <- bh.gull_nf_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_ff_4.5_gr.2_unlist_p22 <- bh.gull_ff_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_nf_8.5_gr.2_unlist_p22 <- bh.gull_nf_8.5_gr.2_unlist_p2[-c(3:5,7:9)]
bh.gull_ff_8.5_gr.2_unlist_p22 <- bh.gull_ff_8.5_gr.2_unlist_p2[-c(3:5,7:9)]

bh_gull_gr.2_perc_chang_in <- bh.gull_ref_gr.2_unlist_p22 %>% 
  right_join(bh.gull_nf_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_gr.2_unlist_p22, by=c("model","subbasin"))

names(bh_gull_gr.2_perc_chang_in) <- c("model", "subbasin", "ref",
                                       "abs_nf_4.5", "abs_ff_4.5", 
                                       "abs_nf_8.5", "abs_ff_8.5")


bh_gull_gr.2_perc_chang_in$NF_4.5 <- (bh_gull_gr.2_perc_chang_in$abs_nf_4.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100
bh_gull_gr.2_perc_chang_in$FF_4.5 <- (bh_gull_gr.2_perc_chang_in$abs_ff_4.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100
bh_gull_gr.2_perc_chang_in$NF_8.5 <- (bh_gull_gr.2_perc_chang_in$abs_nf_8.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100
bh_gull_gr.2_perc_chang_in$FF_8.5 <- (bh_gull_gr.2_perc_chang_in$abs_ff_8.5/bh_gull_gr.2_perc_chang_in$ref - 1)*100


bh_gull_gr.2_perc_chang <- pivot_longer(bh_gull_gr.2_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.2_perc_chang <- bh_gull_gr.2_perc_chang[-c(3:7)]

bh_gull_gr.2_perc_chang$scenario <- factor(bh_gull_gr.2_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

bh_gull_gr.2_perc_graph <- ggplot(bh_gull_gr.2_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("BH.gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(bh_gull_gr.2_perc_graph)


#######%%%%%%%%%%
# BH-Gull percentage change in values box_plot GR.3

bh_gull_gr.3_perc_chang_in <- bh.gull_ref_list_gr.3_unlist_p1 %>% 
  right_join(bh.gull_nf_4.5_list_gr.3_unlist_p1, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_4.5_list_gr.3_unlist_p1, by=c("model","subbasin")) %>%
  right_join(bh.gull_nf_8.5_list_gr.3_unlist_p1, by=c("model","subbasin")) %>%
  right_join(bh.gull_ff_8.5_list_gr.3_unlist_p1, by=c("model","subbasin"))

bh_gull_gr.3_perc_chang_in <- bh_gull_gr.3_perc_chang_in[ -c(3,5,7,9,11)]

names(bh_gull_gr.3_perc_chang_in) <- c("model", "subbasin", "ref",
                                        "abs_nf_4.5", "abs_ff_4.5", 
                                        "abs_nf_8.5",  "abs_ff_8.5")


bh_gull_gr.3_perc_chang_in$NF_4.5 <- (bh_gull_gr.3_perc_chang_in$abs_nf_4.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100
bh_gull_gr.3_perc_chang_in$FF_4.5 <- (bh_gull_gr.3_perc_chang_in$abs_ff_4.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100
bh_gull_gr.3_perc_chang_in$NF_8.5 <- (bh_gull_gr.3_perc_chang_in$abs_nf_8.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100
bh_gull_gr.3_perc_chang_in$FF_8.5 <- (bh_gull_gr.3_perc_chang_in$abs_ff_8.5/bh_gull_gr.3_perc_chang_in$ref - 1)*100


bh_gull_gr.3_perc_chang <- pivot_longer(bh_gull_gr.3_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.3_perc_chang <- bh_gull_gr.3_perc_chang[-c(3:7)]

bh_gull_gr.3_perc_chang$scenario <- factor(bh_gull_gr.3_perc_chang$scenario , 
                                           levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

bh_gull_gr.3_perc_graph <- ggplot(bh_gull_gr.3_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Max. falling within VP 1=Yes 0=No") +
  ggtitle("BH.gull Gr.3")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(bh_gull_gr.3_perc_graph)

#######%%%%%%%%%%
# BH-Gull percentage change box_plot GR.4

bh_gull_gr.4_perc_chang_in <- bh.gull_ref_gr.4_p2 %>% right_join(bh.gull_nf_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
                                     right_join(bh.gull_ff_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
                                      right_join(bh.gull_nf_8.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
                                        right_join(bh.gull_ff_8.5_gr.4_p2, by=c("Group.1","Group.2"))

names(bh_gull_gr.4_perc_chang_in) <- c("model", "subbasin", "val1", "ref",
                                       "val.2", "abs_nf_4.5", "val.3", "abs_ff_4.5", 
                                       "val.4", "abs_nf_8.5", "val.5" , "abs_ff_8.5")


bh_gull_gr.4_perc_chang_in$NF_4.5 <- (bh_gull_gr.4_perc_chang_in$abs_nf_4.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100
bh_gull_gr.4_perc_chang_in$FF_4.5 <- (bh_gull_gr.4_perc_chang_in$abs_ff_4.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100
bh_gull_gr.4_perc_chang_in$NF_8.5 <- (bh_gull_gr.4_perc_chang_in$abs_nf_8.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100
bh_gull_gr.4_perc_chang_in$FF_8.5 <- (bh_gull_gr.4_perc_chang_in$abs_ff_8.5/bh_gull_gr.4_perc_chang_in$ref - 1)*100


bh_gull_gr.4_perc_chang <- pivot_longer(bh_gull_gr.4_perc_chang_in, cols=13:16, names_to = "scenario", values_to = "perc_change")
bh_gull_gr.4_perc_chang <- bh_gull_gr.4_perc_chang[-c(3:12)]

bh_gull_gr.4_perc_chang$scenario <- factor(bh_gull_gr.4_perc_chang$scenario , 
                                             levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

bh_gull_gr.4_perc_graph <- ggplot(bh_gull_gr.4_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.95 percentile") +
  ggtitle("% bh_gull Gr.4 >0.95")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))
  
plot(bh_gull_gr.4_perc_graph)

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

bh_gull_box_plot_perc <- ggarrange( bh_gull_gr.1_incub_perc_graph, 
                                bh_gull_gr.1_rear_perc_graph, bh_gull_gr.2_perc_graph, 
                                bh_gull_gr.3_perc_graph, bh_gull_gr.4_perc_graph,
                                
                                nrow = 3, ncol = 3)

plot(bh_gull_box_plot_perc)

ggsave(bh_gull_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "bh_gull_box_plot_perc.jpg"),device = "jpg", width = 20, height = 10)


############################################
#*******   Little Tern   ***********#
###########################################


#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.1 Laying eggs

tern_gr.1_le_perc_chang_in <- tern_ref_le_gr.1_unlist %>% 
  right_join(tern_nf_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_le_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_le_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
tern_gr.1_le_perc_chang_in <- tern_gr.1_le_perc_chang_in[-c(2,5,7,9,11)]

names(tern_gr.1_le_perc_chang_in) <- c("model", "subbasin", "ref",
                                         "abs_nf_4.5",  "abs_ff_4.5", 
                                         "abs_nf_8.5",  "abs_ff_8.5")


tern_gr.1_le_perc_chang_in$NF_4.5 <- (tern_gr.1_le_perc_chang_in$abs_nf_4.5/tern_gr.1_le_perc_chang_in$ref - 1)*100
tern_gr.1_le_perc_chang_in$FF_4.5 <- (tern_gr.1_le_perc_chang_in$abs_ff_4.5/tern_gr.1_le_perc_chang_in$ref - 1)*100
tern_gr.1_le_perc_chang_in$NF_8.5 <- (tern_gr.1_le_perc_chang_in$abs_nf_8.5/tern_gr.1_le_perc_chang_in$ref - 1)*100
tern_gr.1_le_perc_chang_in$FF_8.5 <- (tern_gr.1_le_perc_chang_in$abs_ff_8.5/tern_gr.1_le_perc_chang_in$ref - 1)*100


tern_gr.1_le_perc_chang <- pivot_longer(tern_gr.1_le_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.1_le_perc_chang <- tern_gr.1_le_perc_chang[-c(3:7)]

# establish an order
tern_gr.1_le_perc_chang$scenario <- factor(tern_gr.1_le_perc_chang$scenario , 
                                             levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


tern_gr.1_le_perc_graph <- ggplot(tern_gr.1_le_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.1_le_perc_graph)

#######%%%%%%%%%%
# Tern percentage change in change in values box_plot GR.1 Incub

tern_gr.1_incub_perc_chang_in <- tern_ref_incub_gr.1_unlist %>% 
  right_join(tern_nf_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_incub_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_incub_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
tern_gr.1_incub_perc_chang_in <- tern_gr.1_incub_perc_chang_in[-c(2,5,7,9,11)]

names(tern_gr.1_incub_perc_chang_in) <- c("model", "subbasin", "ref",
                                            "abs_nf_4.5",  "abs_ff_4.5", 
                                            "abs_nf_8.5",  "abs_ff_8.5")


tern_gr.1_incub_perc_chang_in$NF_4.5 <- (tern_gr.1_incub_perc_chang_in$abs_nf_4.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100
tern_gr.1_incub_perc_chang_in$FF_4.5 <- (tern_gr.1_incub_perc_chang_in$abs_ff_4.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100
tern_gr.1_incub_perc_chang_in$NF_8.5 <- (tern_gr.1_incub_perc_chang_in$abs_nf_8.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100
tern_gr.1_incub_perc_chang_in$FF_8.5 <- (tern_gr.1_incub_perc_chang_in$abs_ff_8.5/tern_gr.1_incub_perc_chang_in$ref - 1)*100


tern_gr.1_incub_perc_chang <- pivot_longer(tern_gr.1_incub_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.1_incub_perc_chang <- tern_gr.1_incub_perc_chang[-c(3:7)]

# establish an order
tern_gr.1_incub_perc_chang$scenario <- factor(tern_gr.1_incub_perc_chang$scenario , 
                                                levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


tern_gr.1_incub_perc_graph <- ggplot(tern_gr.1_incub_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.1_incub_perc_graph)

#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.1 Rear

tern_gr.1_rear_perc_chang_in <- tern_ref_rear_gr.1_unlist %>% 
  right_join(tern_nf_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_rear_gr.1_unlist, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_rear_gr.1_unlist, by=c("model","subbasin"))

#remove repeating columns
tern_gr.1_rear_perc_chang_in <- tern_gr.1_rear_perc_chang_in[-c(2,5,7,9,11)]

names(tern_gr.1_rear_perc_chang_in) <- c("model", "subbasin", "ref",
                                           "abs_nf_4.5",  "abs_ff_4.5", 
                                           "abs_nf_8.5",  "abs_ff_8.5")


tern_gr.1_rear_perc_chang_in$NF_4.5 <- (tern_gr.1_rear_perc_chang_in$abs_nf_4.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100
tern_gr.1_rear_perc_chang_in$FF_4.5 <- (tern_gr.1_rear_perc_chang_in$abs_ff_4.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100
tern_gr.1_rear_perc_chang_in$NF_8.5 <- (tern_gr.1_rear_perc_chang_in$abs_nf_8.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100
tern_gr.1_rear_perc_chang_in$FF_8.5 <- (tern_gr.1_rear_perc_chang_in$abs_ff_8.5/tern_gr.1_rear_perc_chang_in$ref - 1)*100


tern_gr.1_rear_perc_chang <- pivot_longer(tern_gr.1_rear_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.1_rear_perc_chang <- tern_gr.1_rear_perc_chang[-c(3:7)]

# establish an order
tern_gr.1_rear_perc_chang$scenario <- factor(tern_gr.1_rear_perc_chang$scenario , 
                                               levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))


tern_gr.1_rear_perc_graph <- ggplot(tern_gr.1_rear_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 rear")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.1_rear_perc_graph)

#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.2

tern_ref_gr.2_unlist_p22 <- tern_ref_gr.2_unlist_p2[-c(3:5,7:9)]
tern_nf_4.5_gr.2_unlist_p22 <- tern_nf_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
tern_ff_4.5_gr.2_unlist_p22 <- tern_ff_4.5_gr.2_unlist_p2[-c(3:5,7:9)]
tern_nf_8.5_gr.2_unlist_p22 <- tern_nf_8.5_gr.2_unlist_p2[-c(3:5,7:9)]
tern_ff_8.5_gr.2_unlist_p22 <- tern_ff_8.5_gr.2_unlist_p2[-c(3:5,7:9)]

tern_gr.2_perc_chang_in <- tern_ref_gr.2_unlist_p22 %>% 
  right_join(tern_nf_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(tern_ff_4.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(tern_nf_8.5_gr.2_unlist_p22, by=c("model","subbasin")) %>%
  right_join(tern_ff_8.5_gr.2_unlist_p22, by=c("model","subbasin"))

names(tern_gr.2_perc_chang_in) <- c("model", "subbasin", "ref",
                                      "abs_nf_4.5", "abs_ff_4.5", 
                                      "abs_nf_8.5", "abs_ff_8.5")


tern_gr.2_perc_chang_in$NF_4.5 <- (tern_gr.2_perc_chang_in$abs_nf_4.5/tern_gr.2_perc_chang_in$ref - 1)*100
tern_gr.2_perc_chang_in$FF_4.5 <- (tern_gr.2_perc_chang_in$abs_ff_4.5/tern_gr.2_perc_chang_in$ref - 1)*100
tern_gr.2_perc_chang_in$NF_8.5 <- (tern_gr.2_perc_chang_in$abs_nf_8.5/tern_gr.2_perc_chang_in$ref - 1)*100
tern_gr.2_perc_chang_in$FF_8.5 <- (tern_gr.2_perc_chang_in$abs_ff_8.5/tern_gr.2_perc_chang_in$ref - 1)*100


tern_gr.2_perc_chang <- pivot_longer(tern_gr.2_perc_chang_in, cols=8:11, names_to = "scenario", values_to = "perc_change")
tern_gr.2_perc_chang <- tern_gr.2_perc_chang[-c(3:7)]

tern_gr.2_perc_chang$scenario <- factor(tern_gr.2_perc_chang$scenario , 
                                          levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

tern_gr.2_perc_graph <- ggplot(tern_gr.2_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.2_perc_graph)


#######%%%%%%%%%%
# Tern percentage change in values box_plot GR.4

tern_gr.4_perc_chang_in <- tern_ref_gr.4_p2 %>% right_join(tern_nf_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(tern_ff_4.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(tern_nf_8.5_gr.4_p2, by=c("Group.1","Group.2")) %>%
  right_join(tern_ff_8.5_gr.4_p2, by=c("Group.1","Group.2"))

names(tern_gr.4_perc_chang_in) <- c("model", "subbasin", "val1", "ref",
                                      "val.2", "abs_nf_4.5", "val.3", "abs_ff_4.5", 
                                      "val.4", "abs_nf_8.5", "val.5" , "abs_ff_8.5")


tern_gr.4_perc_chang_in$NF_4.5 <- (tern_gr.4_perc_chang_in$abs_nf_4.5/tern_gr.4_perc_chang_in$ref - 1)*100
tern_gr.4_perc_chang_in$FF_4.5 <- (tern_gr.4_perc_chang_in$abs_ff_4.5/tern_gr.4_perc_chang_in$ref - 1)*100
tern_gr.4_perc_chang_in$NF_8.5 <- (tern_gr.4_perc_chang_in$abs_nf_8.5/tern_gr.4_perc_chang_in$ref - 1)*100
tern_gr.4_perc_chang_in$FF_8.5 <- (tern_gr.4_perc_chang_in$abs_ff_8.5/tern_gr.4_perc_chang_in$ref - 1)*100


tern_gr.4_perc_chang <- pivot_longer(tern_gr.4_perc_chang_in, cols=13:16, names_to = "scenario", values_to = "perc_change")
tern_gr.4_perc_chang <- tern_gr.4_perc_chang[-c(3:12)]

tern_gr.4_perc_chang$scenario <- factor(tern_gr.4_perc_chang$scenario , 
                                          levels=c("NF_4.5", "FF_4.5", "NF_8.5", "FF_8.5"))

tern_gr.4_perc_graph <- ggplot(tern_gr.4_perc_chang, aes(x=scenario, y=perc_change)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% tern Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))

plot(tern_gr.4_perc_graph)


# join all the graphs together 

library("ggpubr")
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

tern_box_plot_perc <- ggarrange(tern_gr.1_le_perc_graph, tern_gr.1_incub_perc_graph, 
                                    tern_gr.1_rear_perc_graph, tern_gr.2_perc_graph, 
                                    tern_gr.4_perc_graph,
                                    
                                    nrow = 3, ncol = 3)

plot(tern_box_plot_perc)

ggsave(tern_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "tern_box_plot_perc.jpg"),device = "jpg", width = 20, height = 10)

######## ALL 3 BIRDS ########
#model/subbasin/value 
# 162 values (9 models * 18 subbasins)

all_box_plot_perc <- ggarrange(m_gull_gr.1_le_perc_graph, m_gull_gr.1_incub_perc_graph, 
                                   m_gull_gr.1_rear_perc_graph, m_gull_gr.2_perc_graph,  
                                   m_gull_gr.4_perc_graph,
                                   bh_gull_gr.1_incub_perc_graph, bh_gull_gr.1_rear_perc_graph,
                                   bh_gull_gr.3_perc_graph, bh_gull_gr.2_perc_graph, 
                                   bh_gull_gr.4_perc_graph,
                                   tern_gr.1_le_perc_graph, tern_gr.1_incub_perc_graph, 
                                   tern_gr.1_rear_perc_graph, tern_gr.2_perc_graph,  
                                   tern_gr.4_perc_graph,
                                   
                                   nrow = 3, ncol = 5)

plot(all_box_plot_perc)

ggsave(all_box_plot_perc , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/",
                     "all_box_plot_perc.jpg"),device = "jpg", width = 20, height = 10)


####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
#### GRAPHS FOR SUBBASINS #####
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
##### TERN
tern_gr.1_le_perc_graph_sub <- ggplot(tern_gr.1_le_perc_chang, 
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(tern_gr.1_le_perc_graph_sub)

ggsave(tern_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


tern_gr.1_incub_perc_graph_sub <- ggplot(tern_gr.1_incub_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

tern_gr.1_rear_perc_graph_sub <- ggplot(tern_gr.1_rear_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


tern_gr.2_perc_graph_sub <- ggplot(tern_gr.2_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


tern_gr.4_perc_graph_sub <- ggplot(tern_gr.4_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% tern Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

# all together
all_box_plot_tern_perc_graph_sub <- ggarrange(tern_gr.1_le_perc_graph_sub, tern_gr.1_incub_perc_graph_sub,
                               tern_gr.1_rear_perc_graph_sub, tern_gr.2_perc_graph_sub,
                               tern_gr.4_perc_graph_sub,
                               
                               nrow = 5, ncol = 1)

plot(all_box_plot_tern_perc_graph_sub)

ggsave(all_box_plot_tern_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_tern_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 30)


##### Mew Gull
m_gull_gr.1_le_perc_graph_sub <- ggplot(m_gull_gr.1_le_perc_chang, 
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(m_gull_gr.1_le_perc_graph_sub)

ggsave(m_gull_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


m_gull_gr.1_incub_perc_graph_sub <- ggplot(m_gull_gr.1_incub_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

m_gull_gr.1_rear_perc_graph_sub <- ggplot(m_gull_gr.1_rear_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


m_gull_gr.2_perc_graph_sub <- ggplot(m_gull_gr.2_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


m_gull_gr.4_perc_graph_sub <- ggplot(m_gull_gr.4_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% m_gull Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

# all together
all_box_plot_m_gull_perc_graph_sub <- ggarrange(m_gull_gr.1_le_perc_graph_sub, m_gull_gr.1_incub_perc_graph_sub,
                                              m_gull_gr.1_rear_perc_graph_sub, m_gull_gr.2_perc_graph_sub,
                                              m_gull_gr.4_perc_graph_sub,
                                              
                                              nrow = 5, ncol = 1)

plot(all_box_plot_m_gull_perc_graph_sub)

ggsave(all_box_plot_m_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_m_gull_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 30)


##### Black Headed Gull

bh_gull_gr.1_incub_perc_graph_sub <- ggplot(bh_gull_gr.1_incub_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

bh_gull_gr.1_rear_perc_graph_sub <- ggplot(bh_gull_gr.1_rear_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


bh_gull_gr.2_perc_graph_sub <- ggplot(bh_gull_gr.2_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


bh_gull_gr.3_perc_graph_sub <- ggplot(bh_gull_gr.3_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Max. falling within VP") +
  ggtitle("bh_gull Gr.3 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.3_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.3_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)


bh_gull_gr.4_perc_graph_sub <- ggplot(bh_gull_gr.4_perc_chang, aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.95 percentile") +
  ggtitle("% bh_gull Gr.4 >0.95")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 10)

# all together
all_box_plot_bh_gull_perc_graph_sub <- ggarrange(bh_gull_gr.1_incub_perc_graph_sub,
                                                bh_gull_gr.1_rear_perc_graph_sub, 
                                                bh_gull_gr.2_perc_graph_sub, bh_gull_gr.3_perc_graph_sub,
                                                bh_gull_gr.4_perc_graph_sub,
                                                
                                                nrow = 5, ncol = 1)

plot(all_box_plot_bh_gull_perc_graph_sub)

ggsave(all_box_plot_bh_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_bh_gull_perc_graph_sub.jpg"),device = "jpg", width = 20, height = 30)
######
######
######
###### 3 selected subbasins

# 910, 1358, 1875
######
######
######


##### Black Headed Gull

bh_gull_gr.1_incub_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.1_incub_perc_chang, 
                                            subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                            aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

bh_gull_gr.1_rear_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.1_rear_perc_chang, 
                                           subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                            aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.1 rear")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


bh_gull_gr.2_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.2_perc_chang, 
                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("bh_gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


bh_gull_gr.3_perc_graph_sub <- ggplot(data=subset(bh_gull_gr.3_perc_chang, 
                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="Max. falling within VP") +
  ggtitle("bh_gull Gr.3 ") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.3_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.3_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


bh_gull_gr.4_perc_graph_sub <- ggplot(dat=subset(bh_gull_gr.4_perc_chang, 
                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                      aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.95 percentile") +
  ggtitle("% bh_gull Gr.4 >0.95")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(bh_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "bh_gull_3_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

# all together
all_box_plot_bh_gull_perc_graph_sub <- ggarrange(bh_gull_gr.1_incub_perc_graph_sub,
                                                 bh_gull_gr.1_rear_perc_graph_sub, 
                                                 bh_gull_gr.2_perc_graph_sub, bh_gull_gr.3_perc_graph_sub,
                                                 bh_gull_gr.4_perc_graph_sub,
                                                 
                                                 nrow = 5, ncol = 1)

plot(all_box_plot_bh_gull_perc_graph_sub)

ggsave(all_box_plot_bh_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_bh_gull_3_perc_graph_sub.jpg"),device = "jpg", width = 3, height = 20)


##### Mew Gull
m_gull_gr.1_le_perc_graph_sub <- ggplot(data=subset(m_gull_gr.1_le_perc_chang, 
                                               subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                        aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(m_gull_gr.1_le_perc_graph_sub)

ggsave(m_gull_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


m_gull_gr.1_incub_perc_graph_sub <- ggplot(data=subset(m_gull_gr.1_incub_perc_chang, 
                                          subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                           aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

m_gull_gr.1_rear_perc_graph_sub <- ggplot(data=subset(m_gull_gr.1_rear_perc_chang, 
                                          subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                          aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


m_gull_gr.2_perc_graph_sub <- ggplot(data=subset(m_gull_gr.2_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("m_gull Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


m_gull_gr.4_perc_graph_sub <- ggplot(data=subset(m_gull_gr.4_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% m_gull Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(m_gull_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "m_gull_3_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

# all together
all_box_plot_m_gull_perc_graph_sub <- ggarrange(m_gull_gr.1_le_perc_graph_sub, m_gull_gr.1_incub_perc_graph_sub,
                                                m_gull_gr.1_rear_perc_graph_sub, m_gull_gr.2_perc_graph_sub,
                                                m_gull_gr.4_perc_graph_sub,
                                                
                                                nrow = 5, ncol = 1)

plot(all_box_plot_m_gull_perc_graph_sub)

ggsave(all_box_plot_m_gull_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_m_gull_3_perc_graph_sub.jpg"),device = "jpg", width = 3, height = 20)


##### Tern
tern_gr.1_le_perc_graph_sub <- ggplot(data=subset(tern_gr.1_le_perc_chang, 
                                                    subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                        aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 le")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

plot(tern_gr.1_le_perc_graph_sub)

ggsave(tern_gr.1_le_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.1_le_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


tern_gr.1_incub_perc_graph_sub <- ggplot(data=subset(tern_gr.1_incub_perc_chang, 
                                                       subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                           aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_incub_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.1_incub_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

tern_gr.1_rear_perc_graph_sub <- ggplot(data=subset(tern_gr.1_rear_perc_chang, 
                                                      subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                          aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.1 Incub")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.1_rear_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.1_rear_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


tern_gr.2_perc_graph_sub <- ggplot(data=subset(tern_gr.2_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in mean streamflow m3/s") +
  ggtitle("tern Gr.2 day_03max") +
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.2_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.2_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)


tern_gr.4_perc_graph_sub <- ggplot(data=subset(tern_gr.4_perc_chang, 
                                                 subbasin=="910" | subbasin=="1358"|subbasin=="1875"),
                                     aes(y=perc_change, group=subbasin)) + 
  geom_boxplot( alpha =0.8, width = 0.9)+ 
  labs(y="% change in no. of days above 0.75 percentile") +
  ggtitle("% tern Gr.4 >0.75")+
  theme(axis.title.x = element_blank())+
  geom_hline(yintercept = c(-30, 30), colour = "Red")+ 
  scale_y_continuous(breaks = seq(from = -100, to = 200, by = 10))+
  facet_grid(scenario ~ subbasin)

ggsave(tern_gr.4_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "tern_3_gr.4_perc_graph_sub.jpg"),device = "jpg", width = 5, height = 10)

# all together
all_box_plot_tern_perc_graph_sub <- ggarrange(tern_gr.1_le_perc_graph_sub, tern_gr.1_incub_perc_graph_sub,
                                                tern_gr.1_rear_perc_graph_sub, tern_gr.2_perc_graph_sub,
                                                tern_gr.4_perc_graph_sub,
                                                
                                                nrow = 5, ncol = 1)

plot(all_box_plot_tern_perc_graph_sub)

ggsave(all_box_plot_tern_perc_graph_sub , 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Results/baseline_iha/subbasin/",
                     "all_box_plot_tern_3_perc_graph_sub.jpg"),device = "jpg", width = 3, height = 20)

