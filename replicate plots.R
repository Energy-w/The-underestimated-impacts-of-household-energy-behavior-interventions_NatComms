

theme_set(
  theme_bw()+
    theme(
      panel.border = element_blank(),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = rel(1.1)),
      legend.position = "none")
)

#Fig.3a-----------
read_rds("coefreg_interveniton_effects.rds") %>% as_tibble() %>% 
  mutate(across(c(coef,LB,UB, coef2,LB2,UB2), 
                ~.x/mean(regdata_inter$add_ele_wh))) %>% 
  mutate(bin = 1:96, mean = mean(coef2)) %>% 
  ggplot(aes(x=bin)) + 
  geom_line(aes(y=coef),color="grey80",linetype="longdash",size=0.5)+ 
  geom_ribbon(aes(ymin=LB,ymax=UB),alpha=0.1,fill='grey40')+
  geom_line(aes(y=coef2),color="#7fc97f",size=0.5)+      
  geom_ribbon(aes(ymin=LB2,ymax=UB2),alpha=0.3,fill='#74a9cf')+
  geom_hline(yintercept = 0,color="grey50",size=0.5,
             linetype="dashed",alpha=0.7) +
  
  annotate("segment",x=2,xend=8,y=0.25,yend = 0.25,
           linetype="longdash",color="grey80")+
  annotate("text",x=30,y=0.25,label="General knowledge intervention (g2)") +
  annotate("segment",x=2,xend=8,y=0.2,yend = 0.2,color="#7fc97f")+
  annotate("text",x=29,y=0.2,label="Specific guidance intervention (g3)") +
  labs(x=NULL, y=NULL)+
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = seq(0,96,16), labels = timeHM_formatter) +
  scale_y_percent(limits = c(-0.7,0.3))+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(size = rel(1.2)))
ggsave("tu/reg.png",width = 7,height = 4,dpi=600,device = png)


#Fig.3b-----------
showvar <- c('income:\nmiddle','income:\nmiddle_high','income:\nhigh','education:\nmiddle','education:\nhigh','child &\nno elderly','no child &\nelderly','child &\nelderly')
varlevels <- c('income:\nmiddle','income:\nmiddle_high','income:\nhigh','education:\nmiddle','education:\nhigh','no child &\nelderly','child &\nno elderly','child &\nelderly')

read_rds("coef_hetero_effects.rds") %>% as_tibble() %>% 
  gather(ends_with("...1."),key="var",value="coef") %>% 
  mutate(var = str_remove_all(var, "\\...1."),
         var=str_replace(var,"edu",""),
         type = case_when(
           var %in% c('inc_lev2','inc_lev3','inc_lev4') ~ 'income',
           var %in% c('ISCED3','ISCED4_8') ~ 'education',
           var %in% c('child','old','child_old') ~ 'HHstructure')) %>%  
  mutate(var2 = rep(showvar, each=96),
         var2 = factor(var2, levels = varlevels)) %>% 
  mutate(coef = coef/mean(regdata_inter$add_ele_wh)) %>% 
  
  ggplot(aes(x=var2,y=coef,fill= type))+
  geom_jitter(aes(color=type), alpha=0.2, width = 0.2) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  stat_summary(fun='mean', shape=21, fill='white',size=0.4) +
  geom_hline(yintercept = 0,color="grey50",size=0.5,
             linetype="dashed",alpha=0.7) +
  scale_y_percent(limits=c(-1,1.8), 
                  breaks = c(seq(-0.6, 0.6, 0.4)),
                  label = c('-60%','-20%','20%','60%')) +
  labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(size = rel(0.8)),
        legend.position = 'none')


#Fig.4a-----------
pacman::p_load(sf,BBmisc,ggspatial,hrbrthemes,ggbump,ggnewscale)

background <- read_rds("projection_provincial_ac and ele_2050.rds")
label <- read_rds("Geo_provlabel2021.rds")

#p1
ggplot() +
  geom_sf(data = background, aes(fill = defcolor), #mill
          alpha=0.5, color = "grey40",size = 0.1) +
  geom_sf(data = background %>% dplyr::filter(is.na(amount)),
          fill="white",color = "grey40",size = 0.05) +
  geom_sf(data = slice(line,4), color = "grey60", size = 0.3) +
  scale_fill_identity() + 
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(plot.margin = margin(),
        panel.grid = element_blank(),
        legend.position = 'none',
        axis.text = element_blank(),
        axis.ticks = element_blank()) -> p1
p1


p1 +
  xlim(-1300000,1650000)+ ylim(2300000,4550000) +  
  geom_text(data = label,
            aes(x=X, y= Y, label = ename), 
            size = 2.5)  -> p2
p2


#p3
legend_data <- data.frame(
  x = rep(1:4, 4),
  y = rep(4:1, each = 4),
  fill = defcolor,
  label_x = c(1, 2, 3, 4, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  label_y = c(4.6, 4.6, 4.6, 4.6, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  label = c("0-30", "30-50", "50-80", ">80", "", "", "", "", "", 
            "", "", "", "", "", "", ""),
  row_label = rep(c("0-30", "30-50", "50-80", ">80"), each=4)
)
legend_data %>% ggplot() +
  geom_tile(aes(x = x, y = y, fill = fill), alpha=0.5, 
            width = 0.9, height = 0.9) +
  geom_text(aes(x = label_x, y = label_y, label = label), 
            size = 2, hjust = 0.5, vjust = 0.5) +
  geom_text(data = legend_data[!duplicated(legend_data$row_label), ], 
            aes(x = 0.5, y = y, label = row_label), 
            size = 2, hjust = 1, vjust = 0.5) +
  annotate("text", x = 2.5, y = 5,
           label = "AC units (million)", size = 3, hjust = 0.5) +
  annotate("text", x = -0.4, y = 2.5, 
           label = "Cooling demand (TWh)", 
           angle = 90, size = 3, hjust = 0.5, vjust = 0.5)+
  scale_fill_identity() + 
  theme_void() +
  theme(legend.position = "none") -> p3
p3


#combine:
p11 <- ggplotGrob(p1)
p33 <- ggplotGrob(p3)

p2 + annotation_custom(p11, xmin = -1400000, xmax = -720000,
                       ymin =4000000, ymax = 4660000) +
  annotation_custom(p33, xmin = -1400000, xmax = -700000,
                    ymin = 2240000, ymax = 2860000)



#Fig.4b-----------
df <- read_rds("projection_national_ele_2021-2050.rds")
df2 <- acele_save_n_yr %>% dplyr::filter(rcp!='rcp_60')

df %>% 
  ggplot(aes(x=year)) +
  geom_smooth(data = df2, aes(y = ELE, group = interaction(ssp, rcp)), 
              color = 'grey90', size = 0.4) +
  geom_smooth(data = df2, aes(y = ELE_inter1, group = interaction(ssp, rcp)), 
              color = '#ffecb3', size = 0.4) +
  geom_smooth(data = df2, aes(y = ELE_inter2, group = interaction(ssp, rcp)), 
              color = '#d1c4e9', size = 0.4) +
  geom_line(aes(y=ELE), color='grey80',size=1.2) + 
  geom_ribbon(aes(ymin=ELE_LB,ymax=ELE_UB),alpha=0.1,
              fill='grey50') +
  geom_point(aes(y=ELE), shape=21, fill='white',color='grey40',size=3) +
  
  geom_line(aes(y=ELE_inter1), color='#f6cf71', size=0.7)+
  geom_point(aes(y=ELE_inter1),shape=21,fill='white',color='#f6cf71',size=3)+
  
  geom_line(aes(y=ELE_inter2), color='#8491B4FF', size=1.2)+ 
  geom_ribbon(aes(ymin=ELE_LB_inter2,ymax=ELE_UB_inter2),alpha=0.2,
              fill='#8491B4FF') +
  geom_point(aes(y=ELE_inter2),shape=21,fill='white',color='#8491B4FF',size=3)+
  
  annotate("segment", x=2050.3, xend =2050.3,
           y = df$ELE[df$year==2050],
           yend = df$ELE_inter1[df$year==2050],
           color="#f6cf71",size=1,
           arrow=arrow(angle=30,length=unit(.2,"cm"))) +
  annotate("segment", x=2050.6, xend =2050.6,
           y = df$ELE[df$year==2050],
           yend = df$ELE_inter2[df$year==2050],
           color="#8491B4FF",size=1,
           arrow=arrow(angle=30,length=unit(.2,"cm"))) +
  
  annotate("segment",x=2021,xend=2022,y=960,yend = 960,size=2, color='#f6cf71')+
  annotate("text",x=2030.03,y=960,
           label="Estimated savings without demographic transition") +
  annotate("segment",x=2021,xend=2022,y=910,yend = 910,size=2,color='#8491B4FF')+
  annotate("text",x=2029.6,y=910,
           label="Estimated savings with demographic transition") +
  scale_x_continuous(limits = c(2021,2050.7),expand=c(0.05,0),
                     breaks = c(2030,2040,2050),labels = c(2030,2040,2050))+
  scale_y_continuous(expand = c(0,0.05),limits = c(-100,1000),
                     breaks = seq(200,1000,200),labels = seq(200,1000,200))+
  labs(x=NULL, y=NULL) +
  theme(axis.text = element_text(size = rel(0.8)),
        panel.grid.major.y = element_line(linetype='dashed',
                                          color='grey60',size=0.1)) -> p1
p1

#2030
df3 <- df %>% dplyr::filter(year %in% c(2030,2050)) %>% 
  mutate(F1 = ELE_inter1, F1_LB = ELE_LB_inter1, F1_UB = ELE_UB_inter1,
         F2 = ELE_inter2, F2_LB = ELE_LB_inter2, F2_UB = ELE_UB_inter2) %>% 
  mutate(ELE_LB_inter1 = ELE - (ELE_UB - F1_UB),
         ELE_UB_inter1 = ELE - (ELE_LB - F1_LB),
         ELE_LB_inter2 = ELE - (ELE_UB - F2_UB),
         ELE_UB_inter2 = ELE - (ELE_LB - F2_LB))

plot30 <- df3 %>% dplyr::filter(year==2030) %>% 
  select(year,ELE,ELE_inter1,ELE_inter2,F2) %>% 
  set_names('year','ELE','inter1','inter2','F') %>% 
  pivot_longer(!year, names_to = 'var',values_to = 'lower') %>% 
  mutate(upper = ifelse(var %in% c('ELE','inter1','inter2'),
                        df3$ELE[df3$year==2030], lower),
         lower = ifelse(var %in% c('ELE','F'), 0, lower)) %>% 
  left_join(
    df3 %>% select(year,ELE_LB,ELE_LB_inter1,ELE_LB_inter2,F2_LB) %>% 
      set_names('year','ELE','inter1','inter2','F') %>% 
      pivot_longer(!year, names_to = 'var',values_to = 'lb2'),
    by = c('year','var')
  ) %>% 
  left_join(
    df3 %>% select(year,ELE_UB,ELE_UB_inter1,ELE_UB_inter2,F2_UB) %>% 
      set_names('year','ELE','inter1','inter2','F') %>% 
      pivot_longer(!year, names_to = 'var',values_to = 'ub2'),
    by = c('year','var')
  ) %>% 
  mutate(savep = ifelse(var %in% c('inter1','inter2'), 
                        (lower-df3$ELE[df3$year==2030])/df3$ELE[df3$year==2030],
                        NA),
         save = ifelse(var %in% c('inter1','inter2'), 
                       (lower-df3$ELE[df3$year==2030]),NA)) %>% 
  mutate(var=as.numeric(factor(var,levels = c('ELE','inter1','inter2','F'))))

plot50 <- df3 %>% dplyr::filter(year==2050) %>% 
  select(year,ELE,ELE_inter1,ELE_inter2,F2) %>% 
  set_names('year','ELE','inter1','inter2','F') %>% 
  pivot_longer(!year, names_to = 'var',values_to = 'lower') %>% 
  mutate(upper = ifelse(var %in% c('ELE','inter1','inter2'),
                        df3$ELE[df3$year==2050], lower),
         lower = ifelse(var %in% c('ELE','F'), 0, lower)) %>% 
  left_join(
    df3 %>% select(year,ELE_LB,ELE_LB_inter1,ELE_LB_inter2,F2_LB) %>% 
      set_names('year','ELE','inter1','inter2','F') %>% 
      pivot_longer(!year, names_to = 'var',values_to = 'lb2'),
    by = c('year','var')
  ) %>% 
  left_join(
    df3 %>% select(year,ELE_UB,ELE_UB_inter1,ELE_UB_inter2,F2_UB) %>% 
      set_names('year','ELE','inter1','inter2','F') %>% 
      pivot_longer(!year, names_to = 'var',values_to = 'ub2'),
    by = c('year','var')
  ) %>% 
  mutate(savep = ifelse(var %in% c('inter1','inter2'), 
                        (lower-df3$ELE[df3$year==2050])/df3$ELE[df3$year==2050],
                        NA),
         save = ifelse(var %in% c('inter1','inter2'), 
                       (lower-df3$ELE[df3$year==2050]),NA)) %>% 
  mutate(var=as.numeric(factor(var,levels = c('ELE','inter1','inter2','F'))))



#subfigures
defcolor <- c('grey70','#f6cf71','#8491B4FF','#66c2a5')

plot30 %>% 
  ggplot() +
  geom_rect(aes(xmin=var, xmax = var+0.9, ymin=lower, ymax=upper,
                fill = factor(var)),alpha = 0.7)+
  geom_errorbar(aes(x=var+0.45, ymin=lb2, ymax=ub2), width=0.2,color='grey60')+
  geom_text(aes(x=var+0.45,y=upper+80,
                label=ifelse(is.na(save),"",
                             paste0(round(save,1),"\n(",round(savep,3)*100,'%)'))),
            face='bold',fontface='italic',size=2.5) +
  scale_fill_manual(values= defcolor)+
  scale_color_manual(values= defcolor)+
  scale_y_continuous(expand = c(0,0),limits=c(0,600),
                     breaks = seq(0,400,200), labels = seq(0,400,200))+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1.45,4.45,1),
                     labels=c('2030\nbaseline','','','after\nintervention'))+
  labs(x=NULL, y=NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(.1,.1,.1,.1),'cm'),
        legend.position = 'none',
        axis.text = element_text(size = rel(0.6))) -> p2
p2
plot50 %>% 
  ggplot() +
  geom_rect(aes(xmin=var, xmax = var+0.9, ymin=lower, ymax=upper,
                fill = factor(var)), alpha = 0.7)+
  geom_errorbar(aes(x=var+0.45, ymin=lb2, ymax=ub2), width=0.2,color='grey60')+
  geom_text(aes(x=var+0.45,y=upper+100,
                label=ifelse(is.na(save),"",
                             paste0(round(save,1),"\n(",round(savep,3)*100,'%)'))),
            face='bold',fontface='italic',size=2.5) +
  scale_fill_manual(values= defcolor)+
  scale_color_manual(values= defcolor)+
  scale_y_continuous(expand = c(0,0),limits=c(0,950),
                     breaks = seq(0,800,200), labels = seq(0,800,200))+
  scale_x_continuous(expand=c(0.02,0.02),breaks=seq(1.45,4.45,1),
                     labels=c('2050\n baseline','','','after\nintervention'))+
  labs(x=NULL, y=NULL)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(.1,.1,.1,.1),'cm'),
        legend.position = 'none',
        axis.text = element_text(size = rel(0.6))) -> p3
p3



#combine：
p22 <- ggplotGrob(p2)
p33 <- ggplotGrob(p3)

p1 + annotation_custom(p22,xmin = 2026,xmax =2034,ymin =-105,ymax =200) +
  annotation_custom(p33,xmin = 2041,xmax =2051,ymin =-105,ymax =320)




