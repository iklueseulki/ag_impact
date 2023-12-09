library(pacman)
p_load(ggplot2, dplyr, magrittr, stringr, ggrepel,tidyr,forcats)


##final version
df <-read.csv("dCC_gCC.csv")
df_Wheat <-filter(df, Crop=="Wheat")
df_Wheat$Scenario <- factor(df_Wheat$Scenario, levels=c("Ref_dCC","Ref_gCC"))
df_Wheat$Query <-factor(df_Wheat$Query, levels=c("Yield", "Land allocation","Profit rate"))
df_Wheat$Management <-factor(df_Wheat$Management, levels=c("RFD_hi","RFD_lo"))
ggplot(data=df_Wheat,aes(x = fct_relabel(Management,stringr::str_wrap,width = 20), y=Percent.change, shape=GCM))+
  geom_point(aes(color=Scenario), size=5)+
  labs(y="Impact_%")+
  scale_shape_manual(values=c(0,5,1,7,9,10))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=20)) +
  theme(axis.text.x=element_text(size=15), axis.text.y = element_text(size=12), strip.text = element_text(size=15))+
  scale_color_manual(values = c("#F8766D", "#619CFF"))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")


df_Rice <-filter(df, Crop=="Rice")
df_Rice$Scenario <- factor(df_Rice$Scenario, levels=c("Ref_dCC","Ref_gCC"))
df_Rice$Query <-factor(df_Rice$Query, levels=c("Yield", "Land allocation","Profit rate"))
df_Rice$Management <-factor(df_Rice$Management, levels=c("IRR_hi","IRR_lo","RFD_hi","RFD_lo"))
ggplot(data=df_Rice,aes(x = fct_relabel(Management,stringr::str_wrap,width = 20), y=Percent.change, shape=GCM))+
  geom_point(aes(color=Scenario), size=5)+
  labs(y="Impact_%")+
  scale_shape_manual(values=c(0,5,1,7,9,10))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=20)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=15))+
  scale_color_manual(values = c("#00C19F", "#F8766D", "#619CFF"))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")

##dCC vs. gCC domestic production comparison
df <-read.csv("dCC_gCC.csv")
df$Crop <-factor(df$Crop, levels=c("Rice", "Wheat","Soybeans"))
ggplot(data=df,aes(x=Scenario, y=PC, shape=GCM))+
  labs(title="Domestic Production")+
  theme_bw()+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  geom_point(size=5)+
  labs(y="Impact_%")+
  scale_shape_manual(values=c(0,5,1,7,9,10))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  geom_hline(yintercept=0, linetype='dashed')+
  coord_cartesian(ylim=c(-15,5))+
  facet_wrap(~Crop,scales="free")

##gCC producer and consumer
###Producer
df <-read.csv("gCC_prod.csv")
df_85 <-filter(df, RCP=="RCP85")
df_85$Crop <-factor(df_85$Crop, levels=c("Rice", "Wheat","Soybeans"))
df_85$Query <-factor(df_85$Query, levels=c("Revenue", "Domestic Production","Producer Price"))
ggplot(data=df_85,aes(x=Crop, y=PC, shape=GCM))+
  geom_point(size=5)+
  theme_bw()+
  labs(y="Impact_%")+
  scale_shape_manual(values=c(1,2,0))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")


###Consumer
df <-read.csv("gCC_cons.csv")
df_85 <-filter(df, RCP=="RCP85")
df_85$Crop <-factor(df_85$Crop, levels=c("Staples", "NonStaples"))
df_85$Query <-factor(df_85$Query, levels=c("Expenditure", "Food Price","Consumption"))
ggplot(data=df_85,aes(x=Crop, y=PC, shape=GCM))+
  geom_point(size=5)+
  theme_bw()+
  labs(y="Impact_%")+
  scale_shape_manual(values=c(1,2,0))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), strip.text = element_text(size=12))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")

## Ref vs. SS revenue
df <-read.csv("Ref_SS_rev.csv")
df$Crop <-factor(df$Crop, levels=c("Rice", "Wheat","Soybeans"))
ggplot(data=df,aes(fill=Query, x=Scenario, y=value))+
  labs(title='Revenue(including subsidy)', y='(mil 2015$)')+
  theme_bw()+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  geom_bar(position="stack", stat="identity", width=0.3)+
  scale_fill_manual(values = c("#CCCCCC","#999999"))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12), legend.title = element_blank(), legend.position = "bottom",strip.text = element_text(size=12)) +
  facet_wrap(~Crop,scales="free")

## Ref vs. SS expenditure
df <-read.csv("Ref_SS_exp.csv")
df$Crop <-factor(df$Crop, levels=c("Staples", "NonStaples"))
ggplot(data=df,aes(fill=Scenario, x=Scenario, y=value))+
  labs(title='Expenditure per capita', y='(2015$/day)')+
  theme_bw()+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  geom_bar(position="dodge", stat="identity", width=0.3)+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12),strip.text = element_text(size=12)) +
  scale_fill_manual(values = c("#999999","#999999"))+
  theme(legend.position = 'none')+
  facet_wrap(~Crop,scales="free")



## Ref vs. SS price comparison
df <-read.csv("Ref_SS_price.csv")
df$Crop <-factor(df$Crop, levels=c("Rice", "Wheat","Soybeans"))
p1<-ggplot(data=df,aes(fill=Condition, x=Scenario, y=value))+
  labs(title='Producer Price including subsidy', y='(2015$/kg)')+
  theme_bw()+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  geom_bar(position="stack", stat="identity", width=0.3)+
  scale_fill_manual(values = c("#CCCCCC","#999999"))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12), legend.title = element_blank(), legend.position = "bottom",strip.text = element_text(size=12)) +
  facet_wrap(~Crop,scales="free")

## Ref vs. SS production comparison
df <-read.csv("Ref_SS_prod.csv")
df$Crop <-factor(df$Crop, levels=c("Rice", "Wheat","Soybeans"))
p2<-ggplot(data=df,aes(fill=Query, x=Scenario, y=value))+
  labs(title='Domestic Production', y='(Mt)')+
  theme_bw()+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  geom_bar(position="dodge", stat="identity", width=0.3)+
  theme(legend.position = 'none')+
  theme(axis.title.x = element_blank(), axis.title.y = element_text(size=12), strip.text = element_text(size=12)) +
  scale_fill_manual(values = c("#999999"))+
  facet_wrap(~Crop,scales="free")

library(gridExtra)
grid.arrange(p2,p1)

## Ref vs. SS consumer
df <-read.csv("Ref_SS_cons.csv")
df$Crop <-factor(df$Crop, levels=c("Staples", "NonStaples"))
df$Query <-factor(df$Query, levels=c("Food Price(2015$/kcal)", "Consumption(kcal/day)"))
ggplot(data=df,aes(fill=Scenario, x=Crop, y=value))+
  theme_bw()+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  geom_bar(position="dodge", stat="identity", width=0.3)+
  theme(legend.title=element_blank(),legend.position = "bottom")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), strip.text = element_text(size=12)) +
  scale_fill_manual(values = c("#619CFF","#F8766D"))+
  facet_wrap(~Query,scales="free")


## Ref_gCC vs. SS_gCC
df <-read.csv("Ref_SS_CC_prod.csv")
df_r <-filter(df, Query=="Revenue", RCP=="RCP85")
df_r$Crop <-factor(df_r$Crop, levels=c("Rice", "Wheat","Soybeans"))
ggplot(data=df_r,aes(x=Scenario, y=PC, shape=GCM))+
  geom_point(size=5)+
  labs(title="Revenue",y="Impact_%")+
  theme_bw()+
  scale_shape_manual(values=c(1,2,0))+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_grid(~Crop,scales="free")

df_pp <-filter(df, Query!="Revenue", RCP=="RCP85")
df_pp$Crop <-factor(df_pp$Crop, levels=c("Rice", "Wheat","Soybeans"))
df_pp$Query <-factor(df_pp$Query, levels=c("Domestic Production", "Producer Price"))
ggplot(data=df_pp,aes(x = fct_relabel(Query,stringr::str_wrap,width = 10), y=PC, shape=GCM))+
  geom_point(aes(color=Scenario),size=5)+
  theme_bw()+
  labs(y="Impact_%")+
  scale_shape_manual(values=c(1,2,0))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=9), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  scale_color_manual(values = c("#619CFF","#F8766D"))+
  geom_hline(yintercept=0, linetype='dashed')+
  coord_cartesian(ylim=c(-15,15))+
  facet_grid(~Crop,scales="free")

df <-read.csv("Ref_SS_CC_cons.csv")
df_e <-filter(df, Query=="Expenditure", RCP=="RCP85")
df_e$Crop <-factor(df_e$Crop, levels=c("Staples", "NonStaples"))
ggplot(data=df_e,aes(x=Scenario, y=PC, shape=GCM))+
  geom_point(size=5)+
  theme_bw()+
  labs(title="Expenditure",y="Impact_%")+
  scale_shape_manual(values=c(1,2,0))+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=15)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  geom_hline(yintercept=0, linetype='dashed')+
  coord_cartesian(ylim=c(0,5))+
  facet_wrap(~Crop,scales="free")

df_fc <-filter(df, Query!="Expenditure", RCP=="RCP85")
df_fc$Crop <-factor(df_fc$Crop, levels=c("Staples", "NonStaples"))
df_fc$Query <-factor(df_fc$Query, levels=c("Food Price", "Consumption"))
ggplot(data=df_fc,aes(x=Query, y=PC, shape=GCM))+
  geom_point(aes(color=Scenario),size=5)+
  theme_bw()+
  labs(y="Impact_%")+
  scale_shape_manual(values=c(1,2,0))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  scale_color_manual(values = c("#619CFF","#F8766D"))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_grid(~Crop,scales="free")


##Appendix-RCP45 and 85
df <-read.csv("gCC_prod.csv")
df_rice <-filter(df, Crop=="Rice")
df_rice$Query <-factor(df_rice$Query, levels=c("Revenue", "Domestic Production","Producer Price"))
ggplot(data=df_rice,aes(x=RCP, y=PC, shape=GCM))+
  geom_point(size=5)+
  theme_bw()+
  labs(title='Rice', y="Impact_%")+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  scale_shape_manual(values=c(1,2,0))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  scale_color_manual(values = c("#61D04F", "#CD0BBC" ,"#2297E6"))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")

df <-read.csv("gCC_cons.csv")
df_staples <-filter(df, Crop=="Staples")
df_staples$Query <-factor(df_staples$Query, levels=c("Expenditure", "Food Price","Consumption"))
ggplot(data=df_staples,aes(x=RCP, y=PC, shape=GCM))+
  geom_point(size=5)+
  theme_bw()+
  labs(title='Staples', y="Impact_%")+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  scale_shape_manual(values=c(1,2,0))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), strip.text = element_text(size=12))+
  scale_color_manual(values = c("#619CFF","#F8766D"))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")

df <-read.csv("Ref_SS_CC_prod.csv")
df_rice <-filter(df, Crop=="Rice")
df_rice$Query <-factor(df_rice$Query, levels=c("Revenue", "Domestic Production","Producer Price"))
ggplot(data=df_rice,aes(x=Scenario, y=PC, shape=GCM))+
  geom_point(aes(color=RCP),size=5)+
  theme_bw()+
  labs(title="Rice",y="Impact_%")+
  scale_shape_manual(values=c(1,2,0))+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  scale_color_manual(values = c("#619CFF","#F8766D"))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")

df <-read.csv("Ref_SS_CC_cons.csv")
df_nonstaples <-filter(df, Crop=="NonStaples")
df_nonstaples$Query <-factor(df_nonstaples$Query, levels=c("Expenditure", "Food Price","Consumption"))
ggplot(data=df_nonstaples,aes(x=Scenario, y=PC, shape=GCM))+
  geom_point(aes(color=RCP),size=5)+
  theme_bw()+
  labs(title="NonStaples",y="Impact_%")+
  scale_shape_manual(values=c(1,2,0))+
  theme(plot.title = element_text(size=15, hjust = 0.5))+
  theme(axis.title.x = element_blank(),axis.title.y = element_text(size=12)) +
  theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=12), strip.text = element_text(size=12))+
  scale_color_manual(values = c("#619CFF","#F8766D"))+
  geom_hline(yintercept=0, linetype='dashed')+
  facet_wrap(~Query,scales="free")

