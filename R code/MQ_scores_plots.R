## plots for Martin Quinn Scores

## https://mqscores.lsa.umich.edu/measures.php - website for the 

## load packages and data - will also need to install geomtextpath
library(tidyverse)
mqscore <- read.csv("justices.csv")


## data to use with mqscore for plotting

mqscore.names=data.frame(
  name=c("John Roberts","Clarence Thomas","Stephen Breyer",
         "Samuel Alito","Sonia Sotomayor","Elena Kagan",
         "Neil Gorsuch","Brett Kavanaugh","Amy Coney Barrett"),
  justiceName=c("JGRoberts","CThomas","SGBreyer","SAAlito","SSotomayor",
                "EKagan","NMGorsuch","BMKavanaugh","ACBarrett"),
  party=c("red","red","blue","red","blue","blue","red","red","red")
)


## bar plot of MQ Scores for 2021

mqscore %>% filter(term==2021) %>%
  select(justiceName,post_mn) %>%
  left_join(mqscore.names) %>%
  mutate(post_mn2=ifelse(party=="red",1,-1)) %>%
  pivot_longer(cols=starts_with("post"), names_to="xxx",values_to="score") %>%
  mutate(party=ifelse(score==1 | score==-1,"white",party),
         justiceName=fct_reorder(factor(justiceName),score)) %>%
  ggplot(aes(score,justiceName))+
  geom_col(aes(fill=party),width=.25)+
  scale_fill_manual(values=c("blue","red","white"))+
  geom_text(data=data.frame(score=rep(0,9),name=mqscore.names$justiceName,label=mqscore.names$name),
            aes(x=score,y=name,label=label),size=6.5)+
  geom_text(data=mqscore %>% filter(term==2021) %>% 
              mutate(score=ifelse(post_mn<0,post_mn-1.25,post_mn+1.25)),
            aes(x=score,y=justiceName,label=round(post_mn,2)),size=5)+
  xlim(-5.5,5.5)+
  scale_y_discrete(limits=rev)+
  theme_void()+
  labs(title="Martin-Quinn Scores 2021-22")+
  theme(legend.position = "none",
        plot.title=element_text(hjust=.5,size=24))

## save plot

ggsave("MQscores2021.png",dpi=320,height=8,width=14,bg="white")


## data to use with mqscore for 2017-2021 scores

mqscore.names=data.frame(
  name=c("John Roberts","Clarence Thomas","Ruth Bader Ginsburg","Stephen Breyer",
         "Samuel Alito","Sonia Sotomayor","Elena Kagan",
         "Neil Gorsuch","Brett Kavanaugh","Amy Coney Barrett"),
  justiceName=c("JGRoberts","CThomas","RBGinsburg","SGBreyer","SAAlito","SSotomayor",
                "EKagan","NMGorsuch","BMKavanaugh","ACBarrett"),
  party=c("red","red","blue","blue","red","blue","blue","red","red","red"),
  #vjust=c(1.5,-1,-1,1.5,-1,-1,-1,-1,0,-1),
  hjust=c(.35,0.35,.9,0.35,0.35,0.35,0.35,0.35,0.5,.5)
)


## line plot 2017-2021

mqscore %>% filter(term >= 2017, justiceName != "AMKennedy") %>%
  select(justiceName,post_mn,term) %>%
  left_join(mqscore.names) %>%
  mutate(justiceName=fct_reorder(factor(justiceName),post_mn)) %>%
  ggplot(aes(term,post_mn,group=justiceName))+
  geom_line(data=
              data.frame(x=c(rep(2016.7,3),rep(2021.25,3)),
                         y=rep(c(-4,-2,2),2),
                         justiceName=rep(letters[1:3],2)),
            aes(x=x,y=y,group=justiceName),col="gray90")+
  geom_segment(x=2016.7,y=0,xend=2021.5,yend=0)+
  geom_point(aes(col=party),size=4)+
  geomtextpath::geom_textline(aes(label=name,linecolor=party,hjust=hjust),size=6.5,lty=2,
                              linewidth=.75)+
  geom_segment(aes(x=2021.4,y=2.5,xend=2021.4,yend=3.5),col="red",arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geom_segment(aes(x=2021.4,y=-2.2,xend=2021.4,yend=-3.5),col="blue",arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geomtextpath::geom_textline(data=data.frame(x=c(2021.4,2021.4),y=c(.5,2.5),justiceName=NA),
                              aes(x,y,label="More Conservative"),size=6,linecolor=NA)+
  geomtextpath::geom_textline(data=data.frame(x=c(2021.4,2021.4),y=c(-.5,-2.5),justiceName=NA),
                              aes(x,y,label="More Liberal"),size=6,angle=180,linecolor=NA,)+
  geom_text(data=mqscore %>% filter(term>=2017,justiceName != "AMKennedy") %>% 
              group_by(justiceName) %>% slice_min(order_by=term),
            aes(x=term-.15,y=post_mn,label=post_mn),col="gray20",size=4.5)+
  geom_text(data=mqscore %>% filter(term>=2017,justiceName != "AMKennedy") %>%
              group_by(justiceName) %>% slice_max(order_by=term),
            aes(x=term+.15,y=post_mn,label=post_mn),col="gray20",size=4.5)+
  geom_text(aes(x=2016.7,y=0,label=0),hjust=1.5,size=5,col="gray60")+
  ylim(-4.2,3.5)+
  xlim(2016.7,2021.5)+
  scale_color_manual(values=c("blue","red"))+
  labs(
    title="Martin-Quinn Scores 2017-2021",
    subtitle="(https://mqscores.lsa.umich.edu/measures.php)",
    caption='\nSource:  Andrew D. Martin and Kevin M. Quinn. 2002.  "Dynamic Ideal Point Estimation via Markov Chain Monte Carlo for the U.S. Supreme Court, 1953-1999."  Political Analysis. 10:134-153\n'
  )+
  theme_bw()+
  theme(legend.position = "none",
        plot.title=element_text(hjust=.5,size=24),
        plot.subtitle=element_text(hjust=.5,size=16),
        plot.caption=element_text(hjust=.5),
        axis.title=element_blank(),
        axis.text.x=element_text(size=12),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border=element_blank())

ggsave("MQscores_all.png",dpi=320,height=10,width=16,bg="white")


## Alternate plot 2017-2021

mqscore.names=data.frame(
  name=c("John Roberts","Clarence Thomas","Ruth Bader Ginsburg","Stephen Breyer",
         "Samuel Alito","Sonia Sotomayor","Elena Kagan",
         "Neil Gorsuch","Brett Kavanaugh","Amy Coney Barrett"),
  justiceName=c("JGRoberts","CThomas","RBGinsburg","SGBreyer","SAAlito","SSotomayor",
                "EKagan","NMGorsuch","BMKavanaugh","ACBarrett"),
  party=c("red","red","blue","blue","red","blue","blue","red","red","red")
) %>% left_join(
  mqscore %>% filter(term>=2017,justiceName != "AMKennedy") %>%
    group_by(justiceName) %>%
    summarize(post_mn_sd=sd(post_mn),post_mn=mean(post_mn))
)

mqscore %>% filter(term >= 2017, justiceName != "AMKennedy") %>%
  select(justiceName,post_mn,term) %>%
  left_join(mqscore.names) %>%
  mutate(justiceName=fct_reorder(factor(justiceName),post_mn)) %>%
  ggplot(aes(post_mn,justiceName))+
  geom_vline(xintercept=0,col="gray60",lty=2)+
  geom_segment(data=data.frame(x=c(-5:-1,1:5),y=rep(0,10),xend=c(-5:-1,1:5),yend=rep(10.5,10)),
               aes(x=x,y=y,xend=xend,yend=yend),col="gray90")+
  geom_point(aes(alpha=term),size=4)+
  geom_text(data=mqscore.names,aes(post_mn,justiceName,label=name),size=6,vjust=1.75)+
  scale_x_continuous(breaks=-5:5,labels=-5:5,limits=c(-5,5))+
  scale_y_discrete(expand=c(.1,.25))+
  labs(
    title="Martin-Quinn Scores 2017-2021"
  )+
  geom_segment(aes(x=2.5,xend=4.5,y=11,yend=11),arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geom_segment(aes(x=-2.5,xend=-4.5,y=11,yend=11),arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geomtextpath::geom_textline(data=data.frame(x=c(.5,2.5),y=c(11,11),justiceName=NA),
                              aes(x,y,label="More Conservative"),size=6,linecolor=NA)+
  geomtextpath::geom_textline(data=data.frame(x=c(-.5,-2.5),y=c(11,11),justiceName=NA),
                              aes(x,y,label="More Liberal"),size=6,linecolor=NA)+
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    plot.title=element_text(size=24,hjust=.5),
    axis.title=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=12),
    panel.grid=element_blank()
  )

ggsave("MQscores_2017_2021_alt.png",dpi=320,height=8,width=12,bg="white")


## line plot 2012-2021

mqscore.names=data.frame(
  name=c("John Roberts","Clarence Thomas","Ruth Bader Ginsburg","Stephen Breyer",
         "Samuel Alito","Sonia Sotomayor","Elena Kagan",
         "Neil Gorsuch","Brett Kavanaugh","Amy Coney Barrett","Anthony Kennedy","Antonin Scalia"),
  justiceName=c("JGRoberts","CThomas","RBGinsburg","SGBreyer","SAAlito","SSotomayor",
                "EKagan","NMGorsuch","BMKavanaugh","ACBarrett","AMKennedy","AScalia"),
  party=c("red","red","blue","blue","red","blue","blue","red","red","red","red","red"),
  vjust=c(-1,-1,-1,1.5,-1,-1,-1,-1,-1,-1,1.5,1.5),
  hjust=c(.35,0.35,.9,0.35,0.35,0.35,0.35,0.35,0.5,.5,0,0)
)

kennedy <- mqscore %>% filter(term >= 2012, justiceName=="AMKennedy") %>% 
  rbind(mqscore %>% filter(term > 2012, term < 2017, justiceName=="AMKennedy")) %>%  
  arrange(term) %>% 
  mutate(group=rep(1:5,each=2),colorc=rep(c("red","blue","blue","blue","red"),each=2))

mqscore %>% filter(term >= 2012,justiceName != "AMKennedy") %>%
  select(justiceName,post_mn,term) %>%
  left_join(mqscore.names) %>%
  mutate(justiceName=fct_reorder(factor(justiceName),post_mn)) %>%
  ggplot(aes(term,post_mn,group=justiceName))+
  geom_line(data=
              data.frame(x=c(rep(2011.5,3),rep(2021.25,3)),
                         y=rep(c(-4,-2,2),2),
                         justiceName=rep(letters[1:3],2)),
            aes(x=x,y=y,group=justiceName),col="gray90")+
  geom_segment(x=2011.5,y=0,xend=2021.5,yend=0)+
  geom_point(aes(col=party),size=3)+
  geomtextpath::geom_textline(aes(label=name,linecolor=party,vjust=vjust),size=4.5,lty=2,
                              linewidth=.75)+
  geom_line(data=kennedy,
            aes(x=term,y=post_mn,col=colorc,group=group),lty=2,size=.75)+
  geom_point(data=mqscore %>% filter(term >= 2012, justiceName=="AMKennedy") %>% mutate(colorc=ifelse(post_mn>0,"red","blue")),
             aes(x=term,y=post_mn,col=colorc),size=3)+
  geomtextpath::geom_textline(data=kennedy %>% select(-group,-colorc),
                              aes(x=term,y=post_mn,label="Anthony Kennedy"),linecolor=NA,size=4.5,vjust=1.5)+
  geom_segment(aes(x=2021.9,y=2.5,xend=2021.9,yend=3.5),col="red",arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geom_segment(aes(x=2021.9,y=-2.2,xend=2021.9,yend=-3.5),col="blue",arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geomtextpath::geom_textline(data=data.frame(x=c(2021.9,2021.9),y=c(.5,2.5),justiceName=NA),
                              aes(x,y,label="More Conservative"),size=6,linecolor=NA)+
  geomtextpath::geom_textline(data=data.frame(x=c(2021.9,2021.9),y=c(-.5,-2.5),justiceName=NA),
                              aes(x,y,label="More Liberal"),size=6,angle=180,linecolor=NA)+
  geom_text(data=mqscore %>% filter(term>=2012) %>% 
              group_by(justiceName) %>% slice_min(order_by=term),
            aes(x=term-.3,y=post_mn,label=post_mn),col="gray20",size=4)+
  geom_text(data=mqscore %>% filter(term>=2012) %>%
              group_by(justiceName) %>% slice_max(order_by=term),
            aes(x=term+.3,y=post_mn,label=post_mn),col="gray20",size=4)+
  geom_text(aes(x=2011.5,y=0,label=0),hjust=2,size=4,col="gray40")+
  geom_text(data=data.frame(x=rep(2011.5,3),y=c(-4,-2,2),label=c(-4,-2,2),justiceName=NA),
            aes(x=x,y=y,label=label),hjust=2,col="gray40",size=3)+
  ylim(-4.2,3.5)+
  scale_color_manual(values=c("blue","red"))+
  scale_x_continuous(breaks=2012:2021,labels=2012:2021,limits=c(2011.5,2022))+
  labs(
    title="Martin-Quinn Scores 2012-2021",
    subtitle="(https://mqscores.lsa.umich.edu/measures.php)",
    caption='\nSource:  Andrew D. Martin and Kevin M. Quinn. 2002.  "Dynamic Ideal Point Estimation via Markov Chain Monte Carlo for the U.S. Supreme Court, 1953-1999."  Political Analysis. 10:134-153\n'
  )+
  theme_bw()+
  theme(legend.position = "none",
        plot.title=element_text(hjust=.5,size=24),
        plot.subtitle=element_text(hjust=.5,size=16),
        plot.caption=element_text(hjust=.5),
        axis.title=element_blank(),
        axis.text.x=element_text(size=12),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border=element_blank())
ggsave("MQscores_2012_2021.png",dpi=320,height=10,width=16,bg="white")





