library(tidyverse)
library(rvest)
library(patchwork)
#https://www.color-hex.com/color-palette/3098  iceblue
#https://www.canva.com/learn/100-color-combinations/  14 Icy Blues and grays
c.pal2 <- c("#F1F1F2","#dcf3ff","#baf2ef","#a2d2df","#257ca3","#396d7c")

justices <- data.frame(justice=c("John_Roberts_(Supreme_Court)","Clarence_Thomas_(Supreme_Court)",
                                 "Ruth_Bader_Ginsburg","Stephen_Breyer","Samuel_Alito",
                                 "Sonia_Sotomayor","Elena_Kagan","Neil_Gorsuch",
                                 "Brett_Kavanaugh","Amy_Coney_Barrett"),
                       appointed=c("Bush","Bush","Clinton","Clinton","Bush",
                                   "Obama","Obama","Trump","Trump","Trump"),
                       year=c(2005,1991,1993,1994,2005,2009,2010,2017,2018,2020),
                       index=c(1,1,2,1,1,1,1,1,3,4),
                       table.no=c(3,4,2,6,3,3,6,5,10,9)
)

cons.b=c("Clarence Thomas","Samuel Alito","Amy Coney Barrett","Neil Gorsuch",
         "Brett Kavanaugh","John Roberts","Anthony Kennedy")
libs.b=c("Stephen Breyer","Elena Kagan","Ruth Bader Ginsburg",
         "Sonia Sotomayor")

sc_just <- c(cons.b,libs.b)
sc_just_1 <- gsub(" ","_",sc_just)

just_all <- c("Roberts","Scalia","Kennedy","Thomas","Alito","Gorsuch","Kavanaugh","Barrett",
              "Breyer","Ginsburg","Kagan","Sotomayor")
###
## prep data
###

SCDB_raw <- read_csv("SCDB_2022_01_justiceCentered_Citation.csv") %>%
  filter(chief=="Roberts",term>2012) %>%
  select(term,caseName,justiceName,vote,majVotes,minVotes)   %>%
  pivot_wider(names_from=justiceName,values_from=vote) %>%
  rename(Roberts=JGRoberts,
         Scalia=AScalia,
         Kennedy=AMKennedy,
         Thomas=CThomas,
         Ginsburg=RBGinsburg,
         Breyer=SGBreyer,
         Alito=SAAlito,
         Sotomayor=SSotomayor,
         Kagan=EKagan,
         Gorsuch=NMGorsuch,
         Kavanaugh=BMKavanaugh,
         Barrett=ACBarrett
  )

SCDB <- SCDB_raw[,just_all]
SCDB[SCDB == 3 | SCDB == 4 | SCDB == 5] <- 1

libs_all <- c("Breyer","Ginsburg","Kagan","Sotomayor")
cons_all <- c("Alito","Barrett","Gorsuch","Kavanaugh","Kennedy","Roberts","Scalia","Thomas")


#######
#######  3 justice agreement plots
#######

### generate triangle
ht=sqrt(3)/2
ggplot()+
  geom_polygon(data=data.frame(x=c(0,.5,1),y=c(0,ht,0)),aes(x=x,y=y),fill=NA,col="black")+
  geom_polygon(data=data.frame(x=c(1/3,.5,2/3),y=c(1/(3*sqrt(3)),ht-1/3,1/(3*sqrt(3)))),
               aes(x=x,y=y),fill=NA,col="black")+
  geom_segment(aes(x=0,xend=1/3,y=0,yend=1/(3*sqrt(3))))+
  geom_segment(aes(x=2/3,xend=1,y=1/(3*sqrt(3)),yend=0))+
  geom_segment(aes(x=.5,xend=.5,y=ht-1/3,yend=ht))+
  geom_point(aes(x=.5,y=1/(2*sqrt(3))))
ggsave("triangle.png",dpi=320,height=5.1,width=6)

### generate justice plots

scotus_year=2021
just_all_year <- case_when(
  scotus_year >= 2020 ~ c("Alito","Barrett","Gorsuch","Kavanaugh","Roberts","Thomas","Breyer","Kagan","Sotomayor"),
  scotus_year == 2018 | scotus_year == 2019 ~ c("Alito","Gorsuch","Kavanaugh","Roberts","Thomas","Breyer","Ginsburg","Kagan","Sotomayor"),
  scotus_year == 2017 ~ c("Alito","Gorsuch","Roberts","Scalia","Thomas","Breyer","Ginsburg","Kagan","Sotomayor"),
  scotus_year <= 2016 ~ c("Alito","Kennedy","Roberts","Scalia","Thomas","Breyer","Ginsburg","Kagan","Sotomayor")
)

libs <- setdiff(just_all_year,cons_all)
cons <- setdiff(just_all_year,libs_all)

# non-unanimous cases
for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_3j_n")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year,minVotes != 0) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_3(cons[i],libs,.,c.pal2)
  )
}

sc_cons_2020 <- list(Samuel_Alito_3j_n,Amy_Coney_Barrett_3j_n,
                     Neil_Gorsuch_3j_n,Brett_Kavanaugh_3j_n,
                     John_Roberts_3j_n,Clarence_Thomas_3j_n)
for(i in 1:length(sc_cons_2020)) {
  ggsave(paste0(sc_just_1[grep(cons[i],sc_just_1)],"_3_justice_non_unam.png"),sc_cons_2020[[i]],
         dpi=320,height=6.7,width=7.7,bg="white")
}

n_cases <- SCDB_raw %>% filter(term==scotus_year,minVotes != 0) %>% nrow()

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_3j_n2")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year,minVotes != 0) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_3(cons[i],libs,.,c.pal2)+theme(plot.subtitle=element_blank())
  )
}

(plot_spacer()+plot_spacer())/
  (Samuel_Alito_3j_n2+Amy_Coney_Barrett_3j_n2)/
  (Neil_Gorsuch_3j_n2+Brett_Kavanaugh_3j_n2)/
  (John_Roberts_3j_n2+Clarence_Thomas_3j_n2)+
  plot_layout(heights = c(.01,2,2,2))+
  plot_annotation(title=paste("Agreement with Liberal Justices", scotus_year),
                  subtitle=paste("non-unamimous cases (n=",n_cases,")"), ### fix this 
                  theme=theme(plot.title=element_text(hjust=.5,size=32),
                              plot.subtitle=element_text(hjust=.5,size=20)))+
  plot_layout(guides="collect") & theme(legend.position="top",
                                        legend.key.size=unit(1,'cm'),
                                        legend.title=element_text(size=16),
                                        legend.key.height=unit(.75,'cm'),
                                        panel.background = element_rect(fill="gray90",
                                                                        color="gray90"),
                                        plot.background = element_rect(color="gray90",
                                                                       fill="gray90"))
ggsave("justices_3_justice_non_unam.png",dpi=320,height=20,width=15)

# all cases
for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_3j_a")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_3(cons[i],libs,.,c.pal2)
  )
}

sc_cons_2020 <- list(Samuel_Alito_3j_a,Amy_Coney_Barrett_3j_a,
                     Neil_Gorsuch_3j_a,Brett_Kavanaugh_3j_a,
                     John_Roberts_3j_a,Clarence_Thomas_3j_a)
for(i in 1:length(sc_cons_2020)) {
  ggsave(paste0(sc_just_1[grep(cons[i],sc_just_1)],"_3_justice_all.png"),sc_cons_2020[[i]],
         dpi=320,height=6.5,width=7.5,bg="white")
  #dpi=320,height=5.1,width=6,bg="white")
}

n_cases <- SCDB_raw %>% filter(term==scotus_year) %>% nrow()

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_3j_a2")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_3(cons[i],libs,.,c.pal2)+theme(plot.subtitle=element_blank())
  )
}

(plot_spacer()+plot_spacer())/
  (Samuel_Alito_3j_a2+Amy_Coney_Barrett_3j_a2)/
  (Neil_Gorsuch_3j_a2+Brett_Kavanaugh_3j_a2)/
  (John_Roberts_3j_a2+Clarence_Thomas_3j_a2)+
  plot_layout(heights = c(.01,2,2,2))+
  plot_annotation(title=paste("Agreement with Liberal Justices", scotus_year),
                  subtitle=paste("all cases (n=",n_cases,")"), ### fix this 
                  theme=theme(plot.title=element_text(hjust=.5,size=32),
                              plot.subtitle=element_text(hjust=.5,size=20)))+
  plot_layout(guides="collect") & theme(legend.position="top",
                                        legend.key.size=unit(1,'cm'),
                                        legend.title=element_text(size=16),
                                        legend.key.height=unit(.75,'cm'),
                                        panel.background = element_rect(fill="gray90",
                                                                        color="gray90"),
                                        plot.background = element_rect(color="gray90",
                                                                       fill="gray90"))
ggsave("justices_3_justice_all.png",dpi=320,height=20,width=15)
