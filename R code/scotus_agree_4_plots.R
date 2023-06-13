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
  select(term,caseName,justiceName,majority,majVotes,minVotes)   %>%
  pivot_wider(names_from=justiceName,values_from=majority) %>%
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

libs_all <- c("Breyer","Ginsburg","Kagan","Sotomayor")
cons_all <- c("Alito","Barrett","Gorsuch","Kavanaugh","Kennedy","Roberts","Scalia","Thomas")


#######
#######  4
#######

scotus_year=2019
just_all_year <- case_when(
  scotus_year >= 2020 ~ c("Alito","Barrett","Gorsuch","Kavanaugh","Roberts","Thomas","Breyer","Kagan","Sotomayor"),
  scotus_year == 2018 | scotus_year == 2019 ~ c("Alito","Gorsuch","Kavanaugh","Roberts","Thomas","Breyer","Ginsburg","Kagan","Sotomayor"),
  scotus_year == 2017 ~ c("Alito","Gorsuch","Roberts","Scalia","Thomas","Breyer","Ginsburg","Kagan","Sotomayor"),
  scotus_year <= 2016 ~ c("Alito","Kennedy","Roberts","Scalia","Thomas","Breyer","Ginsburg","Kagan","Sotomayor")
)

libs <- setdiff(just_all_year,cons_all)
cons <- setdiff(just_all_year,libs_all)

# agree w/liberals non-unanimous

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_nl")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year,minVotes != 0) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],libs,.,c.pal2)
  )
}

sc_cons_2019 <- list(Samuel_Alito_4j_nl,
                     Neil_Gorsuch_4j_nl,Brett_Kavanaugh_4j_nl,
                     John_Roberts_4j_nl,Clarence_Thomas_4j_nl)
for(i in 1:length(sc_cons_2019)) {
  ggsave(paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4_justice_non_unam_lib.png"),sc_cons_2019[[i]],
         dpi=320,height=8.6,width=8.6)
}

n_cases <- SCDB_raw %>% filter(term==scotus_year,minVotes != 0) %>% nrow()

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_nl2")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year,minVotes != 0) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],libs,.,c.pal2)+theme(plot.subtitle=element_blank())
  )
}

((plot_spacer()+plot_spacer())/
    (Neil_Gorsuch_4j_nl2+Brett_Kavanaugh_4j_nl2)/
    (plot_spacer()+plot_spacer())/
    (Samuel_Alito_4j_nl2+Clarence_Thomas_4j_nl2)+
    plot_layout(heights = c(.01,2,.3,2))+
    plot_annotation(title=paste("Agreement with Liberal Justices", scotus_year),
                    subtitle=paste("non-unamimous cases (n=",n_cases,")"),
                    theme=theme(plot.title=element_text(hjust=.5,size=32),
                                plot.subtitle=element_text(hjust=.5,size=24)))+
    plot_layout(guides="collect") & theme(legend.position="top",
                                          legend.key.size=unit(1,'cm'),
                                          legend.title=element_text(size=16),
                                          panel.background = element_rect(fill="gray90",
                                                                          color="gray90"),
                                          plot.background = element_rect(color="gray90",
                                                                         fill="gray90")))+
  inset_element(John_Roberts_4j_nl2+theme(legend.position = "none",
                                         plot.background = element_rect(fill=NA,color=NA),
                                         panel.background = element_rect(fill=NA,color=NA)),
                left=.25,top=1.65,bottom=.65,right=.75)

ggsave("justics_4_justice_non_unam_lib.png",dpi=320,height=19,width=18)

# agree w/liberals all cases
for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_al")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],libs,.,c.pal2)
  )
}

sc_cons_2019 <- list(Samuel_Alito_4j_al,
                     Neil_Gorsuch_4j_al,Brett_Kavanaugh_4j_al,
                     John_Roberts_4j_al,Clarence_Thomas_4j_al)
for(i in 1:length(sc_cons_2019)) {
  ggsave(paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4_justice_all_lib.png"),sc_cons_2019[[i]],
         dpi=320,height=8.6,width=8.6)
}

n_cases <- SCDB_raw %>% filter(term==scotus_year) %>% nrow()

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_al2")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],libs,.,c.pal2)+theme(plot.subtitle=element_blank())
  )
}

((plot_spacer()+plot_spacer())/
    (Neil_Gorsuch_4j_al2+Brett_Kavanaugh_4j_al2)/
    (plot_spacer()+plot_spacer())/
    (Samuel_Alito_4j_al2+Clarence_Thomas_4j_al2)+
    plot_layout(heights = c(.01,2,.3,2))+
    plot_annotation(title=paste("Agreement with Liberal Justices", scotus_year),
                    subtitle=paste("all cases (n=",n_cases,")"),
                    theme=theme(plot.title=element_text(hjust=.5,size=32),
                                plot.subtitle=element_text(hjust=.5,size=24)))+
    plot_layout(guides="collect") & theme(legend.position="top",
                                          legend.key.size=unit(1,'cm'),
                                          legend.title=element_text(size=16),
                                          panel.background = element_rect(fill="gray90",
                                                                          color="gray90"),
                                          plot.background = element_rect(color="gray90",
                                                                         fill="gray90")))+
  inset_element(John_Roberts_4j_al2+theme(legend.position = "none",
                                         plot.background = element_rect(fill=NA,color=NA),
                                         panel.background = element_rect(fill=NA,color=NA)),
                left=.25,top=1.65,bottom=.65,right=.75)

ggsave("justics_4_justice_all_lib.png",dpi=320,height=19,width=18)


# agree w/conservatives non-unanimous

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_nc")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year,minVotes != 0) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],cons[-i],.,c.pal2)
  )
}

sc_cons_2019 <- list(Samuel_Alito_4j_nc,
                     Neil_Gorsuch_4j_nc,Brett_Kavanaugh_4j_nc,
                     John_Roberts_4j_nc,Clarence_Thomas_4j_nc)
for(i in 1:length(sc_cons_2019)) {
  ggsave(paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4_justice_non_unam_con.png"),sc_cons_2019[[i]],
         dpi=320,height=8.5,width=8.5,bg="white")
}

n_cases <- SCDB_raw %>% filter(term==scotus_year,minVotes != 0) %>% nrow()

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_nc2")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year,minVotes != 0) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],cons[-i],.,c.pal2)+theme(plot.subtitle=element_blank())
  )
}

((plot_spacer()+plot_spacer())/
    (Neil_Gorsuch_4j_nc2+Brett_Kavanaugh_4j_nc2)/
    (plot_spacer()+plot_spacer())/
    (Samuel_Alito_4j_nc2+Clarence_Thomas_4j_nc2)+
    plot_layout(heights = c(.01,2,.3,2))+
    plot_annotation(title=paste("Agreement with Conservative Justices", scotus_year),
                    subtitle=paste("non-unamimous cases (n=",n_cases,")"),
                    theme=theme(plot.title=element_text(hjust=.5,size=32),
                                plot.subtitle=element_text(hjust=.5,size=24)))+
    plot_layout(guides="collect") & theme(legend.position="top",
                                          legend.key.size=unit(1,'cm'),
                                          legend.title=element_text(size=16),
                                          panel.background = element_rect(fill="gray90",
                                                                          color="gray90"),
                                          plot.background = element_rect(color="gray90",
                                                                         fill="gray90")))+
  inset_element(John_Roberts_4j_nc2+theme(legend.position = "none",
                                         plot.background = element_rect(fill=NA,color=NA),
                                         panel.background = element_rect(fill=NA,color=NA)),
                left=.25,top=1.65,bottom=.65,right=.75)

ggsave("justics_4_justice_non_unam_con.png",dpi=320,height=19,width=18)

# agree w/conservatives all cases
for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_ac")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],cons[-i],.,c.pal2)
  )
}

sc_cons_2019 <- list(Samuel_Alito_4j_ac,
                     Neil_Gorsuch_4j_ac,Brett_Kavanaugh_4j_ac,
                     John_Roberts_4j_ac,Clarence_Thomas_4j_ac)
for(i in 1:length(sc_cons_2019)) {
  ggsave(paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4_justice_all_con.png"),sc_cons_2019[[i]],
         dpi=320,height=8.5,width=8.5,bg="white")
}

n_cases <- SCDB_raw %>% filter(term==scotus_year) %>% nrow()

for(i in 1:length(cons)) {
  scj <- paste0(sc_just_1[grep(cons[i],sc_just_1)],"_4j_ac2")
  assign(scj,
         SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
           filter(term==scotus_year) %>%
           select(-setdiff(just_all,just_all_year)) %>% 
           select(-c(term,minVotes)) %>% data.frame() %>%
           scotus_agree_4(cons[i],cons[-i],.,c.pal2)+theme(plot.subtitle=element_blank())
  )
}

((plot_spacer()+plot_spacer())/
    (Neil_Gorsuch_4j_ac2+Brett_Kavanaugh_4j_ac2)/
    (plot_spacer()+plot_spacer())/
    (Samuel_Alito_4j_ac2+Clarence_Thomas_4j_ac2)+
    plot_layout(heights = c(.01,2,.3,2))+
    plot_annotation(title=paste("Agreement with Conservative Justices", scotus_year),
                    subtitle=paste("all cases (n=",n_cases,")"),
                    theme=theme(plot.title=element_text(hjust=.5,size=32),
                                plot.subtitle=element_text(hjust=.5,size=24)))+
    plot_layout(guides="collect") & theme(legend.position="top",
                                          legend.key.size=unit(1,'cm'),
                                          legend.title=element_text(size=16),
                                          panel.background = element_rect(fill="gray90",
                                                                          color="gray90"),
                                          plot.background = element_rect(color="gray90",
                                                                         fill="gray90")))+
  inset_element(John_Roberts_4j_ac2+theme(legend.position = "none",
                                         plot.background = element_rect(fill=NA,color=NA),
                                         panel.background = element_rect(fill=NA,color=NA)),
                left=.25,top=1.65,bottom=.65,right=.75)

ggsave("justics_4_justice_all_con.png",dpi=320,height=19,width=18)






