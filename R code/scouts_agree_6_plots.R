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

#####
##### 6
#####

x.black.bar <- data.frame(
  x=c(.5,.9,1.3,1.7,2.1,.8,1.2,1.6,2,2.4),
  y=rep(1,10),
  grp = rep(1:5,2)
)

cons2 <- c("SA","ACB","NG","BK","JR","CT")

scotus_year=2021
just_all_year <- case_when(
  scotus_year >= 2020 ~ c("Alito","Barrett","Gorsuch","Kavanaugh","Roberts","Thomas","Breyer","Kagan","Sotomayor"),
  scotus_year == 2018 | scotus_year == 2019 ~ c("Alito","Gorsuch","Kavanaugh","Roberts","Thomas","Breyer","Ginsburg","Kagan","Sotomayor"),
  scotus_year == 2017 ~ c("Alito","Gorsuch","Roberts","Scalia","Thomas","Breyer","Ginsburg","Kagan","Sotomayor"),
  scotus_year <= 2016 ~ c("Alito","Kennedy","Roberts","Scalia","Thomas","Breyer","Ginsburg","Kagan","Sotomayor")
)

libs <- setdiff(just_all_year,cons_all)
cons <- setdiff(just_all_year,libs_all)

j_abbr <- function(x) {
  for(i in 1:length(cons)){
    x <- gsub(cons[i],cons2[i],x)
  }
  return(x)
}

# non-unanimous

data <- SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
  filter(term==scotus_year,minVotes != 0) %>%
  select(-setdiff(just_all,just_all_year)) %>% 
  select(-c(term,minVotes)) %>% data.frame()

for(i in 1:length(libs)) {
  scj <- paste0(sc_just_1[grep(libs[i],sc_just_1)],"_6j_n")
  assign(scj,scotus_agree_6(libs[i],cons,data,c.pal2))
}

sc_libs_2020 <- list(Stephen_Breyer_6j_n,Elena_Kagan_6j_n,Sonia_Sotomayor_6j_n)
for(i in 1:length(sc_libs_2020)) {
  ggsave(paste0(sc_just[grep(libs[i],sc_just)],"_6_justice_non_unam.png"),sc_libs_2020[[i]],
         dpi=320,height=12,width=12,bg="white")
}

n_cases <- data %>% nrow()

for(i in 1:length(libs)) {
  scj <- paste0(sc_just_1[grep(libs[i],sc_just_1)],"_6j_n.2")
  assign(scj,scotus_agree_6.2(libs[i],cons,data,c.pal2))
}

(plot_spacer()+plot_layout(widths=2.2))/
  ((Stephen_Breyer_6j_n.2)+plot_spacer()+
     (Elena_Kagan_6j_n.2)+plot_layout(widths = c(1,.2,1)))/
  (plot_spacer()+
     (Sonia_Sotomayor_6j_n.2)+plot_spacer()+plot_layout(widths=c(.6,1,.6)))+
  plot_annotation(title=paste("Agreement with Conservative Justices", scotus_year),
                  subtitle=paste("non-unanimous cases (n=",n_cases,")"),
                  theme=theme(plot.title=element_text(hjust=.5,size=48),
                              plot.subtitle=element_text(hjust=.5,size=32)))+
  plot_layout(guides="collect",heights=c(.05,1,1)) & theme(legend.position="top",
                                                           legend.key.size=unit(1,'cm'),
                                                           legend.title=element_text(size=24),
                                                           panel.background = element_rect(fill="gray90",
                                                                                           color="gray90"),
                                                           plot.background = element_rect(color="gray90",
                                                                                          fill="gray90"))
ggsave("justices_6_justice_non_unam.png",dpi=320,height=28,width=28)


# all cases

data <- SCDB %>% mutate(term=SCDB_raw$term, minVotes=SCDB_raw$minVotes) %>%
  filter(term==scotus_year) %>%
  select(-setdiff(just_all,just_all_year)) %>% 
  select(-c(term,minVotes)) %>% data.frame()

for(i in 1:length(libs)) {
  scj <- paste0(sc_just_1[grep(libs[i],sc_just_1)],"_6j_a")
  assign(scj,scotus_agree_6(libs[i],cons,data,c.pal2))
}

sc_libs_2020 <- list(Stephen_Breyer_6j_a,Elena_Kagan_6j_a,Sonia_Sotomayor_6j_a)
for(i in 1:length(sc_libs_2020)) {
  ggsave(paste0(sc_just[grep(libs[i],sc_just)],"_6_justice_all.png"),sc_libs_2020[[i]],
         dpi=320,height=12,width=12,bg="white")
}

n_cases <- data %>% nrow()

for(i in 1:length(libs)) {
  scj <- paste0(sc_just_1[grep(libs[i],sc_just_1)],"_6j_a.2")
  assign(scj,scotus_agree_6.2(libs[i],cons,data,c.pal2))
}

(plot_spacer()+plot_layout(widths=2.2))/
  ((Stephen_Breyer_6j_a.2)+plot_spacer()+
     (Elena_Kagan_6j_a.2)+plot_layout(widths = c(1,.2,1)))/
  (plot_spacer()+
     (Sonia_Sotomayor_6j_a.2)+plot_spacer()+plot_layout(widths=c(.6,1,.6)))+
  plot_annotation(title=paste("Agreement with Conservative Justices", scotus_year),
                  subtitle=paste("all cases (n=",n_cases,")"),
                  theme=theme(plot.title=element_text(hjust=.5,size=48),
                              plot.subtitle=element_text(hjust=.5,size=32)))+
  plot_layout(guides="collect",heights=c(.05,1,1)) & theme(legend.position="top",
                                                           legend.key.size=unit(1,'cm'),
                                                           legend.title=element_text(size=24),
                                                           panel.background = element_rect(fill="gray90",
                                                                                           color="gray90"),
                                                           plot.background = element_rect(color="gray90",
                                                                                          fill="gray90"))
ggsave("justices_6_justice_all.png",dpi=320,height=28,width=28)






