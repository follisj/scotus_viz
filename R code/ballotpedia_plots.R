## code for Ballotpedia plots

## packages

library(tidyverse)
library(rvest)
library(patchwork)


## background data for managing scraped data and plot preparation

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


### create the just.agree dataset used for the plots by scraping data from Ballotpedia

just.agree <- NULL
for(i in 1:10) {
  url <- paste0("https://ballotpedia.org/",justices$justice[i])
  
  xxx <- url %>%
    read_html() %>%
    html_table(fill=T)
  
  if(justices$index[i]==1) {
    xxx <- xxx[[justices$table.no[i]]][c(-1,-2),c(1,seq(2,10,2))] %>%
      `colnames<-`(c("Justice","agree17","agree18",
                     "agree19","agree20","agree21")) %>%
      pivot_longer(-Justice,names_to='term',values_to='rate') %>%
      mutate(term=gsub("agree","20",term),justice_1=justices$justice[i])
  }
  
  if(justices$index[i]==2) {
    xxx <- xxx[[justices$table.no[i]]][c(-1,-2),c(1,seq(2,6,2))] %>%
      `colnames<-`(c("Justice","agree17","agree18",
                     "agree19")) %>%
      pivot_longer(-Justice,names_to='term',values_to='rate') %>%
      mutate(term=gsub("agree","20",term),justice_1=justices$justice[i])
  }
  
  if(justices$index[i]==3) {
    xxx <- xxx[[justices$table.no[i]]][c(-1,-2),c(1,seq(2,8,2))] %>%
      `colnames<-`(c("Justice","agree18",
                     "agree19","agree20","agree21")) %>%
      pivot_longer(-Justice,names_to='term',values_to='rate') %>%
      mutate(term=gsub("agree","20",term),justice_1=justices$justice[i])
  } 
  
  if(justices$index[i]==4) {
    xxx <- xxx[[justices$table.no[i]]][c(-1,-2),c(1,seq(2,4,2))] %>%
      `colnames<-`(c("Justice","agree20","agree21")) %>%
      pivot_longer(-Justice,names_to='term',values_to='rate') %>%
      mutate(term=gsub("agree","20",term),justice_1=justices$justice[i])
  } 
  just.agree <- rbind(just.agree,xxx)
}

just.agree <- just.agree %>%
  mutate(rate=gsub("N/A",NA,rate)) %>% 
  drop_na()


## generate plots for each justice - using ballot_plot.R function

for(i in 1:length(c(cons.b,libs.b))) {
  scj <- sc_just_1[i]
  assign(scj,ballot_plot(sc_just[i],just.agree,cons.b,libs.b))
}

## save plots

sc_just_plots <- list(Clarence_Thomas,Samuel_Alito,Amy_Coney_Barrett,
                      Neil_Gorsuch,Brett_Kavanaugh,John_Roberts,
                      Stephen_Breyer,Elena_Kagan,Ruth_Bader_Ginsburg,Sonia_Sotomayor)
sc_just_2 <- sc_just_1[-7]
for(i in 1:length(sc_just_plots)) {
  ggsave(paste0(sc_just_2[i],".png"),sc_just_plots[[i]],dpi=320,width=9,bg="white")
}


## patchwork plot of all justices (uses ballot_plot_1.R function)

(plot_spacer())/
  (plot_spacer()+
     ballot_plot_1("John Roberts",just.agree,c(cons.b,libs.b))+
     ballot_plot_1("Clarence Thomas",just.agree,c(cons.b,libs.b))+
     ballot_plot_1("Samuel Alito",just.agree,c(cons.b,libs.b))+
     plot_spacer()+
     plot_layout(width=c(.3,1,1,1,.7))
  )/
  (plot_spacer())/
  (ballot_plot_1("Ruth Bader Ginsburg",just.agree,c(cons.b,libs.b))+
     ballot_plot_1("Stephen Breyer",just.agree,c(cons.b,libs.b))+
     ballot_plot_1("Sonia Sotomayor",just.agree,c(cons.b,libs.b))+
     ballot_plot_1("Elena Kagan",just.agree,c(cons.b,libs.b))+
     plot_layout(width=c(1,1,1,1))
  )/
  (plot_spacer())/
  (plot_spacer()+
     ballot_plot_1("Neil Gorsuch",just.agree,c(cons.b,libs.b))+
     ballot_plot_1("Brett Kavanaugh",just.agree,c(cons.b,libs.b))+
     ballot_plot_1("Amy Coney Barrett",just.agree,c(cons.b,libs.b))+
     plot_spacer()+
     plot_layout(width=c(.3,1,1,1,.7))
  )+
  plot_annotation(title="SCOTUS Justice Agreement 2017-2021")+
  plot_layout(guides="collect",height=c(.05,1,.05,1,.05,1)) &
  theme(legend.position = "none",
        plot.title=element_text(hjust=.56,size=20))


## save the patchwork plot

ggsave("all_justice_ballotpedia.png",dpi=320,width=18,height=14)
