ballot_plot <- function(justice_p,data,cons,libs){
  j.list <- c(cons,rev(libs))
  data %>% 
    mutate(Justice=factor(as.factor(Justice),levels=j.list)) %>%
    mutate(justice_1=gsub("_\\(Supreme_Court)","",justice_1)) %>%
    mutate(justice_1=gsub("_"," ",justice_1)) %>%
    filter(justice_1==justice_p) %>%
    mutate(rate=as.numeric(gsub("%","",rate)),
           party=ifelse(Justice %in% cons,"Conservative","Liberal")) %>%
    ggplot(aes(term,Justice,fill=rate))+
    geom_tile(col="black")+
    geom_label(aes(label=rate),fill="white",size=5)+
    labs(title=justice_p,
         fill="Percent Agree")+
    #viridis::scale_fill_viridis(limits=c(50,100),name="")+
    scale_fill_gradientn(colors=c.pal2,limits=c(0,100))+
    theme_bw()+
    facet_wrap(~party,scales="free")+
    theme(plot.title=element_text(hjust=.5,size=24),
          legend.position="top",
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          axis.text=element_text(size=12,face="bold"),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          strip.text.x=element_text(size=14,face="bold"),
          strip.background.x = element_rect(fill="white",color="black"))
}

