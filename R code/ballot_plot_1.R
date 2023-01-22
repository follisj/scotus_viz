ballot_plot_1 <- function(justice_p,data,j.list){
  data %>% 
    mutate(Justice=factor(as.factor(Justice),levels=j.list)) %>%
    mutate(justice_1=gsub("_\\(Supreme_Court)","",justice_1)) %>%
    mutate(justice_1=gsub("_"," ",justice_1)) %>%
    filter(justice_1==justice_p) %>%
    mutate(rate=as.numeric(gsub("%","",rate)),
           party=ifelse(Justice %in% cons,"Conservative","Liberal"),
           term=str_replace(term,'20',"'")) %>%
    ggplot(aes(term,Justice,fill=rate))+
    geom_tile(col="black")+
    geom_label(aes(label=rate),fill="white",size=3)+
    labs(subtitle=justice_p,
         fill="Percent Agree")+
    #viridis::scale_fill_viridis(limits=c(50,100),name="")+
    scale_fill_gradientn(colors=c.pal2,limits=c(0,100))+
    theme_bw()+
    theme(plot.subtitle=element_text(hjust=.5,size=16),
          legend.position="top",
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          axis.text=element_text(size=12,face="bold"),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}
