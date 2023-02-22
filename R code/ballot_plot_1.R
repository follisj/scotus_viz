ballot_plot_1 <- function(justice_p,data,j.list){
  data %>% 
    mutate(Justice=factor(as.factor(Justice),levels=j.list)) %>%
    mutate(justice_1=gsub("_\\(Supreme_Court)","",justice_1)) %>%
    mutate(justice_1=gsub("_"," ",justice_1)) %>%
    filter(justice_1==justice_p) %>%
    mutate(rate=as.numeric(gsub("%","",rate)),
           party=ifelse(Justice %in% cons.b,"Conservative","Liberal"),
           term=str_replace(term,'20',"'"),
           rate2=ifelse(party=="Liberal",rate,100-rate),
           rate3=ifelse(party=="Conservative",rate,100-rate)) %>%
    ggplot(aes(term,Justice,fill=party))+
    geom_tile(aes(alpha=ifelse(justice_1 %in% libs.b,rate/100,rate/100)),col="black")+
    geom_label(aes(label=rate,),col="black",fill="white",size=4)+
    labs(title=justice_p)+
    scale_fill_manual(values=c("red", "blue"))+
    theme_bw()+
    theme(plot.subtitle=element_text(hjust=.5,size=16),
          legend.position="none",
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          axis.text=element_text(size=12,face="bold"),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}