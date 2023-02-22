ballot_plot <- function(justice_p,data,cons,libs){
  j.list <- c(cons,libs)
  strip <-ggh4x::strip_themed(background_x = elem_list_rect(fill=c("red","blue")))
  data %>% 
    mutate(Justice=factor(as.factor(Justice),levels=j.list)) %>%
    mutate(justice_1=gsub("_\\(Supreme_Court)","",justice_1)) %>%
    mutate(justice_1=gsub("_"," ",justice_1)) %>%
    filter(justice_1==justice_p) %>%
    mutate(rate=as.numeric(gsub("%","",rate)),
           party=ifelse(Justice %in% cons.b,"Conservative","Liberal"),
           rate2=ifelse(party=="Liberal",rate,100-rate),
           rate3=ifelse(party=="Conservative",rate,100-rate)) %>%
    ggplot(aes(term,Justice,fill=party))+
    geom_tile(aes(alpha=ifelse(justice_1 %in% libs.b,rate/100,rate/100)),col="black")+
    geom_label(aes(label=rate),col="black",fill="white",size=5)+
    labs(title=justice_p)+
    scale_fill_manual(values=c("red", "blue"))+
    theme_bw()+
    facet_wrap2(~party,scales="free",strip=strip_themed(text_x=elem_list_text(color=c("red","blue"))))+
    theme(plot.title=element_text(hjust=.5,size=24),
          legend.position="none",
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          axis.text=element_text(size=12,face="bold"),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          strip.text.x=element_text(size=16,face="bold"),
          strip.background.x=element_rect(fill="white")
          #strip.background.x = element_rect(fill=c("red","blue"),color="black")
    )
}