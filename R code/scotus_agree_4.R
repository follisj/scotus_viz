scotus_agree_4 <- function(ind,lib.just,data,color.palette) {
  scotus_year=2019
  party=ifelse("Kagan" %in% lib.just,"Liberal","Conservative")
  
  ## generate agreement rates for labels
  x.lab1 <- j.agree2(ind,data %>% select(ind,lib.just),1) %>%
    mutate(agree=paste0(A,"<br>",pct_agree,"%")) %>% arrange(A)
  x.lab2 <- j.agree2(ind,data %>% select(ind,lib.just),2) %>% 
    mutate(agree=paste0(A," & ",B,"\n",pct_agree,"%")) %>% arrange(A,B)
  x.lab3 <- j.agree2(ind,data %>% select(ind,lib.just),3) %>%
    mutate(agree=paste0(A,",\n",B," &\n",C,"\n",pct_agree,"%")) %>% arrange(A,B,C)
  x.lab4 <- j.agree2(ind,data %>% select(ind,lib.just),4) %>%
    mutate(agree=paste0("All\n",pct_agree,"%"))
  x.lab0 <- j.agree2(ind,data %>% select(ind,lib.just),0) %>%
    mutate(agree=paste0("None\n",pct_agree,"%"))
  
  ggplot() +
    #draw square, diagonals and inner square
    geom_polygon(data=data.frame(x=c(0,1,2,1),y=c(1,2,1,0)),
                 aes(x=x,y=y),fill=NA,col=color.palette[4],size=1)+
    geom_polygon(data=data.frame(x=c(0,1,2,1),y=c(1,1.4,1,.6)),
                 aes(x=x,y=y),fill=NA,col=color.palette[5],size=1)+
    geom_polygon(data=data.frame(x=c(.6,1,1.4,1),y=c(1,2,1,0)),
                 aes(x=x,y=y),fill=NA,col=color.palette[5],size=1)+
    geom_segment(aes(x=0,y=1,xend=2,yend=1),col=color.palette[4],size=1)+
    geom_segment(aes(x=1,y=2,xend=1,yend=0),col=color.palette[4],size=1)+
    
    #label 2 justice agreements
    geomtextpath::geom_labelpath(data=data.frame(x=c(0,1),y=c(1,2),labs=x.lab2$agree[1]),
                                 aes(x=x,y=y,label=labs,fill=x.lab2$pct_agree[1]),size=4,text_only=T)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(1,2),y=c(2,1),labs=x.lab2$agree[4]),
                                 aes(x=x,y=y,label=labs,fill=x.lab2$pct_agree[4]),size=4,text_only=T)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(2,1),y=c(1,0),labs=x.lab2$agree[6]),
                                 aes(x=x,y=y,label=labs,fill=x.lab2$pct_agree[6]),size=4,text_only=T)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(1,0),y=c(0,1),labs=x.lab2$agree[3]),
                                 aes(x=x,y=y,label=labs,fill=x.lab2$pct_agree[3]),size=4,text_only=T)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(.9,1.1),y=c(1.7,1.7)),
                                 aes(x=x,y=y,label=x.lab2$agree[5],fill=x.lab2$pct_agree[5]),
                                 text_only=T,col="black",size=4)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(.9,1.1),y=c(.3,.3)),
                                 aes(x=x,y=y,label=x.lab2$agree[5],fill=x.lab2$pct_agree[5]),
                                 text_only=T,col="black",size=4)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(.3,.3),y=c(.9,1.1)),
                                 aes(x=x,y=y,label=x.lab2$agree[2],fill=x.lab2$pct_agree[2]),
                                 text_only=T,col="black",size=4)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(1.7,1.7),y=c(.9,1.1)),
                                 aes(x=x,y=y,label=x.lab2$agree[2],fill=x.lab2$pct_agree[2]),
                                 text_only=T,col="black",size=4)+
    
    #label 3 justice agreements
    geomtextpath::geom_labelpath(data=data.frame(x=c(.5,.7),y=c(1,1)),
                                 aes(x=x,y=y,label=x.lab3$agree[2],fill=x.lab3$pct_agree[2]),
                                 text_only=T,size=4)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(.9,1.1),y=c(1.4,1.4)),
                                 aes(x=x,y=y,label=x.lab3$agree[1],fill=x.lab3$pct_agree[1]),
                                 text_only=T,size=4)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(1.3,1.5),y=c(1,1)),
                                 aes(x=x,y=y,label=x.lab3$agree[4],fill=x.lab3$pct_agree[4]),
                                 text_only=T,size=4)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(.9,1.1),y=c(.6,.6)),
                                 aes(x=x,y=y,label=x.lab3$agree[3],fill=x.lab3$pct_agree[3]),
                                 text_only=T,size=4)+
    #center and nodes
    geom_label(data=data.frame(x=1,y=1.1,label=x.lab4$agree,pct_agree=x.lab4$pct_agree),
               aes(x=x,y=y,label=label,fill=pct_agree),size=4.5)+
    geom_label(data=data.frame(x=1,y=.9,label=x.lab0$agree,pct_agree=x.lab0$pct_agree),
               aes(x=x,y=y,label=label,fill=pct_agree),size=4.5)+
    ggtext::geom_richtext(data=data.frame(x=c(0,1,2,1),y=c(1,2,1,0),labs=x.lab1$agree,pct_agree=x.lab1$pct_agree),
                          aes(x=x,y=y,label=labs,fill=pct_agree),
                          hjust=.5,
                          size=4.5)+
    labs(
      title=ind,
      subtitle=paste("Agreement with",party,"Justices,",scotus_year,"\n "),
      fill="Percent Agree"
    )+
    xlim(-.25,2.25)+
    ylim(-.1,2)+
    scale_fill_gradientn(colors=color.palette,limits=c(0,100))+
    theme_void()+
    theme(legend.position="top",
          legend.title=element_text(size=12),
          legend.key.height=unit(.5,"cm"),
          plot.title=element_text(hjust=.5,size=20,face="italic"),
          plot.subtitle=element_text(hjust=.5,size=15),
          panel.background = element_rect(fill="#ECECEC",
                                          color="#ECECEC"),
          plot.background = element_rect(color="#ECECEC",
                                         fill="#ECECEC"))
}