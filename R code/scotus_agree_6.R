scotus_agree_6 <- function(ind,cons,data,color.palette){
  ind=ind
  
  ## generate agreement rates for labels
  x.lab0 <- j.agree2(ind,data %>% select(ind,cons),0) %>%
    mutate(agree=paste0("None\n",pct_agree,"%"))
  x.lab1 <- j.agree2(ind,data %>% select(ind,cons),1) %>%
    mutate(agree=paste0(A," ",pct_agree,"%")) %>% arrange(pct_agree,A)
  x.lab2 <- j.agree2(ind,data %>% select(ind,cons),2) %>% 
    mutate(agree=paste0(A," & ",B,"\n",pct_agree,"%")) %>% arrange(pct_agree,A,B) %>%
    mutate(agree=j_abbr(agree))
  x.lab3 <- j.agree2(ind,data %>% select(ind,cons),3) %>%
    mutate(agree=paste0(A,", ",B," \n& ",C,"\n",pct_agree,"%")) %>% arrange(pct_agree,A,B,C) %>%
    mutate(agree=j_abbr(agree))
  x.lab4 <- j.agree2(ind,data %>% select(ind,cons),4) %>%
    mutate(agree=paste0(A,", ",B," \n",C," & ",D," \n",pct_agree,"%"))%>% arrange(pct_agree,A,B,C,D) %>%
    mutate(agree=j_abbr(agree))
  x.lab5 <- j.agree2(ind,data %>% select(ind,cons),5) %>%
    mutate(agree=paste0(A,", ",B," \n",C,", ",D, ", ",E," \n",pct_agree,"%")) %>%
    mutate(agree=j_abbr(agree))
  x.lab6 <- j.agree2(ind,data %>% select(ind,cons),6) %>%
    mutate(agree=paste0("All\n",pct_agree,"%"))
  
  ggplot(x.lab1,aes(x=1.2,y=rep(1/6,6)))+
    geom_col(aes(fill=pct_agree),col="white",width=2.4)+
    
    # single justice agreement
    geomtextpath::geom_textpath(aes(x=rep(2.25,6),y=seq(1/12,11/12,1/6),label=agree),angle=90,size=8,alpha=.75)+
    geom_rect(xmin=0,xmax=2.1,ymin=-Inf,ymax=Inf,fill="gray90")+
    
    # 2 justice agreement
    geom_col(data=x.lab2,aes(x=1.85,y=rep(1/15,15),fill=pct_agree),col="white",width=.3)+
    geomtextpath::geom_textpath(data=x.lab2,aes(x=rep(1.85,15),y=seq(1/30,29/30,1/15),label=agree),angle=90,size=4,alpha=.6)+
    
    # 3 justice agreement
    geom_col(data=x.lab3,aes(x=1.45,y=rep(.05,20),fill=pct_agree),col="white",width=.3)+
    geomtextpath::geom_textpath(data=x.lab3,aes(x=rep(1.45,20),y=seq(.025,.975,.05),label=agree),angle=90,size=4,alpha=.6)+
    
    # 4 justice agreement
    geom_col(data=x.lab4,aes(x=1.05,y=rep(1/15,15),fill=pct_agree),col="white",width=.3)+
    geomtextpath::geom_textpath(data=x.lab4,aes(x=rep(1.05,15),y=seq(1/30,29/30,1/15),label=agree),angle=90,size=4,alpha=.6)+
    
    # 5 justice agreement
    geom_col(data=x.lab5,aes(x=.65,y=rep(1/6,6),fill=pct_agree),col="white",width=.3)+
    geomtextpath::geom_textpath(data=x.lab5,aes(x=rep(.65,6),y=seq(1/12,11/12,1/6),label=agree),angle=90,size=3)+
    
    # 6 justice agreement
    geom_col(data=data.frame(x=c(.2,.2),y=c(.5,.5),
                             agree=c(x.lab6$agree,x.lab0$agree),
                             pct_agree=c(x.lab6$pct_agree,x.lab0$pct_agree)),
             aes(x=x,y=y,fill=pct_agree),width=.4,col="white")+
    geom_text(data=data.frame(x=c(.2,.2),y=c(.25,.75),
                                                agree=c(x.lab6$agree,x.lab0$agree),
                                                pct_agree=c(x.lab6$pct_agree,x.lab0$pct_agree)),
                                aes(x=x,y=y,label=agree),size=4.5)+
    
    geomtextpath::geom_textpath(data=data.frame(x=c(2.25,2.25),y=c(.12,.88),label=c("Lowest  Agreement","Highest  Agreement")),
                                aes(x=x,y=y,label=label),vjust=4,size=8,angle=90,alpha=.75)+
    geom_line(data=x.black.bar,aes(x=x,y=y,group=grp),size=2)+
    
    scale_fill_gradientn(colors=color.palette,limits=c(0,100))+
    labs(fill="Percent Agree",
         title=ind,
         subtitle=paste("Agreement with Conserative Justices,",scotus_year))+
    theme_void()+
    theme(legend.position = c(.45,.95),
          legend.direction="horizontal",
          legend.key.size = unit(1,'cm'),
          legend.title=element_text(size=12),
          plot.title=element_text(hjust=.5,size=40),
          plot.subtitle=element_text(hjust=.5,size=28),
          panel.background = element_rect(fill="gray90",
                                          color="gray90"),
          plot.background = element_rect(color="gray90",
                                         fill="gray90"))+
    coord_polar(theta="y")
}