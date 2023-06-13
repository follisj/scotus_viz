scotus_agree_3 <- function(ind,lib.just,data,color.palette) {
  ## data to create triangles
  ht=sqrt(3)/2
  ht2=sqrt(3)/6
  x.tri2.2 <- data.frame(
    id=c(1,2,3),
    x1=c(1,1.5,1.5),
    y1=c(1,1+ht2,1+ht),
    x2=c(1.5,1.5,2),
    y2=c(1+ht,1+ht2,1),
    x3=c(1,1.5,2),
    y3=c(1,1+ht2,1)
  )
  
  ## generate agreement rates for labels
  x.labx0 <- j.agree2(ind,data %>% select(ind,lib.just),0) %>%
    mutate(agree=paste0("None\n",pct_agree,"%"))
  x.labx1 <- j.agree2(ind,data %>% select(ind,lib.just),1) %>%
    arrange(A)
  x.ag1.lab1 <- data.frame(
    x = c(1,1.5,2),
    y = c(1,1+ht,1),
    labs = c(
      paste0(x.labx1$A,"\n",x.labx1$pct_agree,"%")
    ),
    f_col=x.labx1$pct_agree
  )
  
  x.labx2 <- j.agree2(ind,data %>% select(ind,lib.just),2) %>% 
    mutate(agree=paste0(A," & ",B,"\n",pct_agree,"%")) %>%
    arrange(A,B)
  
  
  x.labx3 <- j.agree2(ind,data %>% select(ind,lib.just),3) %>%
    mutate(agree=paste0("All","\n",pct_agree,"%"))
  
  ## generate plot
  ggplot(data=x.tri2.2) +
    #triangles using 3 smaller triangles
    geom_polygon(aes(x=x1,y=y1),fill="gray90",col="black")+
    geom_polygon(aes(x=x2,y=y2),fill="gray90",col="black")+ 
    geom_polygon(aes(x=x3,y=y3),fill="gray90",col="black")+

    geom_polygon(data=data.frame(x=c(4/3,1.5,5/3),
                                 y=c(y=c(1+1/(3*sqrt(3)),
                                         1+1/(3*sqrt(3))+(sqrt(3)/6),
                                         1+1/(3*sqrt(3))))),
                 aes(x=x,y=y,fill=x.labx0$pct_agree),
                 col="black")+
    geom_polygon(data=data.frame(x=c(7/5,3/2,8/5),
                                 y=c(1+1/(3*sqrt(3))+(sqrt(3)/15),
                                     1+1/(3*sqrt(3))+(sqrt(3)/6),
                                     1+1/(3*sqrt(3))+(sqrt(3)/15))),
                 aes(x=x,y=y,fill=x.labx3$pct_agree),
                 col="black")+
    
    ## labels
    ggtext::geom_richtext(data=x.ag1.lab1 %>% mutate(labs=gsub("\\n","<br>",x.ag1.lab1$labs)),
                          aes(x=x,y=y,label=labs,fill=f_col),
                          angle=c(30,0,330),size=5)+
    
    geomtextpath::geom_labelpath(data=data.frame(x=c(1+1/6,1.5),y=c(1+1/(6*sqrt(3)),1+ht-1/6),
                                                 labs=x.labx2$agree[1],
                                                 f_col=x.labx2$pct_agree[1]),
                                 aes(x=x,y=y,label=labs,fill=f_col),
                                 size=5,text_only = T)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(1+1/6,2-1/6),y=c(1+1/(6*sqrt(3)),1+1/(6*sqrt(3))),
                                                 labs=x.labx2$agree[2],
                                                 f_col=x.labx2$pct_agree[2]),
                                 aes(x=x,y=y,label=labs,
                                     fill=f_col),
                                 size=5,text_only=T)+
    geomtextpath::geom_labelpath(data=data.frame(x=c(1.5,2-1/6),y=c(1+ht-1/6,1+1/(6*sqrt(3))),
                                                 labs=x.labx2$agree[3],
                                                 f_col=x.labx2$pct_agree[3]),
                                 aes(x=x,y=y,label=labs,
                                     fill=f_col),
                                 col="black",size=5,text_only = T)+
    geom_text(data=data.frame(x=1.5,y=.965+.5/sqrt(3),labs=x.labx0$agree),
              aes(x=x,y=y,label=labs),size=5)+
    geom_text(data=data.frame(x=1.5,y=1.075+.5/sqrt(3),labs=x.labx3$agree),
              aes(x=x,y=y,label=labs),size=5)+
    
    labs(
      title=ind,
      subtitle="Agreement with Liberal Justices, 2021\n",
      fill="Percent Agree"
    )+
    xlim(.85,2.15)+
    ylim(0.85,1.9)+

    scale_fill_gradientn(colors=color.palette,limits=c(0,100))+
    theme_void()+
    theme(#legend.position = "none",
      legend.position="top",
      legend.key.height=unit(.5,"cm"),
          plot.title=element_text(hjust=.5,size=20,face="italic"),
      plot.subtitle=element_text(hjust=.5,size=14),
      panel.background = element_rect(fill="#ECECEC",
                                      color="#ECECEC"),
      plot.background = element_rect(color="#ECECEC",
                                     fill="#ECECEC"))
  
}
