j.agree2 <- function(ind,scot,k) {
  if(k==0) {k1=length(names(scot))-1} else {k1=k}
  j.names <- combn(names(scot),k1,paste0,collapse=",") %>% 
    data.frame() %>%
    `colnames<-`(c("x")) %>%
    filter(!grepl(ind,x)) %>%
    separate(x,sep=",",LETTERS[1:k1])
  xxx <- NULL
  for(i in 1:nrow(j.names)){
    if(k==1){
      xxx2 <- data.frame(A1=scot[,j.names[i,1]],B1=scot[,ind]) %>% 
        mutate(agree=ifelse(A1==B1,1,0),just1=ind) %>% drop_na() %>%
        summarize(tot_agree=sum(agree,na.rm=T),cases=n(),pct_agree=round(100*tot_agree/cases,1)) %>%
        mutate(justice=ind,A=j.names[i,1])
    } else if(k==0) {
      xxx2 <- data.frame(do.call(paste0,c(scot[,unlist(j.names[i,])])) %>% data.frame(),
                         scot[,ind] %>% data.frame() %>% unite("x",sep="")) %>%
        `colnames<-`(paste0(LETTERS[1:2],1)) %>%
        mutate(agree=ifelse(str_detect(as.character(A1),as.character(B1)),0,1)) %>%
        cbind(j.names[i,] %>% slice(rep(1:n(),each=nrow(scot)))) %>% drop_na() %>%
        summarize(tot_agree=sum(agree,na.rm=T),cases=n(),pct_agree=round(100*tot_agree/cases,1)) %>%
        mutate(justice=ind) %>% cbind(j.names[i,] %>% slice(rep(1:n(),each=1)))
    } else {
      xxx2 <- data.frame(do.call(paste0,c(scot[,unlist(j.names[i,])])) %>% data.frame(),
                         matrix(rep(scot[,ind],k),ncol=k) %>% data.frame() %>% unite("x",sep="")) %>%
        `colnames<-`(paste0(LETTERS[1:2],1)) %>%
        mutate(agree=ifelse(A1==B1,1,0),just1=ind) %>%
        cbind(j.names[i,] %>% slice(rep(1:n(),each=nrow(scot)))) %>% drop_na() %>%
        summarize(tot_agree=sum(agree,na.rm=T),cases=n(),pct_agree=round(100*tot_agree/cases,1)) %>%
        mutate(justice=ind) %>% cbind(j.names[i,] %>% slice(rep(1:n(),each=1)))
    }
    xxx <- rbind(xxx,xxx2)
  }
  return(xxx %>% arrange(pct_agree))
}