
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output,session) {

  # this could be the way to dyanmically bound N1 and N2 inputs but doesn't seem to be working
  #  output$N1in <- renderUI({
  #     numericInput("N1", "testing", min=0, max=input$N-input$N2,value=10000)
  #   })
  #
  
  ## main plot
  output$my.plot <- renderPlot({
     
    ## proportion protected
    prop.prot <- (input$N1*input$VE1 + input$N2*input$VE2)/input$N
    prop.need.prot <- c(1-1/input$R.opt,1-1/input$R.mod,1-1/input$R.pes)
    ## 
    sd.prop.need.prot <- prop.need.prot*input$pct.uncer
    #sd.prop.need.prot <- sqrt(prop.need.prot*(1-prop.need.prot)/100)
    layout(matrix(c(1,1,1,1,2),nrow=1))
    par(oma=c(10,3,0,0))
    plot(-100,-100,xlim=c(0,1),ylim=c(-.5,3.25),axes=F,xlab="proportion protected",ylab="scenarios")
    axis(1)
    
    red.col = rgb(red=1,green=0,blue=0,alpha=.4)
    yellow.col = "yellow"#rgb(red=255,green=255,blue=204,maxColorValue=255,alpha=.4)
    green.col = rgb(red=0,green=1,blue=0,alpha=.4)
    
    ## bounds for opt scen
    upper1.opt <- max(prop.need.prot[1],0)
    lower1.opt<- 0

    upper2.opt <- min(prop.need.prot[1]+sd.prop.need.prot[1],1)
    lower2.opt<- upper1.opt
    
    upper3.opt <- 1
    lower3.opt<- upper2.opt
    
    polygon(x=c(lower1.opt,upper1.opt,upper1.opt,lower1.opt),
            y=c(-.5,-.5,0.5,0.5),col=red.col,border=FALSE)
    polygon(x=c(lower2.opt,upper2.opt,upper2.opt,lower2.opt),
            y=c(-.5,-.5,0.5,0.5),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.opt,upper3.opt,upper3.opt,lower3.opt),
            y=c(-.5,-.5,0.5,0.5),col=green.col,border=FALSE)
    
    upper1.mod <- max(prop.need.prot[2],0)
    lower1.mod<- 0
    
    upper2.mod <- min(prop.need.prot[2]+sd.prop.need.prot[2],1)
    lower2.mod<- upper1.mod
    
    upper3.mod <- 1
    lower3.mod<- upper2.mod
    
    polygon(x=c(lower1.mod,upper1.mod,upper1.mod,lower1.mod),
            y=c(.75,.75,1.75,1.75),col=red.col,border=FALSE)
    polygon(x=c(lower2.mod,upper2.mod,upper2.mod,lower2.mod),
            y=c(.75,.75,1.75,1.75),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.mod,upper3.mod,upper3.mod,lower3.mod),
            y=c(.75,.75,1.75,1.75),col=green.col,border=FALSE)
    
    upper1.pes <- max(prop.need.prot[3],0)
    lower1.pes<- 0
    
    upper2.pes <- min(prop.need.prot[3]+sd.prop.need.prot[3],1)
    lower2.pes<- upper1.pes
    
    upper3.pes <- 1
    lower3.pes<- upper2.pes
    
    polygon(x=c(lower1.pes,upper1.pes,upper1.pes,lower1.pes),
            y=c(2,2,3,3),col=red.col,border=FALSE)
    polygon(x=c(lower2.pes,upper2.pes,upper2.pes,lower2.pes),
            y=c(2,2,3,3),col=yellow.col,border=FALSE)
    polygon(x=c(lower3.pes,upper3.pes,upper3.pes,lower3.pes),
            y=c(2,2,3,3),col=green.col,border=FALSE)
  
    mtext(text="estimated \n proportion \n protected",at=prop.prot,side=3)
    mtext(text="optimisitic",side=2,at=0,cex=.9)
    mtext(text="moderate",side=2,at=1.25,cex=.9)
    mtext(text="pessimistic",side=2,at=2.5,cex=.9)
    abline(v=prop.prot,lty=2,lwd=2,col=1)        
    
    ## plot stop signs
    sign.opt <- ifelse(prop.prot < prop.need.prot[1], "red",ifelse(prop.prot <  prop.need.prot[1]+sd.prop.need.prot,"yellow","green"))
    sign.mod <- ifelse(prop.prot < prop.need.prot[2], "red",ifelse(prop.prot<  prop.need.prot[2]+sd.prop.need.prot,"yellow","green"))
    sign.pes <- ifelse(prop.prot < prop.need.prot[3], "red",ifelse(prop.prot <  prop.need.prot[3]+sd.prop.need.prot,"yellow","green"))
    
    plot(-100,-100,xlim=c(0,1),ylim=c(-.5,3.25),axes=F,xlab="",ylab="")
    points(c(.4,.4,.4),c(0,1.25,2.5),pch=19,col=c(sign.opt,sign.mod,sign.pes),cex=10)
    mtext(text="Summary",at=.4,side=3,cex=1.2)
    
    par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    
    
    par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    
    legend(-.7,-.5,c("spread highly likely","spread possible","spread unlikley"),col=c(red.col,yellow.col,green.col),lty=1,
           lwd=4,bty="n",cex=2)
    
    legend(0,-.5,c("vaccine urgently required","vaccine required","vaccination reccomended but not priority"),col=c("red","yellow","green"),lty=0,
           lwd=4,pch=19,bty="n",cex=2)
           
    
  })
  
  ## this is the main plot that shows the 3 divisions of people and cum. cases averted by vaccine
  output$ind.dir.plot <- renderPlot({
    
    ## will work with raw numbers first then can normailize if we want
    ## number directly protected  
    dir.prot <- (input$N1*input$VE1 + input$N2*input$VE2)
    
    ## get the indirect protection by solving final size equation for each R
    ## with and without vaccination 
    
    ## final size obf function
    final.size.form <- function(Z.inf,R0,prot,N) {
      lhs <- Z.inf
      rhs <- (N-prot)*(1-exp(-R0*Z.inf/N))
      return(abs(lhs-rhs))
    }
    
    ## optimisitic
    fs.opt.novac <- nlminb(start=input$N/3,
                     objective=final.size.form,
                     lower=1,
                     upper=input$N-1,
                     prot=0,
                     R0=input$R.opt,
                     N=input$N)$par
   
    ## just to make sure another starting value doesn't get us where we need to go
    ## should be a while loop but this seems to break things online
    n.tries <- 1
    while(fs.opt.novac == 1 & input$R.opt > 1 & n.tries < 5000){
      fs.opt.novac <- nlminb(start=sample(input$N,1),
                             objective=final.size.form,
                             lower=1,
                             upper=input$N-1,
                             prot=0,
                             R0=input$R.opt,
                             N=input$N)$par
      n.tries <- n.tries + 1
    }
    
    fs.opt.vac <- nlminb(start=input$N/3,
                         objective=final.size.form,
                         lower=1,
                         upper=input$N-1,
                         prot=dir.prot,
                         R0=input$R.opt,
                         N=input$N)$par
    n.tries <- 1    
    while(fs.opt.vac == 1 & input$R.opt*(1-dir.prot/input$N)>1 & n.tries < 5000){
      fs.opt.vac <- nlminb(start=sample(input$N,1),
                           objective=final.size.form,
                           lower=1,
                           upper=input$N-1,
                           prot=dir.prot,
                           R0=input$R.opt,
                           N=input$N)$par
      n.tries <- n.tries + 1      
    }
    
    ## in cases where there is no outbreak once vaccinated, the remainder of people are considered in
    ## directly protected
    #if (fs.opt.vac == 1){
    #  indir.prot.opt <- input$N-dir.prot
    #} else {
      indir.prot.opt <- max(fs.opt.novac - fs.opt.vac - dir.prot,0)      
    #}
    
    #moderate
    fs.mod.novac <- nlminb(start=input$N/3,
                           objective=final.size.form,
                           lower=1,
                           upper=input$N-1,
                           prot=0,
                           R0=input$R.mod,
                           N=input$N)$par
    
    
    n.tries <- 1
    while(fs.mod.novac == 1 & input$R.mod > 1 & n.tries < 5000){    
      fs.mod.novac <- nlminb(start=sample(input$N,1),
                             objective=final.size.form,
                             lower=1,
                             upper=input$N-1,
                             prot=0,
                             R0=input$R.mod,
                             N=input$N)$par
      n.tries <- n.tries + 1
    }
         
    fs.mod.vac <- nlminb(start=input$N/3,
                         objective=final.size.form,
                         lower=1,
                         upper=input$N-1,
                         prot=dir.prot,
                         R0=input$R.mod,
                         N=input$N)$par
    
    
    n.tries <- 1    
    while(fs.mod.vac == 1 & input$R.mod*(1-dir.prot/input$N)>1 & n.tries < 5000){      
      fs.mod.vac <- nlminb(start=sample(input$N,1),
                           objective=final.size.form,
                           lower=1,
                           upper=input$N-1,
                           prot=dir.prot,
                           R0=input$R.mod,
                           N=input$N)$par
      n.tries <- n.tries + 1
    }
      
    
    #if (fs.mod.vac == 1){
    #  indir.prot.mod <- input$N-dir.prot
    #} else {
      indir.prot.mod <- max(fs.mod.novac - fs.mod.vac - dir.prot,0)      
    #}
    
    #pessimistic
    fs.pes.novac <- nlminb(start=input$N/3,
                           objective=final.size.form,
                           lower=1,
                           upper=input$N-1,
                           prot=0,
                           R0=input$R.pes,
                           N=input$N)$par
    
      n.tries <- 1
      while(fs.pes.novac == 1 & input$R.pes > 1 & n.tries < 5000){    
      fs.pes.novac <- nlminb(start=sample(input$N,1),
                             objective=final.size.form,
                             lower=1,
                             upper=input$N-1,
                             prot=0,
                             R0=input$R.pes,
                             N=input$N)$par
    }
    
    fs.pes.vac <- nlminb(start=input$N/3,
                         objective=final.size.form,
                         lower=1,
                         upper=input$N-1,
                         prot=dir.prot,
                         R0=input$R.pes,
                         N=input$N)$par
    
    n.tries <- 1    
    while(fs.pes.vac == 1 & input$R.pes*(1-dir.prot/input$N)>1 & n.tries < 5000){      
      fs.pes.vac <- nlminb(start=sample(input$N,1),
                           objective=final.size.form,
                           lower=1,
                           upper=input$N-1,
                           prot=dir.prot,
                           R0=input$R.pes,
                           N=input$N)$par
      n.tries <- n.tries + 1
    }
    
    #if (fs.pes.vac == 1){
    #  indir.prot.pes <- input$N-dir.prot
    #} else {
      indir.prot.pes <- max(fs.pes.novac - fs.pes.vac - dir.prot,0)      
    #}
      
    layout(matrix(c(1,1,1,2),nrow=1))
    par(oma=c(0,0,3,0),mar=c(5.1,5.5,4.1,2.1))
    plot(-100,-100,xlim=c(0,1),ylim=c(-.5,3.25),axes=F,xlab="proportion of population",ylab="")
    axis(1)
    abline(v=seq(0,1,by=.1),lty=3,col="grey")
    
    
    susceptible.col = rgb(202,178,214,alpha=200,maxColorValue=255)
    infected.col = rgb(227,26,28,alpha=200,maxColorValue=255)
    direct.prot.col = rgb(51,160,44,alpha=200,maxColorValue=255)
    cases.prev.col = rgb(186,186,186,alpha=200,maxColorValue=255)
    
    red.col = rgb(red=1,green=0,blue=0,alpha=.4)  
    yellow.col = "yellow"#rgb(red=255,green=255,blue=204,maxColorValue=255,alpha=.4)
    light.green.col = rgb(229,245,224,alpha=200,maxColorValue=255)
    green.col = rgb(49,163,84,alpha=200,maxColorValue=255)
    grey.col = rgb(224,224,224,alpha=200,maxColorValue=255)
    light.blue.col = rgb(158,202,225,alpha=200,maxColorValue=255)
    ## bounds for opt scen
    lower1.opt<- 0
    upper1.opt <- dir.prot/input$N

    lower2.opt<- upper1.opt
    upper2.opt <- (dir.prot+indir.prot.opt)/input$N#min(prop.need.prot[1]+sd.prop.need.prot[1],1)

    lower3.opt<- upper2.opt
    upper3.opt <- (dir.prot+indir.prot.opt+(input$N-fs.opt.vac-dir.prot-indir.prot.opt))/input$N
    
    lower4.opt<- upper3.opt
    upper4.opt <- 1
    
    polygon(x=c(lower1.opt,upper1.opt,upper1.opt,lower1.opt),
            y=c(0,0,0.5,0.5),col=direct.prot.col,border=FALSE)
    polygon(x=c(lower2.opt,upper3.opt,upper3.opt,lower2.opt),
            y=c(0,0,0.5,0.5),col=susceptible.col,border=FALSE)
#   polygon(x=c(lower3.opt,upper3.opt,upper3.opt,lower3.opt),
#            y=c(0,-0,0.5,0.5),col=light.red.col,border=FALSE)
    polygon(x=c(lower4.opt,upper4.opt,upper4.opt,lower4.opt),
            y=c(0,0,0.5,0.5),col=infected.col,border=FALSE)
    
    # now cumulative cases prevented by vaccination
    cases.prev.opt <- max(fs.opt.novac - fs.opt.vac,0)/input$N
    polygon(x=c(lower1.opt,cases.prev.opt,cases.prev.opt,lower1.opt),
          y=c(-.5,-.5,-.025,-.025),col=cases.prev.col,border=FALSE)


    ## moderate
    lower1.mod<- 0
    upper1.mod <- dir.prot/input$N
    
    lower2.mod<- upper1.mod
    upper2.mod <- (dir.prot+indir.prot.mod)/input$N#min(prop.need.prot[1]+sd.prop.need.prot[1],1)
    
    lower3.mod<- upper2.mod
    upper3.mod <- (dir.prot+indir.prot.mod+(input$N-fs.mod.vac-dir.prot-indir.prot.mod))/input$N
    
    lower4.mod<- upper3.mod
    upper4.mod <- 1
    
    polygon(x=c(lower1.mod,upper1.mod,upper1.mod,lower1.mod),
            y=c(1.25,1.25,1.75,1.75),col=direct.prot.col,border=FALSE)
    polygon(x=c(lower2.mod,upper3.mod,upper3.mod,lower2.mod),
            y=c(1.25,1.25,1.75,1.75),col=susceptible.col,border=FALSE)
   # polygon(x=c(lower3.mod,upper3.mod,upper3.mod,lower3.mod),
  #          y=c(.75,.75,1.75,1.75),col=light.red.col,border=FALSE)
    polygon(x=c(lower4.mod,upper4.mod,upper4.mod,lower4.mod),
            y=c(1.25,1.25,1.75,1.75),col=infected.col,border=FALSE)
    
    ## cumulative cases prevented
    cases.prev.mod <- max(fs.mod.novac - fs.mod.vac,0)/input$N
    polygon(x=c(lower1.mod,cases.prev.mod,cases.prev.mod,lower1.mod),
        y=c(.75,.75,1.225,1.225),col=cases.prev.col,border=FALSE)
    
    ## pessimistic
    lower1.pes<- 0
    upper1.pes <- dir.prot/input$N
    
    lower2.pes<- upper1.pes
    upper2.pes <- (dir.prot+indir.prot.pes)/input$N
    
    lower3.pes<- upper2.pes
    upper3.pes <- (dir.prot+indir.prot.pes+(input$N-fs.pes.vac-dir.prot-indir.prot.pes))/input$N
    
    lower4.pes<- upper3.pes
    upper4.pes <- 1
    
    polygon(x=c(lower1.pes,upper1.pes,upper1.pes,lower1.pes),
            y=c(2.5,2.5,3,3),col=direct.prot.col,border=FALSE)
    polygon(x=c(lower2.pes,upper3.pes,upper3.pes,lower2.pes),
            y=c(2.5,2.5,3,3),col=susceptible.col,border=FALSE)
    #polygon(x=c(lower3.pes,upper3.pes,upper3.pes,lower3.pes),
    #        y=c(2,2,3,3),col=light.red.col,border=FALSE)
    polygon(x=c(lower4.pes,upper4.pes,upper4.pes,lower4.pes),
            y=c(2.5,2.5,3,3),col=infected.col,border=FALSE)
    
    cases.prev.pes <- max(fs.pes.novac - fs.pes.vac,0)/input$N
    polygon(x=c(lower1.pes,cases.prev.pes,cases.prev.pes,lower1.pes),
        y=c(2,2,2.475,2.475),col=cases.prev.col,border=FALSE)
  #  mtext(text="estimated \n proportion \n protected",at=prop.prot,side=3)
    
    mtext(text="optimisitic",side=2,at=0,cex=1.1,line=3.2)
    mtext(text="cases \n prevented",side=2,at=-.25,cex=.8,line=-2,las=1)
    mtext(text="",side=2,at=.25,cex=.8,line=-2,las=1)
    

    mtext(text="moderate",side=2,at=1.25,cex=1.1,line=3.2)
    mtext(text="cases \n prevented",side=2,at=1,cex=.8,line=-2,las=1)
    mtext(text="",side=2,at=1.5,cex=.8,line=-2,las=1)
    
    mtext(text="pessimistic",side=2,at=2.5,cex=1.1,line=3.2)
    mtext(text="cases \n prevented",side=2,at=2.25,cex=.8,line=-2,las=1)
    mtext(text="",side=2,at=2.75,cex=.8,line=-2,las=1)
    #abline(v=prop.prot,lty=2,lwd=2,col=1)    

    ## stop signs and goal posts
    prop.prot <- (input$N1*input$VE1 + input$N2*input$VE2)/input$N
    prop.need.prot <- c(1-1/input$R.opt,1-1/input$R.mod,1-1/input$R.pes)
    ## 
    sd.prop.need.prot <- prop.need.prot*input$pct.uncer
    sign.opt <- ifelse(prop.prot < prop.need.prot[1], "red",ifelse(prop.prot <  prop.need.prot[1]+sd.prop.need.prot[1],"yellow","green"))
    sign.mod <- ifelse(prop.prot < prop.need.prot[2], "red",ifelse(prop.prot<  prop.need.prot[2]+sd.prop.need.prot[2],"yellow","green"))
    sign.pes <- ifelse(prop.prot < prop.need.prot[3], "red",ifelse(prop.prot <  prop.need.prot[3]+sd.prop.need.prot[3],"yellow","green"))
  
    ## goal posts
    arrows(x0=0,x1=1,y0=.6,angle=90,code=3,length=.02)
    arrows(x0=0,x1=1,y0=1.85,angle=90,code=3,length=.02)
    arrows(x0=0,x1=1,y0=3.1,angle=90,code=3,length=.02)
    points(rep(0,3),c(.6,1.85,3.1),pch=20,col="red",cex=1.8)
    points(prop.need.prot,c(.6,1.85,3.1),pch=20,col="yellow",cex=1.8)
    points(prop.need.prot+sd.prop.need.prot,c(.6,1.85,3.1),pch=20,col="green",cex=1.8)
    points(rep(prop.prot,3),c(.6,1.85,3.1),pch=4,col="black",cex=1.8)

    plot(-100,-100,xlim=c(0,1),ylim=c(-.5,3.25),axes=F,xlab="",ylab="")
    points(c(.4,.4,.4),c(0,1.25,2.5),pch=19,col=c(sign.opt,sign.mod,sign.pes),cex=15)
    par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    
    legend("topleft",c("directly protected","uninfected (susceptible)","infected"),col=c(direct.prot.col,susceptible.col,infected.col),lty=1,
           lwd=4,bty="n",cex=2)
    
    mtext("Level of Population Protection",side=3,outer=TRUE)
    legend("topright",c("tranmission highly likley","transmission possible","transmission unlikley"),col=c("red","yellow","green"),lty=0,
           lwd=4,pch=19,bty="n",cex=2)
  })
  
  output$table = renderTable({
    prop.prot <- (input$N1*input$VE1 + input$N2*input$VE2)/input$N
    prop.need.prot <- c(1-1/input$R.opt,1-1/input$R.mod,1-1/input$R.pes)
    ## 
    #sd.prop.need.prot <- prop.need.prot*input$pct.uncer
    rc <- as.data.frame(rev(prop.need.prot))
    rownames(rc) <- c("pessimistic","moderate","optimistic")
    colnames(rc) <- c("proportion needed to be protected")
    rc
    })
    
  
})
