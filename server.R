function(input, output, session) {
# Scatter plot for all names 
  output$allScatter <- renderPlot({
    scat<-df_full%>%
      group_by(name,sex)%>%
      filter(rank[year==2020]<=150)%>%
      mutate(avg_rank=mean(rank[year<=2015],na.rm =TRUE))%>%
      filter(year==2020 & avg_rank<=150)%>%
      filter(count>=200)%>%
      mutate(badness=rank-avg_rank)%>%
      mutate(name=str_to_title(name))%>%
      ggplot(aes(avg_rank,rank,colour=badness))+
      geom_abline(intercept = 0,slope = 1,colour="39CFDE",lwd=1)+
      geom_label(aes(label=name),size=3)+
      annotate("text",x=110,y=-10,label="Rising stars",colour="#00294E",size=5)+
      annotate("text",x=20,y=150,label="Fading greats",colour="#DC4126",size=5)+
      annotate("text",x=30,y=-15,label="Always popular",colour="#FFDD00",size=5)+
      annotate("text",x=120,y=160,label="Consistent but unsual",colour="#FFDD00",size=5)+
      scale_y_reverse()+
      scale_x_reverse()+
      theme_minimal()+
      theme(panel.grid = element_blank(),
            legend.position = "none")+
      scale_colour_gradientn(colors =
                               c("#00294E","#1488CA","#39CFDE","#FFDD00",
                                 "#FE7D00","#DC4126"))+
      labs(y="Current rank >>>",x="Average rank >>>")
  })

# All Names since 1996 (count and rank)
  observe({
    all_names<-df_full%>%
      filter(sex==input$sexAllNames)%>%
      arrange(rank)%>%
      select(name,sex)%>%
      distinct()%>%pull(name)
    
    updateSelectizeInput(session, 'srDropDown', 
                         choices = all_names, server = TRUE,
                         options = list(placeholder="Select a name"))})
  
   output$allNamesRank <- renderPlot({
    if (is.null(input$srDropDown)){
        filtName="Charlie"
    }else{
      filtName=input$srDropDown}

  
  df_full%>%
      filter(!is.na(rank))%>%
      filter(sex==input$sexAllNames)%>%
      filter(name %in% filtName)%>%
      ggplot(aes(year,rank,colour=name))+
      geom_line()+
      geom_point()+
      geom_label_repel(data=.%>%group_by(name)%>%
                        filter(year==max(year)),
                        aes(label=name),alpha=0.8)+
      theme_minimal()+
      theme(legend.position = "none")+
      scale_y_reverse()+
      scale_x_continuous(limits = c(1995,2020))+
      labs(x="",y="Rank")
    })

    output$allNamesCount <- renderPlot({
      if (is.null(input$srDropDown)){
        filtName="Charlie"
      }else{
        filtName=input$srDropDown}
      
      df_full%>%
        filter(name %in% filtName & sex==input$sexAllNames)%>%
        ggplot(aes(year,count,fill=name))+
        geom_bar(stat="identity")+
        theme_minimal()+
        theme(legend.position = "none",
              panel.grid.minor = element_blank())+
        facet_wrap(.~name,nrow=1)+
        labs(x="",y="Number of babies born per year")+
        scale_x_continuous(limits = c(1995,2021))
    })
    
    
# All Names since 1996 (rank only) 
  observe({
    topHundNames<-df_lr%>%
      filter(sex==input$sexAllNamesLR)%>%
      select(name,sex)%>%
      distinct()%>%pull(name)
    
    updateSelectizeInput(session, 'lrDropDown', 
                         choices = sort(topHundNames), server = TRUE,
                         options = list(placeholder="Select a name"))})
    
    
  output$allNamesRankLR <- renderPlot({

    if (is.null(input$lrDropDown) & input$sexAllNamesLR=="boy"){
      filtNameLR="James"
    } else if (is.null(input$lrDropDown) & input$sexAllNamesLR=="girl"){
      filtNameLR="Elizabeth"
    } else {
      filtNameLR=input$lrDropDown;
    }
     
    df_lr%>%
        filter(name %in% filtNameLR & sex==input$sexAllNamesLR)%>%
        ggplot(aes(year,rank,colour=name))+
        geom_line()+
        geom_point()+
        geom_label_repel(data=.%>%group_by(name)%>%
                           filter(year==max(year)),
                         aes(label=name),alpha=0.8)+
        theme_minimal()+
        theme(legend.position = "none")+
        scale_y_reverse(limits=c(100,0))+
        scale_x_continuous(limits = c(1904,2020),breaks = seq(1900,2020,20))+
        labs(x="",y="Rank")
    })
    
# Ranking: Rising Stars  
  output$risingStars <- renderPlot({
    
    risStarts<-df_full%>%
      filter(sex==input$sexRisStar)%>%
      arrange(year)%>%
      group_by(name)%>%
      filter(mean(count[year%in%c(1996:2000)],na.rm=TRUE)>70)%>%
      filter(rank[year==2020]<=100)%>%
      mutate(rollrank=rollmean(rank,3,na.pad = TRUE,align = c("right")))%>%
      mutate(change=rollrank-lag(rollrank,15))%>%
      filter(year==2020)%>%
      arrange(change)%>%
      head(10)%>%pull(name)

    df_full%>%
      filter(sex==input$sexRisStar & name %in% risStarts)%>%
      mutate(name=fct_reorder(name,rank,min))%>%
      group_by(name)%>%
      ggplot(aes(year,rank,colour=name))+
      geom_line(lwd=0.8)+
      theme_minimal()+
      theme(legend.position = "left",
            axis.title = element_blank())+
      scale_y_reverse()+
      labs(subtitle="Rank",colour="Top 10 Rising Stars")+
      scale_colour_ordinal(direction=-1)
    
      })

# Rankings: Top 10 Ranks by sex and year

  output$allTopTen <- renderPlot({
    
    df_full%>%
      filter(year==input$yearTop10 & sex==input$sexTop10 & rank<=20)%>%
      mutate(name=fct_reorder(name,rank,.desc=TRUE))%>%
      ggplot(aes(count,name,fill=name))+
      geom_bar(stat="identity")+
      geom_text(aes(label=name,x=count*0.8),colour="white",size=5)+
      theme_minimal()+
        theme(legend.position = "none",
            axis.title = element_blank(),
            panel.grid = element_blank(),
            axis.text.y = element_blank())+
      scale_fill_ordinal(direction=-1)+
      labs(x="Number of babies born per year")
  })
  
  output$centurians <- renderPlot({

    df_lr%>%
      filter(name %in% centNames & sex==input$sexCentuSelect)%>%
      mutate(name=fct_reorder(name,rank,mean,.desc=FALSE))%>%
      ggplot(aes(year,rank,colour=name))+
      geom_text_repel(data=.%>%filter(year==2020),
                       aes(year+15,rank,colour=name,label=name),direction="y")+
      geom_line(alpha=0.6,lwd=0.8)+
      theme_minimal()+
      theme(panel.grid = element_blank(),
            legend.position = "none",
            axis.title = element_blank())+
      scale_colour_ordinal(direction=-1)+
      scale_y_reverse()+
      labs(subtitle="Rank")+
      scale_x_continuous(breaks = seq(1900,2020,20),limits = c(1900,2040))
  })
  
  
    output$genderNeutral <- renderPlot({

    gendNeut<-df_full%>%
      filter(count>100)%>%
      select(name,sex)%>%distinct()%>%
      group_by(name)%>%
      filter(n()>1)%>%
      arrange(name)%>%pull(name)
    
    df_full%>%
      filter(name %in% gendNeut & year==input$genNetYear)%>%
      arrange(name)%>%
      group_by(name)%>%
      mutate(boyrank=rank[sex=="boy"],girlrank=rank[sex=="girl"])%>%
      mutate(boyishness=boyrank-girlrank,tot_rank=(boyrank+girlrank)/2,
             tot_count=sum(count))%>%
      select(name,year,boyrank,girlrank,tot_rank,tot_count,boyishness)%>%
      distinct()%>%ungroup()%>%
      mutate(name=fct_rev(fct_reorder(name,tot_rank)))%>%
      ggplot(aes(x=boyrank,xend=girlrank,y=name,yend=name))+
      geom_segment(size=2,colour="grey",alpha=0.5)+
      geom_point(aes(x=boyrank),colour="blue4",size=3)+
      geom_point(aes(x=girlrank),colour="deeppink",size=3)+
      geom_segment(aes(x=3000,xend=3000,
                       y=12,yend=25),arrow=arrow(),colour="darkgrey")+
      annotate("text",x=3150,y=17,label="Average rank",angle=90,colour="darkgrey")+
      geom_text(data=.%>%filter(name=="Morgan"),aes(label="Boy",x=boyrank-170),
                colour="blue4")+
      geom_text(data=.%>%filter(name=="Morgan"),aes(label="Girl",x=girlrank+150),
                colour="deeppink")+
      geom_text(aes(x=tot_rank*2+500,label=name))+
      theme_minimal()+
      theme(panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank())+
      scale_x_reverse()+
      labs(x="Rank by gender")
    
})
  
# Consistency: strong and stable
output$consistentNames <- renderPlot({
      
  min=input$consSlide-input$consSlide/3
  max=input$consSlide+input$consSlide/3
      
  count<-df_small%>%
    filter(sex==input$consSex)%>%
    group_by(name)%>%
    filter(min(rank)>=min & max(rank)<=max)%>%
    select(name)%>%distinct()%>%
    ungroup()%>%summarise(n())%>%
    pull()
      
    while(count>7|count<3){
        print(paste("min",min,"max",max,"count",count))
        min=min+(count-sample(6:12,1))
        max=max-(count-sample(6:12,1))

        count<-df_small%>%
            filter(sex==input$consSex)%>%
            group_by(name)%>%
            filter(min(rank)>=min & max(rank)<=max)%>%
            select(name)%>%distinct()%>%
            ungroup()%>%summarise(n())%>%
            pull()
      }

    options(ggrepel.max.overlaps = Inf)

    df_small%>%
      filter(sex==input$consSex)%>%
      mutate(name=str_to_title(name))%>%
      ggplot(aes(year,count,colour=name))+
      geom_line()+
      gghighlight(min(rank)>=min & max(rank)<=max)+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(x="",y="Number of babies per year")  
})

# Celebrity Quirks: CIARA
output$celebCiara <- renderPlot({
  df_full%>%
    filter(name %in% c("Ciara","Keira"))%>%
    ggplot(aes(year,count,fill=name))+
    geom_col()+
    geom_segment(aes(x=2003.7,xend=2003.7,y=0,yend=2010),colour="red",size=1)+
    geom_point(aes(2003.7,2010),colour="red",show.legend = FALSE,size=4)+
    annotate("label",x=2008,y=300,label="Keira",colour="firebrick4",size=6)+
    annotate("text",x=2000,y=800,label="Ciara",colour="dodgerblue4",size=6)+
    annotate("text",x=2003,y=2300,
             label="Keira Knightley \nPirates of the Carribean",
             colour="red",size=5)+
    theme_minimal()+
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank())+
    labs(fill="")+
    scale_y_continuous(limits = c(0,2500))+
    scale_fill_manual(values=c("dodgerblue4","firebrick4"))
})

# Celebrity Quirks: JUSTIN
output$celebJustin <- renderPlot({

  df_full%>%
    filter(name %in% c("Justin"))%>%
    ggplot(aes(year,count))+
    geom_col(fill="dodgerblue4")+
    geom_segment(aes(x=2010,xend=2010,y=0,yend=300),colour="red")+
    geom_point(aes(2010,300),colour="red",show.legend = FALSE,size=4)+
    annotate("text",x=2017,y=150,label="Justin",colour="dodgerblue4",size=6)+
    annotate("text",x=2009,y=330,label="Bieber - My World",colour="red",size=6)+
    theme_minimal()+
    theme(legend.position = "none")+
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank())+
    labs(fill="")
})

# Celebrity Quirks: Britney
output$celebBritney <- renderPlot({
  
  df_full%>%
    filter(count>5)%>%
    filter(str_detect(name,"Brit"))%>%
    ggplot(aes(year,count,fill=name))+
    geom_col()+
    geom_segment(aes(x=1998,xend=1998,y=0,yend=1000),colour="red")+
    geom_point(aes(1998,1000),colour="red",show.legend = FALSE,size=4)+
    annotate("text",x=2004,y=1050,label="Call me Brittanie \n...one more time",
             colour="red",size=5)+
    theme_minimal()+
    theme(legend.position = "right")+
    theme(panel.grid = element_blank(),
          axis.title = element_blank())+
    labs(fill="")
})

# Celebrity Quirks: Cristiano
output$celebCristiano <- renderPlot({
  df_full%>%
    filter(name=="Cristiano")%>%
    ggplot(aes(year,count,fill=name))+
    geom_col(fill="dodgerblue4")+
    geom_segment(aes(x=2003,xend=2003,y=0,yend=35),colour="red")+
    geom_point(aes(2003,35),colour="red",show.legend = FALSE,size=4)+
    annotate("text",x=2003,y=40,label="Ronaldo joins Man U",
             colour="red",size=4)+
    annotate("text",x=2012,y=30,label="Cristiano",colour="dodgerblue4",size=5)+
    theme_minimal()+
    theme(legend.position = "right")+
    theme(panel.grid = element_blank(),
          axis.title = element_blank())+
    labs(fill="")
})


}

