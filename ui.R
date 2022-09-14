rm(list=ls())
source("libraries.R")
source("wrangle.R")

# Application title
navbarPage(title = "Baby Names",collapsible = TRUE,
  navbarMenu(title="About",
      tabPanel(title="Site",align="center",
                    h4("Data driven baby name insights",),
                    h5("Names are obviously a personal choice. But identifying
                       trends lets us classify names: fashionable-but-unsual (rising stars),
                       or traditional-but-unfashionable (fading greats). I'm not saying 
                       they're right or wrong, but it can't hurt to know. Especially if you're 
                       about to name your kid Arthur or Matthew."),
                    plotOutput("allScatter")),
      tabPanel(title="Author",
               h3("Justin Beresford"),
              h5("Data scientist (third), economist (second), 
                  all round cool guy (first)"),
              h5("One of about five thousand Justins born since 1989."))
      ),# End of about
# Page 1: All names trend checker
  navbarMenu(title="Check a name",
    tabPanel(title="All names (since 1996)",
             h3("All names since 1996"),
             h5("Names given to less than 3 babies in a year will not appear."),
          sidebarLayout(
            sidebarPanel(h4("Filters"),
             selectInput("sexAllNames", label = h5("Sex"),
                         choices = list("Boy" = "boy", "Girl" = "girl"), 
                         selected = "boy"),
              selectizeInput("srDropDown", h5("Names"),choices = NULL,
                             multiple=TRUE)),
            mainPanel(plotOutput("allNamesRank"),
                      plotOutput("allNamesCount"))
          )
      ),
    tabPanel(title="Top 100 (since 1904)",
       h3("Top 100 since 1904"),
       h5("Names that have never ranked in the top 100 will not appear."),
       sidebarLayout(
         sidebarPanel(h4("Filters"),
              selectInput("sexAllNamesLR", label = h5("Sex"),
                          choices = list("Boy" = "boy", "Girl" = "girl"), 
                          selected = "boy"),
              selectizeInput("lrDropDown", h5("Select"),choices = NULL,
                             multiple=TRUE,
                             options = list(openOnFocus=FALSE))),
         mainPanel(h4("Type in any name to see trends over time. 
                   Note names given to less than 10 babies in a 
                   year will not appear."),
                   plotOutput("allNamesRankLR"))
       )
    )),
# Page 2: Rising Stars
    navbarMenu(title="Top Ranked",
     tabPanel(title="All Names",
              h3("Annual Top 20"),
              h5("These are the 20 most popular names in any given year. Axis shows the number of babies named that year."),
              sidebarLayout(
                sidebarPanel(title="Selections",
                     selectInput("sexTop10", label = h5("Sex"),
                                 choices = list("Boy" = "boy", "Girl" = "girl"), 
                                 selected = "boy"),
                     selectInput("yearTop10",label = h5("Year"),
                                 choices = seq(1996,2020,1),selected = 2020)),
                mainPanel(
                  plotOutput("allTopTen")))),
      tabPanel(title="Rising Stars",
       h3("Top 10 Trending"),
       h5("Of all names currently ranked in the top 100, these
          are the 10 which have most increased in 
          popularity over the last couple of decades"),
        sidebarLayout(
          sidebarPanel(title="risStarSex",
               selectInput("sexRisStar", label = h5("Sex"),
                           choices = list("Boy" = "boy", "Girl" = "girl"), 
                           selected = "boy")),
      mainPanel(
         plotOutput("risingStars")))),
     tabPanel(title="Centurians",
        h3("Top 100 for 100"),
        h5("These are the only names that have made the top 100 every year
               for the last hundred years. Only one girls name makes the cut - 
           take a guess."),
              sidebarLayout(
                sidebarPanel(title="centurianSide",
                 selectInput("sexCentuSelect", label = h5("Sex"),
                             choices = list("Boy" = "boy", "Girl" = "girl"), 
                             selected = "boy")),
              mainPanel(
                  plotOutput("centurians")))),
      tabPanel(title = "Gender Neutral",
       h3("Top gender neutral names"),
       h5("A name is considered gender neutral if it has been given 
            to over 100 boys and girls in a single year.
          This shows name ranks: in 2020 Frankie was very popular for both boys and girls. 
          Tyler is traditionally gender neutral, but has recently been far more male. In general, 
          gender neutral names rank higher for boys than girls."),
       sidebarLayout(
          sidebarPanel(title="gendNetSel",
             selectInput("genNetYear",label = h5("Year"),
                         choices = seq(1996,2020,1),selected = 2020)),
        mainPanel(plotOutput("genderNeutral"))))),
# Page 3: Strong and Stable
    navbarMenu(title="Filter by type",
      tabPanel(title="Strong and stable",
      sidebarLayout(
        sidebarPanel(
          sliderInput("consSlide", label = h4("Names consistently ranked around:"), 
                      min = 0, max = 400, value = c(50)),
        selectInput("consSex",label = h4("Select Gender"),
                    choices = list("Boy" = "boy", "Girl" = "girl"), 
                    selected = "boy")),
          mainPanel(h5("This is aimed at filtering out temporarily fashionable names. 
                      The graph shows around five or six names consistently ranked around 
                      the selected level."),
                    plotOutput("consistentNames"))))
    ),#Interactive
    tabPanel(title="Celebrity quirks",
             h4("Keira was Ciara before Knightley"),
             plotOutput("celebCiara"),
             h4("Justin was cool before Bieber"),
             plotOutput("celebJustin"),
             h4("Britney was popular, but hard to spell"),
             plotOutput("celebBritney"),
             h4("Cristiano stayed cool even after he left for Spain"),
             plotOutput("celebCristiano"))
  )#BabyNames
