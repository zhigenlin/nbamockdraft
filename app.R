rm(list=ls())

library(shiny)

ui <- fluidPage(
  
  titlePanel("NBA Mock Fantasy Draft Simulator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "CSV file for what you believe the true NBA Fantasy rankings are. column 1: names of players (ranked order); column 2: team name; column 3: position; column 4: estimated fantasy points [default used if no rankings supplied]",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      numericInput("attempt", "Attempt number", value = 1, min = 1, max = 400),
      textInput("mypicknumbers", 'Enter your pick numbers', "10,19,42,47,66,71,94,103,122,131"),
      numericInput("fixedpicks", "How many of the first few picks to fix?", value = 7, min = 1, max = 400),
      numericInput("totalteams", "How many teams in the league?", value = 14, min = 1, max = 400),
      numericInput("param", "Randomness (Poisson)", value = 4, min = 1, max = 400),
      numericInput("increment1", "Increment in randomness", value = 4, min = 1, max = 400)
      
    ),
    
    mainPanel(
      
      tableOutput("table1")
      
    )
    
  )
)

server <- function(input, output) {
  
  filedefault <- reactive({
    if (is.null(input$file1)) {
      V1=c("Nikola Jokic","Bradley Beal","Luka Doncic","Damian Lillard","James Harden","Karl-Anthony Towns","Giannis Antetokounmpo","De'Aaron Fox","Trae Young","Paul George","Stephen Curry","Jayson Tatum","Kevin Durant","Donovan Mitchell","Nikola Vucevic","Bam Adebayo","Fred VanVleet","Zion Williamson","Shai Gilgeous-Alexander","Julius Randle","Devin Booker","Joel Embiid","Jrue Holiday","LeBron James","Collin Sexton","Domantas Sabonis","Ben Simmons","LaMelo Ball","Jimmy Butler","Dejounte Murray","Ja Morant","Khris Middleton","Deandre Ayton","DeMar DeRozan","Rudy Gobert","Zach LaVine","Brandon Ingram","Chris Paul","Tobias Harris","CJ McCollum","Kyrie Irving","Christian Wood","Terry Rozier","Jaylen Brown","Michael Porter","Montrezl Harrell","Anthony Edwards","Russell Westbrook","Caris LeVert","Buddy Hield","Anthony Davis","Tyrese Haliburton","Spencer Dinwiddie","Darius Garland","Lonzo Ball","Jalen Suggs","Kyle Lowry","Andrew Wiggins","RJ Barrett","Jakob Poeltl","Malcolm Brogdon","Cade Cunningham","Jarrett Allen","Derrick White","Clint Capela","Draymond Green","Pascal Siakam","Norman Powell","OG Anunoby","Jerami Grant","Harrison Barnes","D'Angelo Russell","Jusuf Nurkic","Devonte' Graham","Dennis Schroder","Jalen Green","Kemba Walker","Mikal Bridges","Eric Bledsoe","Reggie Jackson","Bojan Bogdanovic","Killian Hayes","Marcus Smart","Richaun Holmes","Nickeil Alexander-Walker","Joe Harris","Mike Conley","Tim Hardaway","Thaddeus Young","Myles Turner","Wendell Carter","Evan Fournier","Kristaps Porzingis","Isaac Okoro","Joe Ingles","P.J.Washington","Isaiah Stewart","Ivica Zubac","Robert Williams III","Jonathan Isaac","Saddiq Bey","Chris Boucher","Monte Morris","John Collins","Kevin Porter","Jonas Valanciunas","Robert Covington","Keldon Johnson","Brook Lopez","Daniel Theis","Jaren Jackson","Patrick Williams","Malik Beasley","Bogdan Bogdanovic","Seth Curry","Kyle Kuzma","Miles Bridges","Rui Hachimura","Scottie Barnes","Donte DiVincenzo","T.J.Warren","Steven Adams","Jordan Clarkson","Darius Bazley","Tyler Herro","Duncan Robinson","Gordon Hayward","Dillon Brooks","Kelly Olynyk","Doug McDermott","Brandon Clarke","Markelle Fultz","Ricky Rubio","Klay Thompson","Isaiah Roby","Thomas Bryant","Marcus Morris Sr.","Marvin Bagley III","Lauri Markkanen","Mitchell Robinson","T.J.McConnell","John Wall","Dorian Finney-Smith","Mason Plumlee","Aaron Gordon","Luguentz Dort","Derrick Rose","Cole Anthony","Kevin Huerter","Goran Dragic","Royce O'Neale","Aleksej Pokusevski","Alperen Sengun","Danny Green","Jalen Brunson","Talen Horton-Tucker","Otto Porter","Kelly Oubre","Reggie Bullock","Kyle Anderson","Terrence Ross","Jae Crowder","Bryn Forbes","Coby White","Bobby Portis","Chuma Okeke","Gary Trent","Tomas Satoransky","Cedi Osman","Nicolas Batum","Terance Mann")
      V2=c("Den","Wsh","Dal","Por","Bkn","Min","Mil","Sac","Atl","LAC","GS","Bos","Bkn","Utah","Chi","Mia","Tor","NO","OKC","NY","Phx","Phi","Mil","LAL","Cle","Ind","Phi","Cha","Mia","SA","Mem","Mil","Phx","Chi","Utah","Chi","NO","Phx","Phi","Por","Bkn","Hou","Cha","Bos","Den","Wsh","Min","LAL","Ind","Sac","LAL","Sac","Wsh","Cle","Chi","Orl","Mia","GS","NY","SA","Ind","Det","Cle","SA","Atl","GS","Tor","Por","Tor","Det","Sac","Min","Por","NO","Bos","Hou","NY","Phx","Mem","LAC","Utah","Det","Bos","Sac","NO","Bkn","Utah","Dal","SA","Ind","Orl","NY","Dal","Cle","Utah","Cha","Det","LAC","Bos","Orl","Det","Tor","Den","Atl","Hou","NO","Por","SA","Mil","Hou","Mem","Chi","Min","Atl","Phi","Wsh","Cha","Wsh","Tor","Mil","Ind","Mem","Utah","OKC","Mia","Mia","Cha","Mem","Det","SA","Mem","Orl","Cle","GS","OKC","Wsh","LAC","Sac","Cle","NY","Ind","Hou","Dal","Cha","Den","OKC","NY","Orl","Atl","Tor","Utah","OKC","Hou","Phi","Dal","LAL","GS","Cha","Dal","Mem","Orl","Phx","SA","Chi","Mil","Orl","Tor","Chi","Cle","LAC","LAC")
      V3=c("C","SG","PG","PG","PG/SG","C","PF","PG","PG","SG/SF","PG","SF/PF","SF/PF","PG/SG","C","PF/C","PG/SG","PF","PG/SG","PF","PG/SG","C","PG/SG","PG/SF/PF","PG/SG","PF","PG","PG","SG/SF","PG/SG","PG","SG/SF","C","SG/SF","C","SG","SF/PF","PG","SF/PF","SG","PG/SG","PF/C","PG/SG","SG/SF","SF/PF","PF/C","SG/SF","PG","SG/SF","SG/SF","PF","PG/SG","PG/SG","PG/SG","PG","PG/SG","PG","SG/SF","SG/SF","C","PG/SG","PG","C","PG/SG","C","PF","PF","SG/SF","SF/PF","SF/PF","SF/PF","PG/SG","C","PG/SG","PG","SG","PG","SF","PG/SG","PG","SF/PF","PG","PG/SG","PF/C","SG","SG/SF","PG","SG/SF","SF/PF/C","PF/C","C","SG/SF","PF/C","SF","SG/SF/PF","PF","PF/C","C","PF/C","SF/PF","SF","PF/C","PG","PF","PG/SG","C","SF/PF","SF/PF","C","PF/C","PF","SF/PF","SG/SF","SG/SF","PG/SG","SF/PF","SF/PF","SF/PF","SF/PF","PG/SG","SF","C","SG","SF/PF","PG/SG","SG/SF","SF","SG/SF","PF/C","SF/PF","PF/C","PG","PG","SG","PF","C","SF/PF","PF","PF","C","PG","PG","SF/PF","C","PF","SG/SF","PG","PG","SG/SF","PG","SF/PF","PF","C","SG/SF","PG","SG/SF","SF","SF/PF","SG/SF","SF/PF","SG/SF","SF/PF","PG/SG","PG","PF","PF","SG/SF","PG/SG","SF","SG/SF","SF")
      V4=c("7126","6103","6036","6012","5996","5980","5929","5889","5755","5716","5691","5682","5619","5564","5559","5533","5531","5489","5406","5398","5359","5315","5284","5249","5216","5135","5132","5124","5122","5102","5089","5051","5020","5008","5007","5004","4991","4986","4928","4916","4908","4907","4888","4877","4855","4839","4831","4790","4757","4750","4722","4700","4687","4663","4640","4607","4601","4557","4535","4509","4502","4485","4475","4467","4466","4438","4431","4419","4355","4338","4316","4315","4293","4279","4273","4272","4271","4247","4223","4119","4100","4099","4085","4084","4083","4074","4072","4072","4054","4049","4048","4047","4046","4036","4030","4020","4015","3999","3998","3961","3925","3925","3922","3918","3910","3908","3906","3886","3869","3856","3854","3836","3829","3824","3820","3815","3793","3754","3747","3723","3707","3697","3678","3677","3676","3665","3644","3636","3628","3611","3572","3568","3543","3537","3528","3520","3509","3495","3444","3439","3437","3429","3422","3420","3413","3413","3384","3362","3348","3342","3315","3309","3296","3294","3273","3262","3260","3259","3259","3258","3253","3235","3216","3210","3195","3194","3172","3151","3142","3122","3122")
      data.frame(V1,V2,V3,V4)
    } else {
      read.csv(input$file1$datapath,header=F)
    }
  })
  
  output$table1 <- renderTable({ 
    
    # Check you can use at the end of rounds
    # length(picked)+ourpicknumbers[round]==totalpicks #check
    
    # Note: round 2 is different from round 1 in
    #  mychoices = c(mychoices,mychoice)
    
    nbalist =  filedefault()
    
    set.seed(input$attempt)
    theparam = input$param
    ourpicknumbers = as.integer(strsplit(input$mypicknumbers,",")[[1]])
    totalrounds = length(ourpicknumbers)
    totalpicks = input$totalteams * totalrounds
    
    
    setupleague <- function(nbaleague) {
      picked = 1:input$fixedpicks
      
      # decide on initial theparam
      shufflesizes = NULL
      while (sum(shufflesizes)<totalpicks) {
        shufflesizes = c(shufflesizes,rpois(1,theparam))
        # decide on theparam increment
        theparam=theparam+input$increment1
      }
      shufflesizes = shufflesizes+input$fixedpicks
      shufflesizes = cumsum(shufflesizes)
      shufflesizes = shufflesizes[shufflesizes<totalpicks]
      shufflesizes = c(shufflesizes,totalpicks)
      
      start=input$fixedpicks+1
      end=shufflesizes[1]
      picked = c(picked,sample(start:end,length(start:end)))
      
      for (i in 2:length(shufflesizes)) {
        start = shufflesizes[i-1]+1
        end = shufflesizes[i]
        picked = c(picked,sample(start:end,length(start:end)))
      }
      picked
    }
    picked = setupleague(picked); remaining = 1:totalpicks
    br
    
    setupround <- function(round,thepicknumbers,picked) {
      picksthisround = (thepicknumbers[round-1]+1):(thepicknumbers[round]-1)
      picksthisround = picksthisround-thepicknumbers[round-1]
      chosen = picked[picksthisround]
      #nbalist[chosen,c(1,2,3,4)]
      remaining =  remaining [! remaining %in% chosen]
      newList <- list("picksthisround" = picksthisround, "chosen" = chosen, "remaining"=remaining)
    }
    
    # ======================================
    # Simulation - Round 1
    # ======================================
    round=1;picksthisround = 1:(ourpicknumbers[round]-1);chosen = picked[picksthisround];remaining =  remaining [! remaining %in% chosen]
    mychoice = remaining[1]; nbalist[mychoice,c(1,2,3,4)]
    remaining =  remaining [! remaining %in% mychoice]; picked =  picked [! picked %in% c(chosen,mychoice)]; mychoices = mychoice
    
    # ======================================
    # Simulation - The rest of the rounds
    # ======================================
    for (round in 2:totalrounds) {
      details = setupround(round,ourpicknumbers,picked); picksthisround=details$picksthisround; chosen=details$chosen; remaining=details$remaining
      mychoice = remaining[1]; nbalist[mychoice,c(1,2,3,4)]
      remaining =  remaining [! remaining %in% mychoice]; picked =  picked [! picked %in% c(chosen,mychoice)]; mychoices = c(mychoices,mychoice)
    }
    
    a = nbalist[mychoices,c(1,2,3,4)]
    names(a) = c("Name","Team","Position","Points")
    a
    
  })
  
}

shinyApp(ui = ui, server = server)

runApp("my_app")
