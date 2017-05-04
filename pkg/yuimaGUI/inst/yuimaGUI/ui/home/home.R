tabItem(tabName = "home",
        fluidRow(
          column(12,
                 h1("Welcome to yuimaGUI", align = "center", class = "hTitle"),
                 h4("an amazingly powerful tool for your analysis", align = "center"), 
                 hr(class = "hrHeader"),
                 h4("Get acquainted with yuimaGUI and learn how to best exploit it in a few simple steps:", class = "hTitle", align = "center"),
                 br()
          )),
        fluidRow(
          column(8,
                 h4("Step 1", class = "hTitle"),
                 h4("Load the data you wish to analyze in section 'Data I/O'.", br(), 
                    "An easy way to load economic data (i.e. GDP) or financial series (stocks and shares) from the Internet is provided. Otherwise you can load data from your own files.",br(),
                    "Once data are loaded, you can go and use sections 'Explorative Data Analysis' and 'Modeling'."),
                 h4("Step 2", class = "hTitle"),
                 h4("Model data in section 'Modeling'.", br(),
                    "Here you can fit models choosing between some default options or defining your own model.", br(),
                    "Now you are ready to go to section 'Simulate'."),
                 h4("Step 3", class = "hTitle"),
                 h4("Read the short explanation at the beginning of every section for further information. Enjoy!")
          ),
          column(4,
                 br(), br(),
                 uiOutput("video_intro", align = "center")
          )
        ),
        fluidRow(
          column(8,h4(),br(),br(),br(),
                 h4("Tips", class = "hTitle"),
                 h4("Press F11 to go to full screen.", br(),
                    "Press CTRL+ or CTRL- to zoom in and out.")
          ),
          column(4,
                 h3(em("Developed by"), class = "hTitle", align = "center"),
                 h4("Emanuele Guidotti", align = "center"),
                 h3(em("in collaboration with"), class = "hTitle", align = "center"),
                 h4("Stefano M. Iacus & Lorenzo Mercuri", align = "center")
          )
        )
)
