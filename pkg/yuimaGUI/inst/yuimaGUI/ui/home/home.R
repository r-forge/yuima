tabItem(tabName = "home",
        fluidRow(
          column(12,
                 h1("Welcome to yuimaGUI", align = "center", class = "hTitle"),
                 h4("An amazingly powerful tool for your analyses", align = "center"), 
                 hr(class = "hrHeader"),
                 h4("Get acquainted with yuimaGUI and learn how to get the most out of it in a few simple steps", class = "hTitle", align = "center"),
                 br()
          )),
        fluidRow(
          column(8,
                 h4("Step 1", class = "hTitle"),
                 h4("Load the data you wish to analyze in the section 'Data I/O'.", br(), 
                    "The interface provides an easy way to load economic data (e.g. GDP) or a financial series (stocks and shares) from the Internet. If you prefer, you can load data from your own files.",br(),
                    "Once the data is loaded, you can then use the 'Explorative Data Analysis' and 'Modeling' sections."),
                 h4("Step 2", class = "hTitle"),
                 h4("Model data in section 'Modeling'.", br(),
                    "Here you can fit models choosing between a number of default options or defining your own model.", br(),
                    "Now you are ready to go to the 'Simulate' section."),
                 h4("Step 3", class = "hTitle"),
                 h4("Read the short explanation at the beginning of each section for further information. Enjoy!")
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
