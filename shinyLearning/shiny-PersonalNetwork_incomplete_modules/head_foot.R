# Header and Footer
# Sun Mar 01 10:51:58 2020 ------------------------------


leftText <- 
  shiny::HTML(paste0(
  "<small>&copy;  p.walker"
))

rightText <- 
  shiny::HTML(paste0(
  "<script>",
  "var today = new Date();",
  "var yyyy = today.getFullYear();",
  "</script>",
  "<i class='fas fa-user-astronaut fa-lg'></i>",
  " a pickles product - <script>document.write(yyyy);</script>",
  "<table style='margin-left:auto; margin-right:auto;'>",
  "</table>")
  )

inactiveLink <-
  tags$head(tags$style(".inactiveLink {
                      pointer-events: none;
                      cursor: not-allowed;
                      opacity: 0.5;
                       }"))

# cursor: default;