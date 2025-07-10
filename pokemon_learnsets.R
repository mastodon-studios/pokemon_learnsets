library(shiny)
library(flextable)
library(bslib)

# Some notes: Each gen adds to the last, i.e. gen 1 mons move sets are different than
# gen 1 mons in gen 2, etc. This is going to be an exponential amount of work

# Looking over some resources, I want data frames and to put them into tables
# Problem: multirow tables. Solution: flextable
# Thing to maybe add later: sprites :)

# WHAT YOU WANT: https://www.youtube.com/watch?v=_as239LdGvY 6:48 in
# Sample code:
#dat <- data.frame(
#    let1 = c("a", "b", "b"),
#    let2 = c("d", "c", "e")
#)
#x <- flextable(dat)
#merge_v(x)
#^ this works to display data how you want!

# Pokemon data
# Gen 1 moves
gen_1_pkm <- data.frame(
    Pokemon = c(replicate(10, "Bulbasaur"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)

gen_1_pkm_flex <- flextable(gen_1_pkm)
vm_gen_1_pkm_flex <- merge_v(gen_1_pkm_flex)


# Gen 2 moves

# Gen 3 moves

# Gen 4 moves

# Gen 5 moves

# Gen 6 moves

# Gen 7 moves

# Gen 8 moves

# Gen 9 moves

# Pokemon generation option
gen_select <- c(1:9)


# User interface
ui <- fluidPage(
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    verbatimTextOutput("summary"),
    tableOutput("table")
)


# Server
server <- function(input, output, session) {
    output$summary <- renderPrint({
        dataset <- get(input$dataset, "package:datasets")
        summary(dataset)
    })

    output$table <- renderTable({
        dataset <- get(input$dataset, "package:datasets")
        dataset
    })
}

# calling shinyApp
shinyApp(ui, server)
