library(shiny)
library(bslib)
library(flextable)
library(DT)

# Some notes: Each gen adds to the last, i.e. gen 1 mons move sets are different than
# gen 1 mons in gen 2, etc. This is going to be an exponential amount of work

# Looking over some resources, I want data frames and to put them into tables
# Problem: multirow tables. Solution: flextable
# Thing to maybe add later: sprites :)

# Note: flextable doesn't work with Shiny :( but I found a DT solution that looks ok

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

# keeping these for now as an example
gen_1_pkm_flex <- flextable(gen_1_pkm)
mv_gen_1_pkm_flex <- merge_v(gen_1_pkm_flex)


# Gen 2 moves
gen_2_pkm <- data.frame(
    Pokemon = c(replicate(10, "Bulbasaur"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)

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
    #textOutput("test"),

    selectInput("generations", "Generation Selector", gen_select),
    DT::DTOutput("gen1")
)


# Server
server <- function(input, output, session) {
    #output$test <- renderText(
    #    {
    #        paste("Current Gen is:", input$generations)
    #    }
    #)

    # I want to conditionally show the table based on the selector input
    # tackle this another day
    if (input$generations == 1) {
        output$gen1 <- DT::renderDT(
            {
                dtable <- datatable(
                    gen_1_pkm,
                    rownames = FALSE,
                    options = list(rowsGroup = list(0))
                )
                path <- "/home/nick/R/x86_64-pc-linux-gnu-library/4.5/DT"
                dep <- htmltools::htmlDependency(
                    "RowsGroup",
                    "2.0.0",
                    path,
                    script = "dataTables.rowsGroup.js"
                )
                dtable$dependencies <- c(dtable$dependencies, list(dep))
                dtable
            }
        )
    } else {}
}

# calling shinyApp
shinyApp(ui, server)
