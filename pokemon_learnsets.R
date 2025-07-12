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

# Flextable example of what you might want in the future
# gen_1_pkm_flex <- flextable(gen_1_pkm)
# mv_gen_1_pkm_flex <- merge_v(gen_1_pkm_flex)

# PokeAPI has all of the pokemon data by default
# DO NOT FORGET TO CREDIT THEM APPROPRIATELY

# Need to: from pkm_moves_by_lvl, link pkm_id and version_id
# to the appropriate gen, exclude TMs/HMs, then link move_id
# to the correct move

# Pokemon data
# Gen 1 moves
gen_1_pkm <- data.frame(
    Pokemon = c(replicate(9, "Bulbasaur")),
    Level = c(c(1, 1, 7, 13, 20, 27, 34, 41, 48)),
    Move = c(c(
        "Growl",
        "Tackle",
        "Leech Seed",
        "Vine Whip",
        "Poison Powder",
        "Razor Leaf",
        "Growth",
        "Sleep Powder",
        "Solar Beam"
    )),
    Type = c(c(
        pkm_types[1],
        pkm_types[1],
        pkm_types[6],
        pkm_types[6],
        pkm_types[7],
        pkm_types[6],
        pkm_types[1],
        pkm_types[6],
        pkm_types[6]
    )),
    Power = c(c("-", "35", "-", "35", "-", "55", "-", "-", "120")),
    Accuracy = c(c("100", "95", "90", "100", "75", "95", "-", "75", "100")),
    pp = c(c(40, 35, 10, 25, 35, 25, 20, 15, 10))
)


# Gen 2 moves
gen_2_pkm <- data.frame(
    Pokemon = c(replicate(10, "Charmander"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# Gen 3 moves
gen_3_pkm <- data.frame(
    Pokemon = c(replicate(10, "Bellsprout"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# Gen 4 moves
gen_4_pkm <- data.frame(
    Pokemon = c(replicate(10, "Raticate"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# Gen 5 moves
gen_5_pkm <- data.frame(
    Pokemon = c(replicate(10, "Articuno"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# Gen 6 moves
gen_6_pkm <- data.frame(
    Pokemon = c(replicate(10, "Starmie"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# Gen 7 moves
gen_7_pkm <- data.frame(
    Pokemon = c(replicate(10, "Abra"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# Gen 8 moves
gen_8_pkm <- data.frame(
    Pokemon = c(replicate(10, "Pidgey"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# Gen 9 moves
gen_9_pkm <- data.frame(
    Pokemon = c(replicate(10, "Charmander"), "Squirtle"),
    Level = c((1:10), 1),
    Move = c((1:10), 1),
    Type = c((1:10), 1),
    Power = c((1:10), 1),
    Accuracy = c((1:10), 1),
    pp = c((1:10), 1)
)


# User interface
ui <- fluidPage(
    tabsetPanel(
        tabPanel(
            "Gen 1",
            DT::DTOutput("gen1")
        ),
        tabPanel(
            "Gen 2",
            DT::DTOutput("gen2")
        ),
        tabPanel(
            "Gen 3",
            DT::DTOutput("gen3")
        ),
        tabPanel(
            "Gen 4",
            DT::DTOutput("gen4")
        ),
        tabPanel(
            "Gen 5",
            DT::DTOutput("gen5")
        ),
        tabPanel(
            "Gen 6",
            DT::DTOutput("gen6")
        ),
        tabPanel(
            "Gen 7",
            DT::DTOutput("gen7")
        ),
        tabPanel(
            "Gen 8",
            DT::DTOutput("gen8")
        ),
        tabPanel(
            "Gen 9",
            DT::DTOutput("gen9")
        )
    )
)


# Server
server <- function(input, output, session) {
    # Gen 1 table
    output$gen1 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_1_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 2 table
    output$gen2 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_2_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 3 table
    output$gen3 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_3_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 4 table
    output$gen4 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_4_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 5 table
    output$gen5 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_5_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 6 table
    output$gen6 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_6_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 7 table
    output$gen7 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_7_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 8 table
    output$gen8 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_8_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )

    # Gen 9 table
    output$gen9 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_9_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0))
            )
            path <- getwd()
            dep <- htmltools::htmlDependency(
                "RowsGroup",
                "2.0.0",
                path,
                script = "dataTables.rowsGroup.js"
            )
            dtable$dependencies = c(dtable$dependencies, list(dep))
            dtable
        }
    )
}


# calling shinyApp
shinyApp(ui, server)
