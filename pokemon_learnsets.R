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

# CSV data
list_of_pkm <- read.csv("list_of_pkm.csv")
pkm_mvs_by_lvl <- read.csv("pkm_moves_by_lvl.csv")
pkm_mvs <- read.csv("pkm_moves.csv")
pkm_types <- read.csv("pkm_types.csv")


# Pokemon data
# Gen 1 moves
g1_pkm_rby_pokeid <- c()
g1_pkm_rby_name <- c()
g1_pkm_rby_moveid <- c()
g1_pkm_rby_movenames <- c()
g1_pkm_rby_level <- c()


# sorting g1 into groups (?)
x <- 1
while (pkm_mvs_by_lvl[x, 1] < 152) {
    if (pkm_mvs_by_lvl[x, 2] == 1 & pkm_mvs_by_lvl[x, 5] > 0) {
        g1_pkm_rby_pokeid <- append(g1_pkm_rby_pokeid, pkm_mvs_by_lvl[x, 1])
        g1_pkm_rby_moveid <- append(g1_pkm_rby_moveid, pkm_mvs_by_lvl[x, 3])
        g1_pkm_rby_level <- append(g1_pkm_rby_level, pkm_mvs_by_lvl[x, 5])

        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 2) {
        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 3) {
        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 4) {
        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 5) {
        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 6) {
        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 7) {
        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 8) {
        x <- x + 1
    } else if (pkm_mvs_by_lvl[x, 2] == 9) {
        x <- x + 1
    } else {
        x <- x + 1
    }
}


y <- 1
z <- 1
for (y in g1_pkm_rby_pokeid) {
    if (g1_pkm_rby_pokeid[y] == list_of_pkm[z, 1]) {
        g1_pkm_rby_name <- append(g1_pkm_rby_name, list_of_pkm[z, 2])

        y <- y + 1
    } else {
        z <- z + 1
    }
}

g1_pkm_rby_movenames <- c()
a <- 1
b <- 1
c <- 1
for (x in length(g1_pkm_rby_moveid)) {
    if (g1_pkm_rby_moveid[b] == pkm_mvs[c, 1]) {
        g1_pkm_rby_movenames <- append(g1_pkm_rby_movenames, pkm_mvs[c, 2])

        b <- b + 1
        c <- 1
        print(a)
    } else {
        c <- c + 1
    }
}

tail(g1_pkm_rby_moveid, 100)


print(pkm_mvs_by_lvl[34, 2])
head(pkm_mvs_by_lvl[, c(1, 2, 3, 5)])
head(pkm_mvs_by_lvl[1, 5])
# poke_id, version_group, move_id, level
head(list_of_pkm[3, 1])
head(g1_pkm_rby_pokeid[1])
head(g1_pkm_rby_moveid[1])
head(pkm_mvs[33, 1])

pkm_mvs[33, 1] == g1_pkm_rby_moveid[1]


gen_1_pkm <- data.frame(
    #Pokedex = c(1:151),
    Pokemon = g1_pkm_rby_name,
    Level = g1_pkm_rby_level,
    Move = c(1:1069),
    Power = c(1:1069),
    Accuracy = c(1:1069),
    pp = c(1:1069)
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
