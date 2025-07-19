library(shiny)
library(bslib)
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
# Data is separated on a per game basis, per gen

# Oh also the code is REALLY bad but I'm not trying
# to optimize it here. I just want something that works

# Gen 1 moves
g1_mv_func <- function() {
    g1_pkm_rb_pokeid <- c()
    g1_pkm_rb_name <- c()
    g1_pkm_rb_moveid <- c()
    g1_pkm_rb_movenames <- c()
    g1_pkm_rb_movetypeid <- c()
    g1_pkm_rb_movetype <- c()
    g1_pkm_rb_movepower <- c()
    g1_pkm_rb_moveacc <- c()
    g1_pkm_rb_movepp <- c()
    g1_pkm_rb_level <- c()

    g1_pkm_y_pokeid <- c()
    g1_pkm_y_name <- c()
    g1_pkm_y_moveid <- c()
    g1_pkm_y_movenames <- c()
    g1_pkm_y_movetypeid <- c()
    g1_pkm_y_movetype <- c()
    g1_pkm_y_movepower <- c()
    g1_pkm_y_moveacc <- c()
    g1_pkm_y_movepp <- c()
    g1_pkm_y_level <- c()

    # getting g1 pokemon with the right moveset
    # 152: Max gen number is 151; no easy way to get this otherwise I don't think
    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 152) {
        if (pkm_mvs_by_lvl[x, 2] == 1 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_rb_pokeid <- append(g1_pkm_rb_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_rb_moveid <- append(g1_pkm_rb_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_rb_level <- append(g1_pkm_rb_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 152) {
        if (pkm_mvs_by_lvl[x, 2] == 2 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_y_pokeid <- append(g1_pkm_y_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_y_moveid <- append(g1_pkm_y_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_y_level <- append(g1_pkm_y_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    # Extract the names from the ids
    y <- 1
    z <- 1
    while (z < max(g1_pkm_rb_pokeid) + 1 & y < length(g1_pkm_rb_pokeid) + 1) {
        if (g1_pkm_rb_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_rb_name <- append(g1_pkm_rb_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    y <- 1
    z <- 1
    while (z < max(g1_pkm_y_pokeid) + 1 & y < length(g1_pkm_y_pokeid) + 1) {
        if (g1_pkm_y_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_y_name <- append(g1_pkm_y_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    # Get the move name, type, power, pp and acc from the move id
    # 920: Max move number is 919. No easy way to get that
    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_rb_pokeid) + 1) {
        if (g1_pkm_rb_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_rb_movenames <- append(g1_pkm_rb_movenames, pkm_mvs[b, 2])
            g1_pkm_rb_movetypeid <- append(
                g1_pkm_rb_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_rb_movepower <- append(g1_pkm_rb_movepower, pkm_mvs[b, 5])
            g1_pkm_rb_movepp <- append(g1_pkm_rb_movepp, pkm_mvs[b, 6])
            g1_pkm_rb_moveacc <- append(g1_pkm_rb_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_y_pokeid) + 1) {
        if (g1_pkm_y_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_y_movenames <- append(g1_pkm_y_movenames, pkm_mvs[b, 2])
            g1_pkm_y_movetypeid <- append(
                g1_pkm_y_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_y_movepower <- append(g1_pkm_y_movepower, pkm_mvs[b, 5])
            g1_pkm_y_movepp <- append(g1_pkm_y_movepp, pkm_mvs[b, 6])
            g1_pkm_y_moveacc <- append(g1_pkm_y_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    # Get the move type from the movetype id
    w <- 1
    q <- 1
    while (w < length(g1_pkm_rb_pokeid) + 1) {
        if (g1_pkm_rb_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_rb_movetype <- append(g1_pkm_rb_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    w <- 1
    q <- 1
    while (w < length(g1_pkm_y_pokeid) + 1) {
        if (g1_pkm_y_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_y_movetype <- append(g1_pkm_y_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    # global data table that will be posted in the app
    gen_rb_pkm <<- data.frame(
        Pokedex = g1_pkm_rb_pokeid,
        Pokemon = g1_pkm_rb_name,
        Level = g1_pkm_rb_level,
        Move = g1_pkm_rb_movenames,
        Type = g1_pkm_rb_movetype,
        Power = g1_pkm_rb_movepower,
        pp = g1_pkm_rb_movepp,
        Accuracy = g1_pkm_rb_moveacc
    )

    gen_y_pkm <<- data.frame(
        Pokedex = g1_pkm_y_pokeid,
        Pokemon = g1_pkm_y_name,
        Level = g1_pkm_y_level,
        Move = g1_pkm_y_movenames,
        Type = g1_pkm_y_movetype,
        Power = g1_pkm_y_movepower,
        pp = g1_pkm_y_movepp,
        Accuracy = g1_pkm_y_moveacc
    )
}


# Gen 2 moves
g2_mv_func <- function() {
    pkm_gsc_pokeid <- c()
    pkm_gsc_name <- c()
    pkm_gsc_moveid <- c()
    pkm_gsc_movenames <- c()
    pkm_gsc_movetypeid <- c()
    pkm_gsc_movetype <- c()
    pkm_gsc_movepower <- c()
    pkm_gsc_moveacc <- c()
    pkm_gsc_movepp <- c()
    pkm_gsc_level <- c()

    # getting up to g2 pokemon with the right moveset
    # 252: Max gen number is 251; no easy way to get this otherwise I don't think
    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 252) {
        if (pkm_mvs_by_lvl[x, 2] == 2 & pkm_mvs_by_lvl[x, 5] > 0) {
            pkm_gsc_pokeid <- append(pkm_gsc_pokeid, pkm_mvs_by_lvl[x, 1])
            pkm_gsc_moveid <- append(pkm_gsc_moveid, pkm_mvs_by_lvl[x, 3])
            pkm_gsc_level <- append(pkm_gsc_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    # Extract the names from the ids
    y <- 1
    z <- 1
    while (z < max(pkm_gsc_pokeid) + 1 & y < length(pkm_gsc_pokeid) + 1) {
        if (pkm_gsc_pokeid[y] == list_of_pkm[z, 1]) {
            pkm_gsc_name <- append(pkm_gsc_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    # Get the move name, type, power, pp and acc from the move id
    # 920: Max move number is 919. No easy way to get that
    a <- 1
    b <- 1
    while (b < 920 & a < length(pkm_gsc_pokeid) + 1) {
        if (pkm_gsc_moveid[a] == pkm_mvs[b, 1]) {
            pkm_gsc_movenames <- append(pkm_gsc_movenames, pkm_mvs[b, 2])
            pkm_gsc_movetypeid <- append(
                pkm_gsc_movetypeid,
                pkm_mvs[b, 4]
            )
            pkm_gsc_movepower <- append(pkm_gsc_movepower, pkm_mvs[b, 5])
            pkm_gsc_movepp <- append(pkm_gsc_movepp, pkm_mvs[b, 6])
            pkm_gsc_moveacc <- append(pkm_gsc_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    # Get the move type from the movetype id
    w <- 1
    q <- 1
    while (w < length(pkm_gsc_pokeid) + 1) {
        if (pkm_gsc_movetypeid[w] == pkm_types[q, 1]) {
            pkm_gsc_movetype <- append(pkm_gsc_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    # global data table that will be posted in the app
    gen_2_pkm <<- data.frame(
        Pokedex = pkm_gsc_pokeid,
        Pokemon = pkm_gsc_name,
        Level = pkm_gsc_level,
        Move = pkm_gsc_movenames,
        Type = pkm_gsc_movetype,
        Power = pkm_gsc_movepower,
        pp = pkm_gsc_movepp,
        Accuracy = pkm_gsc_moveacc
    )
}


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


# Function calls
g1_mv_func()
g2_mv_func()

# User interface
ui <- fluidPage(
    tabsetPanel(
        tabPanel(
            "Red/Blue",
            DT::DTOutput("genrb")
        ),
        tabPanel(
            "Yellow",
            DT::DTOutput("geny")
        ),
        tabPanel(
            "Gold/Silver",
            DT::DTOutput("gengs")
        ),
        tabPanel(
            "Crystal",
            DT::DTOutput("genc")
        ),
        tabPanel(
            "Ruby/Sapphire",
            DT::DTOutput("genrs")
        ),
        tabPanel(
            "Emerald",
            DT::DTOutput("gene")
        ),
        tabPanel(
            "FireRed/LeafGreen",
            DT::DTOutput("genfrlg")
        ),
        tabPanel(
            "Diamond/Pearl",
            DT::DTOutput("gendp")
        ),
        tabPanel(
            "Platinum",
            DT::DTOutput("genp")
        ),
        tabPanel(
            "HeartGold/SoulSilver",
            DT::DTOutput("genhgss")
        ),
        tabPanel(
            "Black/White",
            DT::DTOutput("genbw")
        ),
        tabPanel(
            "Colloseum",
            DT::DTOutput("gencol")
        ),
        tabPanel(
            "XD",
            DT::DTOutput("genxd")
        ),
        tabPanel(
            "Black 2/White 2",
            DT::DTOutput("genb2w2")
        ),
        tabPanel(
            "X/Y",
            DT::DTOutput("genxy")
        ),
        tabPanel(
            "Omega Ruby/Alpha Sapphire",
            DT::DTOutput("genoras")
        ),
        tabPanel(
            "Sun/Moon",
            DT::DTOutput("gensm")
        ),
        tabPanel(
            "Ultra Sun/Ultra Moon",
            DT::DTOutput("genusum")
        ),
        tabPanel(
            "Let's go, Pikachu!/Let's go, Eevee!",
            DT::DTOutput("genlgplge")
        ),
        tabPanel(
            "Sword/Shield",
            DT::DTOutput("genss")
        ),
        tabPanel(
            "The Isle of Armor",
            DT::DTOutput("gentioa")
        ),
        tabPanel(
            "The Crown Tundra",
            DT::DTOutput("gentct")
        ),
        tabPanel(
            "Brilliant Diamond/Shining Pearl",
            DT::DTOutput("genbdsp")
        ),
        tabPanel(
            "Legends Arceus",
            DT::DTOutput("genla")
        ),
        tabPanel(
            "Scarlet/Violet",
            DT::DTOutput("gensv")
        ),
        tabPanel(
            "The Teal Mask",
            DT::DTOutput("genttm")
        ),
        tabPanel(
            "The Indigo Disk",
            DT::DTOutput("gentid")
        )
    )
)


# Server
server <- function(input, output, session) {
    # Gen 1 tables
    output$genrb <- DT::renderDT(
        {
            dtable <- datatable(
                gen_rb_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0, 1))
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

    output$geny <- DT::renderDT(
        {
            dtable <- datatable(
                gen_y_pkm,
                rownames = FALSE,
                options = list(rowsGroup = list(0, 1))
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
                options = list(rowsGroup = list(0, 1))
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
