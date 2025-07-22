library(shiny)
library(bslib)
library(DT)

# Some notes: Each gen adds to the last, i.e. gen 1 mons move sets are different than
# gen 1 mons in gen 2, etc. This is going to be an exponential amount of work

# Thing to maybe add later: sprites :)

# PokeAPI has all of the pokemon data by default
# DO NOT FORGET TO CREDIT THEM APPROPRIATELY

# CSV data
list_of_pkm <- read.csv("list_of_pkm.csv")
pkm_mvs_by_lvl <- read.csv("pkm_moves_by_lvl.csv")
pkm_mvs <- read.csv("pkm_moves.csv")
pkm_types <- read.csv("pkm_types.csv")


# Pokemon data
# Data is separated on a per game basis, per gen

# Oh also the code is REALLY bad but I'm not trying
# to optimize it here. I just want something that works

# Gen 1 moves (Red, Blue, Yellow)
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


# Gen 2 moves (Gold, Silver, Crystal)
g2_mv_func <- function() {
    g1_pkm_gs_pokeid <- c()
    g1_pkm_gs_name <- c()
    g1_pkm_gs_moveid <- c()
    g1_pkm_gs_movenames <- c()
    g1_pkm_gs_movetypeid <- c()
    g1_pkm_gs_movetype <- c()
    g1_pkm_gs_movepower <- c()
    g1_pkm_gs_moveacc <- c()
    g1_pkm_gs_movepp <- c()
    g1_pkm_gs_level <- c()

    g1_pkm_c_pokeid <- c()
    g1_pkm_c_name <- c()
    g1_pkm_c_moveid <- c()
    g1_pkm_c_movenames <- c()
    g1_pkm_c_movetypeid <- c()
    g1_pkm_c_movetype <- c()
    g1_pkm_c_movepower <- c()
    g1_pkm_c_moveacc <- c()
    g1_pkm_c_movepp <- c()
    g1_pkm_c_level <- c()

    # getting g1 pokemon with the right moveset
    # 252: Max gen number is 251; no easy way to get this otherwise I don't think
    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 252) {
        if (pkm_mvs_by_lvl[x, 2] == 3 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_gs_pokeid <- append(g1_pkm_gs_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_gs_moveid <- append(g1_pkm_gs_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_gs_level <- append(g1_pkm_gs_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 252) {
        if (pkm_mvs_by_lvl[x, 2] == 4 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_c_pokeid <- append(g1_pkm_c_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_c_moveid <- append(g1_pkm_c_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_c_level <- append(g1_pkm_c_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    # Extract the names from the ids
    y <- 1
    z <- 1
    while (z < max(g1_pkm_gs_pokeid) + 1 & y < length(g1_pkm_gs_pokeid) + 1) {
        if (g1_pkm_gs_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_gs_name <- append(g1_pkm_gs_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    y <- 1
    z <- 1
    while (z < max(g1_pkm_c_pokeid) + 1 & y < length(g1_pkm_c_pokeid) + 1) {
        if (g1_pkm_c_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_c_name <- append(g1_pkm_c_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    # Get the move name, type, power, pp and acc from the move id
    # 920: Max move number is 919. No easy way to get that
    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_gs_pokeid) + 1) {
        if (g1_pkm_gs_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_gs_movenames <- append(g1_pkm_gs_movenames, pkm_mvs[b, 2])
            g1_pkm_gs_movetypeid <- append(
                g1_pkm_gs_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_gs_movepower <- append(g1_pkm_gs_movepower, pkm_mvs[b, 5])
            g1_pkm_gs_movepp <- append(g1_pkm_gs_movepp, pkm_mvs[b, 6])
            g1_pkm_gs_moveacc <- append(g1_pkm_gs_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_c_pokeid) + 1) {
        if (g1_pkm_c_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_c_movenames <- append(g1_pkm_c_movenames, pkm_mvs[b, 2])
            g1_pkm_c_movetypeid <- append(
                g1_pkm_c_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_c_movepower <- append(g1_pkm_c_movepower, pkm_mvs[b, 5])
            g1_pkm_c_movepp <- append(g1_pkm_c_movepp, pkm_mvs[b, 6])
            g1_pkm_c_moveacc <- append(g1_pkm_c_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    # Get the move type from the movetype id
    w <- 1
    q <- 1
    while (w < length(g1_pkm_gs_pokeid) + 1) {
        if (g1_pkm_gs_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_gs_movetype <- append(g1_pkm_gs_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    w <- 1
    q <- 1
    while (w < length(g1_pkm_c_pokeid) + 1) {
        if (g1_pkm_c_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_c_movetype <- append(g1_pkm_c_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    # global data table that will be posted in the app
    gen_gs_pkm <<- data.frame(
        Pokedex = g1_pkm_gs_pokeid,
        Pokemon = g1_pkm_gs_name,
        Level = g1_pkm_gs_level,
        Move = g1_pkm_gs_movenames,
        Type = g1_pkm_gs_movetype,
        Power = g1_pkm_gs_movepower,
        pp = g1_pkm_gs_movepp,
        Accuracy = g1_pkm_gs_moveacc
    )

    gen_c_pkm <<- data.frame(
        Pokedex = g1_pkm_c_pokeid,
        Pokemon = g1_pkm_c_name,
        Level = g1_pkm_c_level,
        Move = g1_pkm_c_movenames,
        Type = g1_pkm_c_movetype,
        Power = g1_pkm_c_movepower,
        pp = g1_pkm_c_movepp,
        Accuracy = g1_pkm_c_moveacc
    )
}


# Gen 3 moves (Ruby, Sapphire, Emerald, FireRed, LeafGreen)
# NOTE: Deoxys is ONLY listed as version_id 5, 8 and 9
# It is therefore missing in emerald (and subsequent games)
g3_mv_func <- function() {
    g1_pkm_rs_pokeid <- c()
    g1_pkm_rs_name <- c()
    g1_pkm_rs_moveid <- c()
    g1_pkm_rs_movenames <- c()
    g1_pkm_rs_movetypeid <- c()
    g1_pkm_rs_movetype <- c()
    g1_pkm_rs_movepower <- c()
    g1_pkm_rs_moveacc <- c()
    g1_pkm_rs_movepp <- c()
    g1_pkm_rs_level <- c()

    g1_pkm_e_pokeid <- c()
    g1_pkm_e_name <- c()
    g1_pkm_e_moveid <- c()
    g1_pkm_e_movenames <- c()
    g1_pkm_e_movetypeid <- c()
    g1_pkm_e_movetype <- c()
    g1_pkm_e_movepower <- c()
    g1_pkm_e_moveacc <- c()
    g1_pkm_e_movepp <- c()
    g1_pkm_e_level <- c()

    g1_pkm_frlg_pokeid <- c()
    g1_pkm_frlg_name <- c()
    g1_pkm_frlg_moveid <- c()
    g1_pkm_frlg_movenames <- c()
    g1_pkm_frlg_movetypeid <- c()
    g1_pkm_frlg_movetype <- c()
    g1_pkm_frlg_movepower <- c()
    g1_pkm_frlg_moveacc <- c()
    g1_pkm_frlg_movepp <- c()
    g1_pkm_frlg_level <- c()

    # getting g3 pokemon with the right moveset
    # 387: Max gen number is 387; no easy way to get this otherwise I don't think
    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 387) {
        if (pkm_mvs_by_lvl[x, 2] == 5 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_rs_pokeid <- append(g1_pkm_rs_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_rs_moveid <- append(g1_pkm_rs_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_rs_level <- append(g1_pkm_rs_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 387) {
        if (pkm_mvs_by_lvl[x, 2] == 6 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_e_pokeid <- append(g1_pkm_e_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_e_moveid <- append(g1_pkm_e_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_e_level <- append(g1_pkm_e_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 387) {
        if (pkm_mvs_by_lvl[x, 2] == 7 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_frlg_pokeid <- append(
                g1_pkm_frlg_pokeid,
                pkm_mvs_by_lvl[x, 1]
            )
            g1_pkm_frlg_moveid <- append(
                g1_pkm_frlg_moveid,
                pkm_mvs_by_lvl[x, 3]
            )
            g1_pkm_frlg_level <- append(g1_pkm_frlg_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    # Extract the names from the ids
    y <- 1
    z <- 1
    while (z < max(g1_pkm_rs_pokeid) + 1 & y < length(g1_pkm_rs_pokeid) + 1) {
        if (g1_pkm_rs_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_rs_name <- append(g1_pkm_rs_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    y <- 1
    z <- 1
    while (z < max(g1_pkm_e_pokeid) + 1 & y < length(g1_pkm_e_pokeid) + 1) {
        if (g1_pkm_e_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_e_name <- append(g1_pkm_e_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    y <- 1
    z <- 1
    while (
        z < max(g1_pkm_frlg_pokeid) + 1 & y < length(g1_pkm_frlg_pokeid) + 1
    ) {
        if (g1_pkm_frlg_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_frlg_name <- append(g1_pkm_frlg_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    # Get the move name, type, power, pp and acc from the move id
    # 920: Max move number is 919. No easy way to get that
    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_rs_pokeid) + 1) {
        if (g1_pkm_rs_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_rs_movenames <- append(g1_pkm_rs_movenames, pkm_mvs[b, 2])
            g1_pkm_rs_movetypeid <- append(
                g1_pkm_rs_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_rs_movepower <- append(g1_pkm_rs_movepower, pkm_mvs[b, 5])
            g1_pkm_rs_movepp <- append(g1_pkm_rs_movepp, pkm_mvs[b, 6])
            g1_pkm_rs_moveacc <- append(g1_pkm_rs_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_e_pokeid) + 1) {
        if (g1_pkm_e_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_e_movenames <- append(g1_pkm_e_movenames, pkm_mvs[b, 2])
            g1_pkm_e_movetypeid <- append(
                g1_pkm_e_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_e_movepower <- append(g1_pkm_e_movepower, pkm_mvs[b, 5])
            g1_pkm_e_movepp <- append(g1_pkm_e_movepp, pkm_mvs[b, 6])
            g1_pkm_e_moveacc <- append(g1_pkm_e_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_frlg_pokeid) + 1) {
        if (g1_pkm_frlg_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_frlg_movenames <- append(
                g1_pkm_frlg_movenames,
                pkm_mvs[b, 2]
            )
            g1_pkm_frlg_movetypeid <- append(
                g1_pkm_frlg_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_frlg_movepower <- append(
                g1_pkm_frlg_movepower,
                pkm_mvs[b, 5]
            )
            g1_pkm_frlg_movepp <- append(g1_pkm_frlg_movepp, pkm_mvs[b, 6])
            g1_pkm_frlg_moveacc <- append(g1_pkm_frlg_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    # Get the move type from the movetype id
    w <- 1
    q <- 1
    while (w < length(g1_pkm_rs_pokeid) + 1) {
        if (g1_pkm_rs_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_rs_movetype <- append(g1_pkm_rs_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    w <- 1
    q <- 1
    while (w < length(g1_pkm_e_pokeid) + 1) {
        if (g1_pkm_e_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_e_movetype <- append(g1_pkm_e_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    w <- 1
    q <- 1
    while (w < length(g1_pkm_frlg_pokeid) + 1) {
        if (g1_pkm_frlg_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_frlg_movetype <- append(
                g1_pkm_frlg_movetype,
                pkm_types[q, 2]
            )

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    # global data table that will be posted in the app
    gen_rs_pkm <<- data.frame(
        Pokedex = g1_pkm_rs_pokeid,
        Pokemon = g1_pkm_rs_name,
        Level = g1_pkm_rs_level,
        Move = g1_pkm_rs_movenames,
        Type = g1_pkm_rs_movetype,
        Power = g1_pkm_rs_movepower,
        pp = g1_pkm_rs_movepp,
        Accuracy = g1_pkm_rs_moveacc
    )

    gen_e_pkm <<- data.frame(
        Pokedex = g1_pkm_e_pokeid,
        Pokemon = g1_pkm_e_name,
        Level = g1_pkm_e_level,
        Move = g1_pkm_e_movenames,
        Type = g1_pkm_e_movetype,
        Power = g1_pkm_e_movepower,
        pp = g1_pkm_e_movepp,
        Accuracy = g1_pkm_e_moveacc
    )

    gen_frlg_pkm <<- data.frame(
        Pokedex = g1_pkm_frlg_pokeid,
        Pokemon = g1_pkm_frlg_name,
        Level = g1_pkm_frlg_level,
        Move = g1_pkm_frlg_movenames,
        Type = g1_pkm_frlg_movetype,
        Power = g1_pkm_frlg_movepower,
        pp = g1_pkm_frlg_movepp,
        Accuracy = g1_pkm_frlg_moveacc
    )
}


# Gen 4 moves (Diamond, Pearl, Platinum, HeartGold, SoulSilver)
g4_mv_func <- function() {
    g1_pkm_dp_pokeid <- c()
    g1_pkm_dp_name <- c()
    g1_pkm_dp_moveid <- c()
    g1_pkm_dp_movenames <- c()
    g1_pkm_dp_movetypeid <- c()
    g1_pkm_dp_movetype <- c()
    g1_pkm_dp_movepower <- c()
    g1_pkm_dp_moveacc <- c()
    g1_pkm_dp_movepp <- c()
    g1_pkm_dp_level <- c()

    g1_pkm_p_pokeid <- c()
    g1_pkm_p_name <- c()
    g1_pkm_p_moveid <- c()
    g1_pkm_p_movenames <- c()
    g1_pkm_p_movetypeid <- c()
    g1_pkm_p_movetype <- c()
    g1_pkm_p_movepower <- c()
    g1_pkm_p_moveacc <- c()
    g1_pkm_p_movepp <- c()
    g1_pkm_p_level <- c()

    g1_pkm_hgss_pokeid <- c()
    g1_pkm_hgss_name <- c()
    g1_pkm_hgss_moveid <- c()
    g1_pkm_hgss_movenames <- c()
    g1_pkm_hgss_movetypeid <- c()
    g1_pkm_hgss_movetype <- c()
    g1_pkm_hgss_movepower <- c()
    g1_pkm_hgss_moveacc <- c()
    g1_pkm_hgss_movepp <- c()
    g1_pkm_hgss_level <- c()

    # getting g4 pokemon with the right moveset
    # 494: Max gen number is 493; no easy way to get this otherwise I don't think
    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 494) {
        if (pkm_mvs_by_lvl[x, 2] == 8 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_dp_pokeid <- append(g1_pkm_dp_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_dp_moveid <- append(g1_pkm_dp_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_dp_level <- append(g1_pkm_dp_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 494) {
        if (pkm_mvs_by_lvl[x, 2] == 9 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_p_pokeid <- append(g1_pkm_p_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_p_moveid <- append(g1_pkm_p_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_p_level <- append(g1_pkm_p_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 494) {
        if (pkm_mvs_by_lvl[x, 2] == 10 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_hgss_pokeid <- append(
                g1_pkm_hgss_pokeid,
                pkm_mvs_by_lvl[x, 1]
            )
            g1_pkm_hgss_moveid <- append(
                g1_pkm_hgss_moveid,
                pkm_mvs_by_lvl[x, 3]
            )
            g1_pkm_hgss_level <- append(g1_pkm_hgss_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    # Extract the names from the ids
    y <- 1
    z <- 1
    while (z < max(g1_pkm_dp_pokeid) + 1 & y < length(g1_pkm_dp_pokeid) + 1) {
        if (g1_pkm_dp_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_dp_name <- append(g1_pkm_dp_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    y <- 1
    z <- 1
    while (z < max(g1_pkm_p_pokeid) + 1 & y < length(g1_pkm_p_pokeid) + 1) {
        if (g1_pkm_p_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_p_name <- append(g1_pkm_p_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    y <- 1
    z <- 1
    while (
        z < max(g1_pkm_hgss_pokeid) + 1 & y < length(g1_pkm_hgss_pokeid) + 1
    ) {
        if (g1_pkm_hgss_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_hgss_name <- append(g1_pkm_hgss_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    # Get the move name, type, power, pp and acc from the move id
    # 920: Max move number is 919. No easy way to get that
    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_dp_pokeid) + 1) {
        if (g1_pkm_dp_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_dp_movenames <- append(g1_pkm_dp_movenames, pkm_mvs[b, 2])
            g1_pkm_dp_movetypeid <- append(
                g1_pkm_dp_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_dp_movepower <- append(g1_pkm_dp_movepower, pkm_mvs[b, 5])
            g1_pkm_dp_movepp <- append(g1_pkm_dp_movepp, pkm_mvs[b, 6])
            g1_pkm_dp_moveacc <- append(g1_pkm_dp_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_p_pokeid) + 1) {
        if (g1_pkm_p_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_p_movenames <- append(g1_pkm_p_movenames, pkm_mvs[b, 2])
            g1_pkm_p_movetypeid <- append(
                g1_pkm_p_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_p_movepower <- append(g1_pkm_p_movepower, pkm_mvs[b, 5])
            g1_pkm_p_movepp <- append(g1_pkm_p_movepp, pkm_mvs[b, 6])
            g1_pkm_p_moveacc <- append(g1_pkm_p_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_hgss_pokeid) + 1) {
        if (g1_pkm_hgss_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_hgss_movenames <- append(
                g1_pkm_hgss_movenames,
                pkm_mvs[b, 2]
            )
            g1_pkm_hgss_movetypeid <- append(
                g1_pkm_hgss_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_hgss_movepower <- append(
                g1_pkm_hgss_movepower,
                pkm_mvs[b, 5]
            )
            g1_pkm_hgss_movepp <- append(g1_pkm_hgss_movepp, pkm_mvs[b, 6])
            g1_pkm_hgss_moveacc <- append(g1_pkm_hgss_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    # Get the move type from the movetype id
    w <- 1
    q <- 1
    while (w < length(g1_pkm_dp_pokeid) + 1) {
        if (g1_pkm_dp_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_dp_movetype <- append(g1_pkm_dp_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    w <- 1
    q <- 1
    while (w < length(g1_pkm_p_pokeid) + 1) {
        if (g1_pkm_p_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_p_movetype <- append(g1_pkm_p_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    w <- 1
    q <- 1
    while (w < length(g1_pkm_hgss_pokeid) + 1) {
        if (g1_pkm_hgss_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_hgss_movetype <- append(
                g1_pkm_hgss_movetype,
                pkm_types[q, 2]
            )

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    # global data table that will be posted in the app
    gen_dp_pkm <<- data.frame(
        Pokedex = g1_pkm_dp_pokeid,
        Pokemon = g1_pkm_dp_name,
        Level = g1_pkm_dp_level,
        Move = g1_pkm_dp_movenames,
        Type = g1_pkm_dp_movetype,
        Power = g1_pkm_dp_movepower,
        pp = g1_pkm_dp_movepp,
        Accuracy = g1_pkm_dp_moveacc
    )

    gen_p_pkm <<- data.frame(
        Pokedex = g1_pkm_p_pokeid,
        Pokemon = g1_pkm_p_name,
        Level = g1_pkm_p_level,
        Move = g1_pkm_p_movenames,
        Type = g1_pkm_p_movetype,
        Power = g1_pkm_p_movepower,
        pp = g1_pkm_p_movepp,
        Accuracy = g1_pkm_p_moveacc
    )

    gen_hgss_pkm <<- data.frame(
        Pokedex = g1_pkm_hgss_pokeid,
        Pokemon = g1_pkm_hgss_name,
        Level = g1_pkm_hgss_level,
        Move = g1_pkm_hgss_movenames,
        Type = g1_pkm_hgss_movetype,
        Power = g1_pkm_hgss_movepower,
        pp = g1_pkm_hgss_movepp,
        Accuracy = g1_pkm_hgss_moveacc
    )
}


# Gen 5 moves
g5_mv_func <- function() {
    g1_pkm_bw_pokeid <- c()
    g1_pkm_bw_name <- c()
    g1_pkm_bw_moveid <- c()
    g1_pkm_bw_movenames <- c()
    g1_pkm_bw_movetypeid <- c()
    g1_pkm_bw_movetype <- c()
    g1_pkm_bw_movepower <- c()
    g1_pkm_bw_moveacc <- c()
    g1_pkm_bw_movepp <- c()
    g1_pkm_bw_level <- c()

    g1_pkm_b2w2_pokeid <- c()
    g1_pkm_b2w2_name <- c()
    g1_pkm_b2w2_moveid <- c()
    g1_pkm_b2w2_movenames <- c()
    g1_pkm_b2w2_movetypeid <- c()
    g1_pkm_b2w2_movetype <- c()
    g1_pkm_b2w2_movepower <- c()
    g1_pkm_b2w2_moveacc <- c()
    g1_pkm_b2w2_movepp <- c()
    g1_pkm_b2w2_level <- c()

    # getting g4 pokemon with the right moveset
    # 650: Max gen number is 649; no easy way to get this otherwise I don't think
    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 650) {
        if (pkm_mvs_by_lvl[x, 2] == 11 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_bw_pokeid <- append(g1_pkm_bw_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_bw_moveid <- append(g1_pkm_bw_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_bw_level <- append(g1_pkm_bw_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 650) {
        if (pkm_mvs_by_lvl[x, 2] == 14 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_b2w2_pokeid <- append(
                g1_pkm_b2w2_pokeid,
                pkm_mvs_by_lvl[x, 1]
            )
            g1_pkm_b2w2_moveid <- append(
                g1_pkm_b2w2_moveid,
                pkm_mvs_by_lvl[x, 3]
            )
            g1_pkm_b2w2_level <- append(g1_pkm_b2w2_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    # Extract the names from the ids
    y <- 1
    z <- 1
    while (
        z < max(g1_pkm_b2w2_pokeid) + 1 & y < length(g1_pkm_b2w2_pokeid) + 1
    ) {
        if (g1_pkm_b2w2_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_b2w2_name <- append(g1_pkm_b2w2_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    # Get the move name, type, power, pp and acc from the move id
    # 920: Max move number is 919. No easy way to get that
    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_bw_pokeid) + 1) {
        if (g1_pkm_bw_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_bw_movenames <- append(g1_pkm_bw_movenames, pkm_mvs[b, 2])
            g1_pkm_bw_movetypeid <- append(
                g1_pkm_bw_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_bw_movepower <- append(g1_pkm_bw_movepower, pkm_mvs[b, 5])
            g1_pkm_bw_movepp <- append(g1_pkm_bw_movepp, pkm_mvs[b, 6])
            g1_pkm_bw_moveacc <- append(g1_pkm_bw_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_b2w2_pokeid) + 1) {
        if (g1_pkm_b2w2_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_b2w2_movenames <- append(
                g1_pkm_b2w2_movenames,
                pkm_mvs[b, 2]
            )
            g1_pkm_b2w2_movetypeid <- append(
                g1_pkm_b2w2_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_b2w2_movepower <- append(
                g1_pkm_b2w2_movepower,
                pkm_mvs[b, 5]
            )
            g1_pkm_b2w2_movepp <- append(g1_pkm_b2w2_movepp, pkm_mvs[b, 6])
            g1_pkm_b2w2_moveacc <- append(g1_pkm_b2w2_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    # Get the move type from the movetype id
    w <- 1
    q <- 1
    while (w < length(g1_pkm_bw_pokeid) + 1) {
        if (g1_pkm_bw_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_bw_movetype <- append(g1_pkm_bw_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    w <- 1
    q <- 1
    while (w < length(g1_pkm_b2w2_pokeid) + 1) {
        if (g1_pkm_b2w2_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_b2w2_movetype <- append(
                g1_pkm_b2w2_movetype,
                pkm_types[q, 2]
            )

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    # global data table that will be posted in the app
    gen_bw_pkm <<- data.frame(
        Pokedex = g1_pkm_bw_pokeid,
        Pokemon = g1_pkm_bw_name,
        Level = g1_pkm_bw_level,
        Move = g1_pkm_bw_movenames,
        Type = g1_pkm_bw_movetype,
        Power = g1_pkm_bw_movepower,
        pp = g1_pkm_bw_movepp,
        Accuracy = g1_pkm_bw_moveacc
    )

    gen_b2w2_pkm <<- data.frame(
        Pokedex = g1_pkm_b2w2_pokeid,
        Pokemon = g1_pkm_b2w2_name,
        Level = g1_pkm_b2w2_level,
        Move = g1_pkm_b2w2_movenames,
        Type = g1_pkm_b2w2_movetype,
        Power = g1_pkm_b2w2_movepower,
        pp = g1_pkm_b2w2_movepp,
        Accuracy = g1_pkm_b2w2_moveacc
    )
}

# Gamecube movesets (Colloseum, XD)
# Note: Very few pokemon available for these games
gen_gc_func <- function() {
    g1_pkm_col_pokeid <- c()
    g1_pkm_col_name <- c()
    g1_pkm_col_moveid <- c()
    g1_pkm_col_movenames <- c()
    g1_pkm_col_movetypeid <- c()
    g1_pkm_col_movetype <- c()
    g1_pkm_col_movepower <- c()
    g1_pkm_col_moveacc <- c()
    g1_pkm_col_movepp <- c()
    g1_pkm_col_level <- c()

    g1_pkm_xd_pokeid <- c()
    g1_pkm_xd_name <- c()
    g1_pkm_xd_moveid <- c()
    g1_pkm_xd_movenames <- c()
    g1_pkm_xd_movetypeid <- c()
    g1_pkm_xd_movetype <- c()
    g1_pkm_xd_movepower <- c()
    g1_pkm_xd_moveacc <- c()
    g1_pkm_xd_movepp <- c()
    g1_pkm_xd_level <- c()

    # getting g4 pokemon with the right moveset
    # 650: Max gen number is 649; no easy way to get this otherwise I don't think
    x <- 1
    while (pkm_mvs_by_lvl[x, 1] < 650) {
        if (pkm_mvs_by_lvl[x, 2] == 11 & pkm_mvs_by_lvl[x, 5] > 0) {
            g1_pkm_bw_pokeid <- append(g1_pkm_bw_pokeid, pkm_mvs_by_lvl[x, 1])
            g1_pkm_bw_moveid <- append(g1_pkm_bw_moveid, pkm_mvs_by_lvl[x, 3])
            g1_pkm_bw_level <- append(g1_pkm_bw_level, pkm_mvs_by_lvl[x, 5])

            x <- x + 1
        } else {
            x <- x + 1
        }
    }

    # Extract the names from the ids
    y <- 1
    z <- 1
    while (z < max(g1_pkm_bw_pokeid) + 1 & y < length(g1_pkm_bw_pokeid) + 1) {
        if (g1_pkm_bw_pokeid[y] == list_of_pkm[z, 1]) {
            g1_pkm_bw_name <- append(g1_pkm_bw_name, list_of_pkm[z, 2])

            y <- y + 1
        } else {
            z <- z + 1
        }
    }

    # Get the move name, type, power, pp and acc from the move id
    # 920: Max move number is 919. No easy way to get that
    a <- 1
    b <- 1
    while (b < 920 & a < length(g1_pkm_bw_pokeid) + 1) {
        if (g1_pkm_bw_moveid[a] == pkm_mvs[b, 1]) {
            g1_pkm_bw_movenames <- append(g1_pkm_bw_movenames, pkm_mvs[b, 2])
            g1_pkm_bw_movetypeid <- append(
                g1_pkm_bw_movetypeid,
                pkm_mvs[b, 4]
            )
            g1_pkm_bw_movepower <- append(g1_pkm_bw_movepower, pkm_mvs[b, 5])
            g1_pkm_bw_movepp <- append(g1_pkm_bw_movepp, pkm_mvs[b, 6])
            g1_pkm_bw_moveacc <- append(g1_pkm_bw_moveacc, pkm_mvs[b, 7])

            a <- a + 1
            b <- 1
        } else {
            b <- b + 1
        }
    }

    # Get the move type from the movetype id
    w <- 1
    q <- 1
    while (w < length(g1_pkm_bw_pokeid) + 1) {
        if (g1_pkm_bw_movetypeid[w] == pkm_types[q, 1]) {
            g1_pkm_bw_movetype <- append(g1_pkm_bw_movetype, pkm_types[q, 2])

            w <- w + 1
            q <- 1
        } else {
            q <- q + 1
        }
    }

    # global data table that will be posted in the app
    gen_bw_pkm <<- data.frame(
        Pokedex = g1_pkm_bw_pokeid,
        Pokemon = g1_pkm_bw_name,
        Level = g1_pkm_bw_level,
        Move = g1_pkm_bw_movenames,
        Type = g1_pkm_bw_movetype,
        Power = g1_pkm_bw_movepower,
        pp = g1_pkm_bw_movepp,
        Accuracy = g1_pkm_bw_moveacc
    )
}

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
g3_mv_func()
g4_mv_func()
g5_mv_func()

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
    output$gengs <- DT::renderDT(
        {
            dtable <- datatable(
                gen_gs_pkm,
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

    output$genc <- DT::renderDT(
        {
            dtable <- datatable(
                gen_c_pkm,
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
    output$genrs <- DT::renderDT(
        {
            dtable <- datatable(
                gen_rs_pkm,
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

    output$gene <- DT::renderDT(
        {
            dtable <- datatable(
                gen_e_pkm,
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

    output$genfrlg <- DT::renderDT(
        {
            dtable <- datatable(
                gen_frlg_pkm,
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

    # Gen 4 table
    output$gendp <- DT::renderDT(
        {
            dtable <- datatable(
                gen_dp_pkm,
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

    output$genp <- DT::renderDT(
        {
            dtable <- datatable(
                gen_p_pkm,
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

    output$genhgss <- DT::renderDT(
        {
            dtable <- datatable(
                gen_hgss_pkm,
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

    # Gen 5 table
    output$genbw <- DT::renderDT(
        {
            dtable <- datatable(
                gen_bw_pkm,
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

    output$genb2w2 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_b2w2_pkm,
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
