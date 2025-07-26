library(shiny)
library(bslib)
library(DT)

# Thing to maybe add later: sprites :)

# PokeAPI has all of the pokemon data by default
# DO NOT FORGET TO CREDIT THEM APPROPRIATELY

# CSV data (from PokeAPI)
list_of_pkm <- read.csv("list_of_pkm.csv")
pkm_mvs_by_lvl <- read.csv("pkm_moves_by_lvl.csv")
pkm_mvs <- read.csv("pkm_moves.csv")
pkm_types <- read.csv("pkm_types.csv")


# Pokemon data
# Data is separated on a per game basis, per gen
pokemon_data_sorting <- function() {
    # Making the appropriate vectors
    # 1:559385: excludes regional forms; I'm handling those separately
    pokedex <- pkm_mvs_by_lvl[1:559385, 1]
    pokemon <- vector("numeric", length = 559385) # predefined vector with size
    version <- pkm_mvs_by_lvl[1:559385, 2]
    level <- pkm_mvs_by_lvl[1:559385, 5]
    move_id <- pkm_mvs_by_lvl[1:559385, 3]
    move_name <- vector("numeric", length = 559385)
    move_type_id <- vector("numeric", length = 559385)
    move_type <- vector("numeric", length = 559385)
    move_power <- vector("numeric", length = 559385)
    move_pp <- vector("numeric", length = 559385)
    move_acc <- vector("numeric", length = 559385)

    # Regional Forms; 50541 is nrow(pkm_mvs_by_lvl) - 559385
    reg_pokedex <- pkm_mvs_by_lvl[559386:nrow(pkm_mvs_by_lvl), 1]
    reg_pokemon <- vector("numeric", length = 50541) # predefined vector with size
    reg_version <- pkm_mvs_by_lvl[559386:nrow(pkm_mvs_by_lvl), 2]
    reg_level <- pkm_mvs_by_lvl[559386:nrow(pkm_mvs_by_lvl), 5]
    reg_move_id <- pkm_mvs_by_lvl[559386:nrow(pkm_mvs_by_lvl), 3]
    reg_move_name <- vector("numeric", length = 50541)
    reg_move_type_id <- vector("numeric", length = 50541)
    reg_move_type <- vector("numeric", length = 50541)
    reg_move_power <- vector("numeric", length = 50541)
    reg_move_pp <- vector("numeric", length = 50541)
    reg_move_acc <- vector("numeric", length = 50541)

    # Relatively fast; list of pokemon from pokedex
    for (x in 1:559386) {
        if (pokedex[x] %in% list_of_pkm[, 1]) {
            pokemon[x] <- replace(
                pokemon[x],
                pokemon[x] == 0,
                list_of_pkm[pokedex[x], 2]
            )
        }
    }

    # Same as above, but for regional forms
    for (x in 1:50541) {
        if (reg_pokedex[x] %in% list_of_pkm[, 1]) {
            val <- which(list_of_pkm[, 1] %in% reg_pokedex[x])
            reg_pokemon[x] <- replace(
                reg_pokemon[x],
                reg_pokemon[x] == 0,
                list_of_pkm[val, 2]
            )
        }
    }

    # Relatively fast; getting relevant move information from the id
    for (a in 1:559386) {
        if (move_id[a] %in% pkm_mvs[, 1]) {
            move_name[a] <- replace(
                move_name[a],
                move_name[a] == 0,
                pkm_mvs[move_id[a], 2]
            )

            move_type_id[a] <- replace(
                move_type_id[a],
                move_type_id[a] == 0,
                pkm_mvs[move_id[a], 4]
            )

            move_power[a] <- replace(
                move_power[a],
                move_power[a] == 0,
                pkm_mvs[move_id[a], 5]
            )

            move_pp[a] <- replace(
                move_pp[a],
                move_pp[a] == 0,
                pkm_mvs[move_id[a], 6]
            )

            move_acc[a] <- replace(
                move_acc[a],
                move_acc[a] == 0,
                pkm_mvs[move_id[a], 7]
            )
        }
    }

    # Same as above, but for regional forms
    for (a in 1:50541) {
        if (reg_move_id[a] %in% pkm_mvs[, 1]) {
            reg_move_name[a] <- replace(
                reg_move_name[a],
                reg_move_name[a] == 0,
                pkm_mvs[reg_move_id[a], 2]
            )

            reg_move_type_id[a] <- replace(
                reg_move_type_id[a],
                reg_move_type_id[a] == 0,
                pkm_mvs[reg_move_id[a], 4]
            )

            reg_move_power[a] <- replace(
                reg_move_power[a],
                reg_move_power[a] == 0,
                pkm_mvs[reg_move_id[a], 5]
            )

            reg_move_pp[a] <- replace(
                reg_move_pp[a],
                reg_move_pp[a] == 0,
                pkm_mvs[reg_move_id[a], 6]
            )

            reg_move_acc[a] <- replace(
                reg_move_acc[a],
                reg_move_acc[a] == 0,
                pkm_mvs[reg_move_id[a], 7]
            )
        }
    }

    # relatively fast; move type from id
    for (q in 1:559386) {
        if (move_type_id[q] %in% pkm_types[, 1]) {
            move_type[q] <- replace(
                move_type[q],
                move_type[q] == 0,
                pkm_types[move_type_id[q], 2]
            )
        }
    }

    # Same as above, but for regional forms
    for (q in 1:50541) {
        if (reg_move_type_id[q] %in% pkm_types[, 1]) {
            reg_move_type[q] <- replace(
                reg_move_type[q],
                reg_move_type[q] == 0,
                pkm_types[reg_move_type_id[q], 2]
            )
        }
    }

    # Data table containing ALL pokemon information I want to plot
    all_pkm_data <- data.frame(
        Pokedex = pokedex,
        Pokemon = pokemon,
        Version = version,
        Level = level,
        Move = move_name,
        Type = move_type,
        Power = move_power,
        pp = move_pp,
        Accuracy = move_acc
    )

    # Regional forms
    reg_pkm_data <- data.frame(
        Pokedex = reg_pokedex,
        Pokemon = reg_pokemon,
        Version = reg_version,
        Level = reg_level,
        Move = reg_move_name,
        Type = reg_move_type,
        Power = reg_move_power,
        pp = reg_move_pp,
        Accuracy = reg_move_acc
    )

    # Separate all the data into relevant generations (by game)
    # This EXCLUDES TM/HMs
    gen_rb_pkm <<- subset(
        all_pkm_data,
        Version == 1 & Level != 0,
        select = -c(Version)
    )
    gen_y_pkm <<- subset(
        all_pkm_data,
        Version == 2 & Level != 0,
        select = -c(Version)
    )
    gen_gs_pkm <<- subset(
        all_pkm_data,
        Version == 3 & Level != 0,
        select = -c(Version)
    )
    gen_c_pkm <<- subset(
        all_pkm_data,
        Version == 4 & Level != 0,
        select = -c(Version)
    )
    gen_rs_pkm <<- subset(
        all_pkm_data,
        Version == 5 & Level != 0,
        select = -c(Version)
    )
    gen_e_pkm <<- subset(
        all_pkm_data,
        Version == 6 & Level != 0,
        select = -c(Version)
    )
    gen_frlg_pkm <<- subset(
        all_pkm_data,
        Version == 7 & Level != 0,
        select = -c(Version)
    )
    gen_dp_pkm <<- subset(
        all_pkm_data,
        Version == 8 & Level != 0,
        select = -c(Version)
    )
    gen_p_pkm <<- subset(
        all_pkm_data,
        Version == 9 & Level != 0,
        select = -c(Version)
    )
    gen_hgss_pkm <<- subset(
        all_pkm_data,
        Version == 10 & Level != 0,
        select = -c(Version)
    )
    gen_bw_pkm <<- subset(
        all_pkm_data,
        Version == 11 & Level != 0,
        select = -c(Version)
    )
    gen_col_pkm <<- subset(
        all_pkm_data,
        Version == 12 & Level != 0,
        select = -c(Version)
    )
    gen_xd_pkm <<- subset(
        all_pkm_data,
        Version == 13 & Level != 0,
        select = -c(Version)
    )
    gen_b2w2_pkm <<- subset(
        all_pkm_data,
        Version == 14 & Level != 0,
        select = -c(Version)
    )
    gen_xy_pkm <<- subset(
        all_pkm_data,
        Version == 15 & Level != 0,
        select = -c(Version)
    )
    gen_oras_pkm <<- subset(
        all_pkm_data,
        Version == 16 & Level != 0,
        select = -c(Version)
    )
    gen_sm_pkm <<- subset(
        all_pkm_data,
        Version == 17 & Level != 0,
        select = -c(Version)
    )
    gen_usum_pkm <<- subset(
        all_pkm_data,
        Version == 18 & Level != 0,
        select = -c(Version)
    )
    gen_lgplge_pkm <<- subset(
        all_pkm_data,
        Version == 19 & Level != 0,
        select = -c(Version)
    )
    gen_ss_pkm <<- subset(
        all_pkm_data,
        Version == 20 & Level != 0,
        select = -c(Version)
    )
    #gen_tioa_pkm <<- subset(all_pkm_data, Version == 21 & Level != 0, select = -c(Version)) # missing
    #gen_tct_pkm <<- subset(all_pkm_data, Version == 22 & Level != 0, select = -c(Version)) # missing
    gen_bdsp_pkm <<- subset(
        all_pkm_data,
        Version == 23 & Level != 0,
        select = -c(Version)
    )
    gen_la_pkm <<- subset(
        all_pkm_data,
        Version == 24 & Level != 0,
        select = -c(Version)
    )
    gen_sv_pkm <<- subset(
        all_pkm_data,
        Version == 25 & Level != 0,
        select = -c(Version)
    )
    #gen_ttm_pkm <<- subset(all_pkm_data, Version == 26 & Level != 0, select = -c(Version)) # missing
    #gen_tid_pkm <<- subset(all_pkm_data, Version == 27 & Level != 0, select = -c(Version)) # missing

    # Regional forms
    gen_reg_pkm <<- subset(reg_pkm_data, select = -c(Version))
}


# Function call
pokemon_data_sorting()


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
        #tabPanel(
        #    "The Isle of Armor",
        #    DT::DTOutput("gentioa")
        #),
        #tabPanel(
        #    "The Crown Tundra",
        #    DT::DTOutput("gentct")
        #),
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
        #tabPanel(
        #    "The Teal Mask",
        #    DT::DTOutput("genttm")
        #),
        #tabPanel(
        #    "The Indigo Disk",
        #    DT::DTOutput("gentid")
        #),
        tabPanel(
            "Regional Forms",
            DT::DTOutput("genreg")
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

    output$gencol <- DT::renderDT(
        {
            dtable <- datatable(
                gen_col_pkm,
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

    output$genxd <- DT::renderDT(
        {
            dtable <- datatable(
                gen_xd_pkm,
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

    # Gen 5 tables
    output$genbw <- DT::renderDT(
        {
            dtable <- datatable(
                gen_bw_pkm,
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

    output$genb2w2 <- DT::renderDT(
        {
            dtable <- datatable(
                gen_b2w2_pkm,
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

    # Gen 6 tables
    output$genxy <- DT::renderDT(
        {
            dtable <- datatable(
                gen_xy_pkm,
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

    output$genoras <- DT::renderDT(
        {
            dtable <- datatable(
                gen_oras_pkm,
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

    # Gen 7 table
    output$gensm <- DT::renderDT(
        {
            dtable <- datatable(
                gen_sm_pkm,
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

    output$genusum <- DT::renderDT(
        {
            dtable <- datatable(
                gen_usum_pkm,
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

    output$genlgplge <- DT::renderDT(
        {
            dtable <- datatable(
                gen_lgplge_pkm,
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

    # Gen 8 table
    output$genss <- DT::renderDT(
        {
            dtable <- datatable(
                gen_ss_pkm,
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

    #output$gentioa <- DT::renderDT(
    #    {
    #        dtable <- datatable(
    #            gen_tioa_pkm,
    #            rownames = FALSE,
    #            options = list(rowsGroup = list(0, 1))
    #        )
    #        path <- getwd()
    #        dep <- htmltools::htmlDependency(
    #            "RowsGroup",
    #            "2.0.0",
    #            path,
    #            script = "dataTables.rowsGroup.js"
    #        )
    #        dtable$dependencies = c(dtable$dependencies, list(dep))
    #        dtable
    #    }
    #)

    #output$gentct <- DT::renderDT(
    #    {
    #        dtable <- datatable(
    #            gen_tct_pkm,
    #            rownames = FALSE,
    #            options = list(rowsGroup = list(0, 1))
    #        )
    #        path <- getwd()
    #        dep <- htmltools::htmlDependency(
    #            "RowsGroup",
    #            "2.0.0",
    #            path,
    #            script = "dataTables.rowsGroup.js"
    #        )
    #        dtable$dependencies = c(dtable$dependencies, list(dep))
    #        dtable
    #    }
    #)

    output$genbdsp <- DT::renderDT(
        {
            dtable <- datatable(
                gen_bdsp_pkm,
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

    output$genla <- DT::renderDT(
        {
            dtable <- datatable(
                gen_la_pkm,
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

    # Gen 9 table
    output$gensv <- DT::renderDT(
        {
            dtable <- datatable(
                gen_sv_pkm,
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

    #output$genttm <- DT::renderDT(
    #    {
    #        dtable <- datatable(
    #            gen_ttm_pkm,
    #            rownames = FALSE,
    #            options = list(rowsGroup = list(0, 1))
    #        )
    #        path <- getwd()
    #        dep <- htmltools::htmlDependency(
    #            "RowsGroup",
    #            "2.0.0",
    #            path,
    #            script = "dataTables.rowsGroup.js"
    #        )
    #        dtable$dependencies = c(dtable$dependencies, list(dep))
    #        dtable
    #    }
    #)

    #output$gentid <- DT::renderDT(
    #    {
    #        dtable <- datatable(
    #            gen_tid_pkm,
    #            rownames = FALSE,
    #            options = list(rowsGroup = list(0, 1))
    #        )
    #        path <- getwd()
    #        dep <- htmltools::htmlDependency(
    #            "RowsGroup",
    #            "2.0.0",
    #           path,
    #           script = "dataTables.rowsGroup.js"
    #       )
    #        dtable$dependencies = c(dtable$dependencies, list(dep))
    #        dtable
    #    }
    #)

    # Regional forms
    output$genreg <- DT::renderDT(
        {
            dtable <- datatable(
                gen_reg_pkm,
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
}

# calling shinyApp
shinyApp(ui, server)
