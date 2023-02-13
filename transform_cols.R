
###################################################################################################
check_needed_colum <- function(v) {
    var <- v
    if (var %in% names(copy_gdm)) {
        print("The var already exist")
        bool <- TRUE
        } else {
            print("The var NOT exist yet !!!")
            bool <- FALSE
        }
    return(bool)
}

#################################################



create_new_var <- function(check) {
    if (check == FALSE) {
        print("ERROR: ===========")
    } else {
        new_col <- as.vector(copy_gdm$PROTO)
        MH <- cbind(MH, new_col)

    }

    return(MH)
}


##############################################




