# f = "TEXT_OUTPUT/MH_text_output.txt"
#####################################################################
write_dashes <- function(f) {
    dashes <- "===================================================================================="
    write(dashes, file= f, append=TRUE)
}
#####################################################################
empty_line <- function(f) {
    write(" ", file= f, append=TRUE)
}
#####################################################################
write_header <- function(f) {
    write_dashes(f)
    f = "TEXT_OUTPUT/MH_text_output.txt"
    curren_time <- Sys.time()
    message_02 <- paste("The file was created at", curren_time)
    write(message_02, file= f, append=TRUE)
    empty_line(f)
    write_dashes(f)
}
#####################################################################
wrtie_message <- function(msg, f) {
    M <- msg
    write(M, file= f, append=TRUE)
}
#####################################################################

wrtie_created_new_var <- function(new_var, f) {
    write_dashes(f)
    msg <- paste("Created new variable: ", new_var)
    write(msg, file= f, append=TRUE)
}

#################################################################
write_basic_statisitcs <- function(var,df, f) {
    len <- length(MH[[var]])
    uni <- unique(MH[[var]])
    msg <- paste("\nLenght is: ", len, "\nUniques values: ", uni)
    write(msg, file= f, append=TRUE)
}
##############################################################


