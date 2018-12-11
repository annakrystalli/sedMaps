#' Update selected varnames during extract mode
#'
#' @param selected last state of selected varnames
#' @param new new state of selected varname following last click.
#'
#' @return a vector of updated selected varnames. If `new` contains a new
#' `varname`, the `varname` is added to the end of `selected`. If a `varname`
#' has been removed in `new`, retuns `selected` with the `varname` removed.
#' @export
#'
#' @examples
update_selected_varnames <- function(selected = NULL, new){

    if(is.null(selected)){
        return(new)}
    if(is.null(new)){
        return(selected)}
    
    test_new <- new %in% selected
    if(all(test_new)){
        # add remove last variable
        selected[selected %in% new]
    }else{
        # add variable to end
        c(selected, new[!test_new])
    }
}