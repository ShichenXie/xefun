# vector to list 
c_list = function(x, name=TRUE) {
    lst = as.list(x)
    if (isTRUE(name)) lst = setNames(lst, x)
    return(lst)
}