require(parsermd)
require(stringr)

ast <- parse_rmd("README.md")

.parse_md_list <- function(astnode) {
    list_items_pos <- seq_along(astnode)[grepl("^-", astnode)]
    pos <- which(astnode == "")
    first_empty_pos <- min(pos[pos > max(list_items_pos)])
    itemlist <- list()
    i <- 0
    for (j in seq(min(list_items_pos), (first_empty_pos - 1))) {
        if (!grepl("^-", astnode[j])) {
            itemlist[[i]] <- append(itemlist[[i]], astnode[j])
        } else {
            i <- i + 1
            itemlist[[i]] <- astnode[j]
        }
    }
    return(itemlist)
}

.sort_by_year <- function(astnode) {
    list_items_pos <- seq_along(astnode)[grepl("^-", astnode)]
    pos <- which(astnode == "")
    first_empty_pos <- min(pos[pos > max(list_items_pos)])
    itemlist <- .parse_md_list(astnode)
    single_line_itemlist <- vapply(itemlist, paste, FUN.VALUE = character(1), collapse = " ", USE.NAMES = FALSE)
    ordering <- order(str_extract(single_line_itemlist, "\\(([0-9]{4})"))
    ordered_list_items <- itemlist[ordering]
    astnode[seq(min(list_items_pos), (first_empty_pos -1))] <- unlist(ordered_list_items)
    astnode
}

.sort_by_title <- function(astnode) {
    list_items_pos <- seq_along(astnode)[grepl("^-", astnode)]
    pos <- which(astnode == "")
    first_empty_pos <- min(pos[pos > max(list_items_pos)])
    itemlist <- .parse_md_list(astnode)
    single_line_itemlist <- vapply(itemlist, paste, FUN.VALUE = character(1), collapse = " ", USE.NAMES = FALSE)
    ordering <- order(single_line_itemlist)
    ordered_list_items <- itemlist[ordering]
    astnode[seq(min(list_items_pos), (first_empty_pos -1))] <- unlist(ordered_list_items)
    astnode
}

## books
ast[[7]] <- .sort_by_year(ast[[7]])

## conference
ast[[9]] <- .sort_by_title(ast[[9]])

## online course and material
ast[[13]] <- .sort_by_title(ast[[13]])

## videos
ast[[15]] <- .sort_by_title(ast[[15]])

## workshops
ast[[17]] <- .sort_by_title(ast[[17]])

## uni 19 TODO

## group 21 TODO

ast[[23]] <- .sort_by_title(ast[[23]])

ast[[25]] <- .sort_by_year(ast[[25]])

ast[[33]] <- .sort_by_title(ast[[33]])

ast[[35]] <- .sort_by_title(ast[[35]])

ast[[37]] <- .sort_by_title(ast[[37]])

writeLines(as_document(ast), "README.md")

