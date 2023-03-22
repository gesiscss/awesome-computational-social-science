require(parsermd)
require(stringr)

ast <- parse_rmd("README.md")

.sort_by_year <- function(astnode) {
    list_items_pos <- seq_along(astnode)[grepl("^-", astnode)]
    ordered_list_items <- astnode[list_items_pos][order(str_extract(astnode[list_items_pos], "\\(([0-9]{4})"))]
    astnode[list_items_pos] <- ordered_list_items
    astnode
}

.sort_by_title <- function(astnode) {
    list_items_pos <- seq_along(astnode)[grepl("^-", astnode)]
    ordered_list_items <- astnode[list_items_pos][order(astnode[list_items_pos])]
    astnode[list_items_pos] <- ordered_list_items
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
