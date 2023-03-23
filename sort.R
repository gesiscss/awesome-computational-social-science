require(parsermd)
require(stringr)

ast <- parse_rmd("README.md")

.parse_md_list <- function(astnode) {
    list_items_pos <- seq_along(astnode)[grepl("^- ", astnode)]
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
    list_items_pos <- seq_along(astnode)[grepl("^- ", astnode)]
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
    list_items_pos <- seq_along(astnode)[grepl("^- ", astnode)]
    pos <- which(astnode == "")
    first_empty_pos <- min(pos[pos > max(list_items_pos)])
    itemlist <- .parse_md_list(astnode)
    single_line_itemlist <- vapply(itemlist, paste, FUN.VALUE = character(1), collapse = " ", USE.NAMES = FALSE)
    ordering <- order(single_line_itemlist)
    ordered_list_items <- itemlist[ordering]
    astnode[seq(min(list_items_pos), (first_empty_pos -1))] <- unlist(ordered_list_items)
    astnode
}

.last2 <- function(x) {
    head(tail(x, n = 2), n = 1)
}

.sort_by_country <- function(astnode, city = FALSE) {
    list_items_pos <- seq_along(astnode)[grepl("^- ", astnode)]
    pos <- which(astnode == "")
    first_empty_pos <- min(pos[pos > max(list_items_pos)])
    itemlist <- .parse_md_list(astnode)
    single_line_itemlist <- vapply(itemlist, paste, FUN.VALUE = character(1), collapse = " ", USE.NAMES = FALSE)
    country <- str_trim(vapply(strsplit(single_line_itemlist, ", "), tail, character(1), 1))
    if (!city) {
        ordering <- order(country)
    } else {
        city <- str_trim(vapply(strsplit(single_line_itemlist, ", "), .last2, character(1)))
        ordering <- order(country, city)
    }
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

## uni
ast[[19]] <- .sort_by_country(ast[[19]])

## group

ast[[21]] <- .sort_by_country(ast[[21]], city = TRUE)


ast[[23]] <- .sort_by_title(ast[[23]])

ast[[25]] <- .sort_by_year(ast[[25]])

ast[[33]] <- .sort_by_title(ast[[33]])

ast[[35]] <- .sort_by_title(ast[[35]])

ast[[37]] <- .sort_by_title(ast[[37]])

## Kill the yaml
ast[[1]] <- NULL

content <- as_document(ast)
clean_content <- c()
space_counter <- 0

for (line in content) {
    if (line != "") {
        clean_content <- append(clean_content, line)
        space_counter <- 0
    } else {
        if (space_counter < 2) {
            clean_content <- append(clean_content, line)
            space_counter <- space_counter + 1
        } else {
            next
        }
    }
}

writeLines(clean_content, "README.md")
