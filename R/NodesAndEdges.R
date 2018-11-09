

#' NodesAndEdges (operational function)
#'
#' This function is used to format the transfer data into nodes and edges format to be supported by the visNetwork library.
#'
#' @param df A df
#' @param value_bins The number of bins. Defaults to 3.
#'


NodesAndEdges = function(df, value_bins = 3){

  colnames(df) = tolower(colnames(df))

  if(!("from" %in% colnames(df))){
    stop("'from' column has to be included in df")
  }else if(!("to" %in% colnames(df))){
    stop("'to' column has to be included in df")
  }

  nodes = data.frame(id = unique(c(unique(df$from), unique(df$to))),
                     # add labels on nodes
                     #shape = c("circularImage"),
                     label = unique(c(unique(df$from), unique(df$to))),
                     stringsAsFactors = FALSE) %>%
    dplyr::left_join(dplyr::distinct(df, club, club_icon), by = c("id" = "club")) %>%
    dplyr::rename(image = club_icon) %>%
    dplyr::mutate(
      image = ifelse(is.na(image), "", image),
      group = ifelse(id %in% unique(df$club), "League Club", "Other League"))


  edges = df %>%
    dplyr::mutate(arrows = "to",
           dashes = ifelse(transfer_type == "Transfer", FALSE, TRUE),
           value_stage = as.integer(cut(fee_new, breaks = value_bins, labels = (1:value_bins))),
           title = paste0("<p><b>Player: </b>", player,"<br><b>Age: </b>",age, "<br><b>Position: </b>", paste0(position, " (", position_code,")"),"<br><b>Transfer Fee: </b>",fee,"<br><b>Market Value: </b>", market_value, "</p>")) %>%
    dplyr::left_join(data.frame(value_stage = 1:value_bins, color = viridis::viridis(n = value_bins), stringsAsFactors = FALSE), by = c("value_stage")) %>%
    dplyr::select(from, to, arrows, dashes, color, title)

  #return:
  list(nodes = nodes, edges = edges)
}
