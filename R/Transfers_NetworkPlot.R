

#' Transfers_NetworkPlot
#'
#' This function creates a Network plot (visNetwork) with the transfer data scrapped before.
#'
#' @param transfers_df A data-frame returned from get_transfers function.
#' @param transfer_fee_bins The number of bins to define from the Fee of the transfer. This is used to define the color of the links on the visualisation illustrating the value of a transaction. Defaults to 3.
#'
#' @return A visNetwork graph.
#'
#' @import magrittr
#' @export
#'
#'


Transfers_NetworkPlot = function(transfers_df, transfer_fee_bins = 3){

  nodes_edges_list = NodesAndEdges(df = transfers_df,
                                   value_bins = transfer_fee_bins)

  visNetwork(nodes = nodes_edges_list$nodes,
             edges = nodes_edges_list$edges,
             main = paste0(unique(finale$League), " Transfers (", unique(finale$Season_Scrapped), " Season)")) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") %>%
    visGroups(groupname = "League Club", shape = "image") %>%
    visGroups(groupname = "Other League", shape = "dot", color = "grey") %>%
    addFontAwesome()

}
