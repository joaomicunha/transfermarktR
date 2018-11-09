



#' get_transfers
#'
#' This function scrappes a transfermarkt webpage to collect a data-frame table with all the transfers for a particular league, season and transfer window.
#'
#' @param url The url for the relevant page to scrape. Example: 'https://www.transfermarkt.co.uk/premier-league/sommertransfers/wettbewerb/GB1'
#' @param season The season year.
#' @param include_end_of_loans Boolean value for whether to include or not players returning from loans in the results. Defaults to TRUE.
#' @param include_retired Boolean value for whether to include or not players that retired. Defaults to TRUE.
#' @param include_without_club Boolean value for whether to include or not players without club in the results. Defaults to TRUE.
#'
#' @return A data-frame where each row consists of a player transfer. The columns returned are From, To, Player, Age, Position, Position_Code, Market_Value, Market_Value_new, Fee, Fee_new, Transfer_Type, Transfer_Direction, Club, Club_Icon, League, Season_Scrapped, Date_Scrapped.
#'
#' @import magrittr
#' @export
#'
#' @examples
#' \dontrun{
#' tranfers_premierLeague = get_TransfersTransferMarket(url = "https://www.transfermarkt.co.uk/premier-league/sommertransfers/wettbewerb/GB1",
#'                                      season = "2018",
#'                                      include_end_of_loans = TRUE,
#'                                      include_retired = TRUE,
#'                                      include_without_club = TRUE
#'                                      )
#'
#'
#'}


get_transfers = function(url, season, include_end_of_loans = TRUE, include_retired = TRUE, include_without_club = TRUE){

  #Append the season element to the url:
  url_final = paste0(url,"/plus/?saison_id=", season)

  #Extract League Name:
  league_name = url_final %>%
    read_html() %>%
    xml_find_all("//h1[@class = 'spielername-profil']") %>%
    xml_text() %>%
    trimws("b")

  #Create a list made of tables/data-frames on the transfer summary webpage:
  all_tables_in_out_transfers = url_final %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//div[@class = 'large-8 columns']//div[@class = 'box']") %>%
    tail(length(.)-3) %>%
    lapply(function(team){

      team %>%
        html_nodes("table") %>%
        html_table(trim = TRUE) %>%

        #For each of the tables we standardize and clean the columns and names to be able to simultaneously use the Out and In table:
        purrr::map_df(function(table){

          transfer_direction = ifelse("Out" %in% colnames(table), "Out", "In")
          colnames(table) = c("Player", "Age", "Nationality", "Position", "Position_Code", "Market_Value", "nada", "To_From", "Fee")

          table_club = table %>%
            mutate_all(funs(ifelse(. == "-" | . == "?", NA, .))) %>%
            mutate(Market_Value_new = CleanFee(Market_Value),
                   Fee_new = CleanFee(Fee),
                   Transfer_Type = ifelse(grepl(x = Fee, pattern = "loan", ignore.case = TRUE), "Loan", "Transfer"),
                   Player = CleanDupyStrings(Player),
                   Transfer_Direction = transfer_direction,
                   League = league_name,
                   Season_Scrapped = paste0(as.integer(season)-1, "/", season),
                   Date_Scrapped = Sys.Date()) %>%
            select(-Nationality, -nada)

          #Filter out players returning from loan:
          if(include_retired){
            table_club = table_club
          }else{
            table_club = table_club %>%
              filter(!grepl(x = Fee, pattern = "End of loan"))
          }

          #Filter out players retired
          if(include_retired){
            table_club = table_club
          }else{
            table_club = table_club %>%
              filter(To_From != "Retired")
          }

          #Filter out players without club
          if(include_without_club){
            table_club = table_club
          }else{
            table_club = table_club %>%
              filter(To_From != "Without Club")
          }

        })

    })


  clubs_header = url_final %>%
    read_html() %>%
    xml_find_all("//div[@class = 'large-8 columns']//div[@class = 'box']//div[@class = 'table-header']//a//img")

  clubs = clubs_header %>%
    xml_attr("alt")

  clubs_icons = clubs_header %>%
    xml_attr("src")

  #Adding the club name to each data-frame:

  purrr::map_df(seq_along(clubs), function(i){

    all_tables_in_out_transfers[[i]] %>%
      mutate(Club = clubs[i],
             Club_Icon = clubs_icons[i],
             From = ifelse(Transfer_Direction == "In", To_From, Club),
             To = ifelse(Transfer_Direction == "In", Club, To_From)) %>%
      select(From, To, Player, Age, Position, Position_Code, Market_Value, Market_Value_new, Fee, Fee_new, Transfer_Type, Transfer_Direction, Club, Club_Icon, League, Season_Scrapped, Date_Scrapped)


  })


}
