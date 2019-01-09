
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
#' tranfers_premierLeague = get_transfers(url = "https://www.transfermarkt.co.uk/premier-league/sommertransfers/wettbewerb/GB1",
#'                                      season = "2018",
#'                                      include_end_of_loans = TRUE,
#'                                      include_retired = TRUE,
#'                                      include_without_club = TRUE
#'                                      )
#'
#'
#'}


get_transfers = function(url, season = NULL, include_end_of_loans = TRUE, include_retired = TRUE, include_without_club = TRUE){

  #Append the season element to the url:
  url_final = paste0(url,"/plus/?saison_id=", ifelse(is.null(season), substr(Sys.Date(), 1,4), season))

  #Extract League Name:
  league_name = url_final %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//h1[@class = 'spielername-profil']") %>%
    xml2::xml_text() %>%
    trimws("b")

  #Create a list made of tables/data-frames on the transfer summary webpage:
  all_tables_in_out_transfers = url_final %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//div[@class = 'large-8 columns']//div[@class = 'box']") %>%
    tail(length(.)-3) %>%
    lapply(function(team){

      team %>%
        rvest::html_nodes("table") %>%
        rvest::html_table(trim = TRUE, fill = TRUE) %>%

        #For each of the tables we standardize and clean the columns and names to be able to simultaneously use the Out and In table:
        purrr::map_df(function(table){

          transfer_direction = ifelse("Out" %in% colnames(table), "Out", "In")
          colnames(table) = c("Player", "Age", "Nationality", "Position", "Position_Code", "Market_Value", "nada", "To_From", "Fee")

          table_club = table %>%
            dplyr::mutate_all(dplyr::funs(ifelse(. == "-" | . == "?", NA, .))) %>%
            dplyr::mutate(Market_Value_new = CleanFee(Market_Value),
                   Fee_new = CleanFee(Fee),
                   Transfer_Type = ifelse(grepl(x = Fee, pattern = "loan", ignore.case = TRUE), "Loan", "Transfer"),
                   Player = CleanDupyStrings(Player),
                   Age = as.character(Age),
                   Transfer_Direction = transfer_direction,
                   League = league_name,
                   Season_Scrapped = paste0(season, "/", as.integer(season)+1),
                   Date_Scrapped = Sys.Date()) %>%
            dplyr::select(-Nationality) %>%
            dplyr::filter(!(Player %in% c("No departures", "No arrivals")))

          #Filter out players returning from loan:
          if(include_retired){
            table_club = table_club
          }else{
            table_club = table_club %>%
              dplyr::filter(!grepl(x = Fee, pattern = "End of loan"))
          }

          #Filter out players retired
          if(include_retired){
            table_club = table_club
          }else{
            table_club = table_club %>%
              dplyr::filter(To_From != "Retired")
          }

          #Filter out players without club
          if(include_without_club){
            table_club = table_club
          }else{
            table_club = table_club %>%
              dplyr::filter(To_From != "Without Club")
          }

        })

    })


  clubs_header = url_final %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//div[@class = 'large-8 columns']//div[@class = 'box']//div[@class = 'table-header']//a//img")

  clubs = clubs_header %>%
    xml2::xml_attr("alt")

  clubs_icons = clubs_header %>%
    xml2::xml_attr("src")

  #Eliminate "Mail", "Twitter", "Facebook" related info that is being scrapped as club icons/names:
  clubs = setdiff(x = clubs, y = c("Mail", "Twitter", "Facebook"))
  clubs_icons = clubs_icons[!grepl(pattern = "mail|Twitter|Facebook", x = clubs_icons)]


  #Adding the club name to each data-frame:

  purrr::map_df(seq_along(clubs), function(i){

    all_tables_in_out_transfers[[i]] %>%
      dplyr::mutate(Club = clubs[i],
             Club_Icon = clubs_icons[i],
             From = ifelse(Transfer_Direction == "In", To_From, Club),
             To = ifelse(Transfer_Direction == "In", Club, To_From)) %>%
      dplyr::select(From, To, Player, Age, Position, Position_Code, Market_Value, Market_Value_new, Fee, Fee_new, Transfer_Type, Transfer_Direction, Club, Club_Icon, League, Season_Scrapped, Date_Scrapped)


  }) %>%
    dplyr::mutate(Age = as.integer(Age))


}
