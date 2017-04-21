#data-henting

#funksjon som tar hvilke data som skal returneres som argument, default spillere
#returnerer en data.frame, ikke selve objektet
#endepunkter dokumentert på https://github.com/bobbymond/FantasyPremierLeagueAPI
#eliteseriens API følger samme mønster

datagrabber = function(x = "spillere"){
        library(httr)
        library(dplyr)
        library(jsonlite)
        library(readr)
        if(x=="spillere"){
                temp_json = GET("https://fantasy.vg.no/drf/elements/")
                stop_for_status(temp_json)
                df = fromJSON(content(temp_json,"text",encoding="UTF-8"))
                #bruker Sys.Date, men usikker på hvor ofte dataene oppdateres.
                df$dato = Sys.Date()
                #fikser noen enkle omkodinger - bør sjekkes for konsistens?
                #konverterer tall
                df$selected_by_percent = parse_number(df$selected_by_percent)
                #lager et fullt navn
                df$navn = paste(df$first_name,df$second_name)
                #koder om posisjonskode til posisjonsnavn: 1 = keepere, 2 = forsvarsspillere, 3 = midtbanespillere, 4 = angrep
                df$posisjon = df$element_type
                df$posisjon = factor(df$element_type,labels=c("Keeper","Forsvar","Midtbane","Angrep"))
                #Koder om lagkode til lagnavn
                df$team_navn = df$team
                df$team_navn = factor(df$team,labels=c("AAFK","BRA","FKH","KBK","LSK","MOL","ODD","RBK","SAN","SO8","SOG","STB","SIF","TIL","VIF","VIK"))
                #lagrer en lokal kopi av dataene
                write.csv2(df,paste0("data/spillerdata_",Sys.Date(),".csv"),row.names=F)
                #returnerer df
                return(df)
        }
        if(x=="spillere_poeng_kamp"){
                #sjekker først hvor mange spillere det er
                temp_json = GET("https://fantasy.vg.no/drf/elements/")
                stop_for_status(temp_json)
                temp_df = fromJSON(content(temp_json,"text",encoding="UTF-8"))
                data_df =data.frame()
                for(i in as.vector(temp_df$id)){
                        Sys.sleep(4)
                        temp_json = GET(paste0("https://fantasy.vg.no/drf/element-summary/",i))
                        stop_for_status(temp_json)
                        temp_df = fromJSON(content(temp_json,"text",encoding="UTF-8"))
                        #hva er det jeg evt. trenger herifra? poeng per kamp? verdi ved kamp? tror kanskje det?
                        #dette ligger i history
                        #for prediksjoner kan jeg prøve å koble fixtures på history?
                        temp_df = temp_df$history
                        temp_df$id_player=i
                        data_df = bind_rows(data_df,temp_df)
                }
                #lagrer en lokal kopi av dataene
                #bruker Sys.Date, men usikker på hvor ofte dataene oppdateres.
                data_df$dato = Sys.Date()
                write.csv2(data_df,paste0("data/spillere_poeng_",Sys.Date(),".csv"),row.names=F)
                warning("Denne returnerer kun history, ikke framtidige fixtures eller summaries")
                return(data_df)
        }
        if(x=="spillere_alle"){
                temp_json = GET("https://fantasy.vg.no/drf/bootstrap-static")
                stop_for_status(temp_json)
                temp_df = fromJSON(content(temp_json,"text",encoding="UTF-8"))
                #gir en liste på 7: 1 faser (events delt inn i mnd), 2 elements (alle spillere, samme som elements over), 3 game settings (div settings), 4 total players (antallet irl-spillere), 5 teams (info om lagene), 6 element types (posisjoner), 7 events (runder)
                #kunne evt. brukt dette for å koble posisjoner og lagnavn - men jobben er jo allerede gjort over.
                warning("Liste - må bearbeides")
                return(temp_df)                
        }
        if(x=="lag"){
                #spilleres lag fra totallista
                # scrape the data - http://pena.lt/y/2014/07/24/mathematically-optimising-fantasy-football-teams/
                temp_json = GET("https://fantasy.vg.no/drf/bootstrap-static")
                stop_for_status(temp_json)
                temp_df = fromJSON(content(temp_json,"text",encoding="UTF-8"))
                antall_spillersider = floor(temp_df[[4]]/50)
                warning(paste0("Totalt ", temp_df[[4]]," lag. Skraping vil ta ca. ",ceiling((antall_spillersider*4)/60)," minutter"))
                data_df =data.frame()
                for(i in 1:antall_spillersider){
                        #Scrape responsibly kids, we don't want to ddos
                        Sys.sleep(4)
                        temp_df = GET(paste0("https://fantasy.vg.no/drf/leagues-classic-standings/319?phase=1&le-page=1&ls-page=",i))
                        stop_for_status(temp_df)
                        data_content = fromJSON(content(temp_df,"text",encoding="UTF-8"))
                        data_df = bind_rows(data_df,data_content[[3]][[3]])
                }
                #lagrer en lokal kopi av dataene
                #bruker Sys.Date, men usikker på hvor ofte dataene oppdateres.
                data_df$dato = Sys.Date()
                write.csv2(data_df,paste0("data/teamdata_",Sys.Date(),".csv"),row.names=F)
                warning("Husk å sjekke antallet rader")
                return(data_df)
        }
        warning("Nå skjedde det ingenting?")
}