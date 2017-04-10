#data-henting

#funksjon som tar hvilke data som skal returneres som argument, default spillere
#returnerer en data.frame, ikke selve objektet

datagrabber = function(x = "spillere"){
        library(httr)
        library(dplyr)
        library(jsonlite)
        if(x=="spillere"){
                temp_json = GET("https://fantasy.vg.no/drf/elements/")
                stop_for_status(temp_json)
                df = fromJSON(content(temp_json,"text"))
                return(df)
        }
}

#kan jeg hente data direkte fra VGs nettside?
#endepunkter dokumentert på https://github.com/bobbymond/FantasyPremierLeagueAPI

library(httr)
library(dplyr)
library(jsonlite)

#dette ser ut til å hente ut informasjon om alle spillere
test_spillere = GET("https://fantasy.vg.no/drf/elements/")
status_code(test_spillere)
http_status(test_spillere)
headers(test_spillere)

#henter ut innholdet
parsed = content(test_spillere,"parsed")
str(parsed,1)
bin = content(test_spillere,"raw")
writeBin(bin,"test.json")

#dette ser ut til å hente ut et enkelt elements kampoppsett
test = GET("https://fantasy.vg.no/drf/element-summary/1")
status_code(test)
http_status(test)
headers(test)
parsed = content(test,"parsed")
str(parsed,3)

bin = content(test,"raw")
writeBin(bin,"test.txt")

#konvertere fra json til data-frame
library(jsonlite)
df = fromJSON(content(test_spillere,"text"))
df = fromJSON(parsed)

#lurer på om dette er et mer overordna element, som også har info om faser og fotballlag?
#kanskje dette kan knyttes sammen?
test_lag = GET("https://fantasy.vg.no/drf/bootstrap-static")
status_code(test_lag)
http_status(test_lag)
headers(test_lag)

#henter ut innholdet
df_lag = fromJSON(content(test_lag,"text"))
str(df_lag,max.level = 1)

df_lag$`total-players`

parsed = content(test_spillere,"parsed")
str(parsed,1)
bin = content(test_spillere,"raw")
writeBin(bin,"test.json")

#spilleres lag fra totallista
#utforsking tyder på at det 1169 sider med lag - 58 450 lag, hvis hver side har 50
#med Sys.sleep på 5 gir dette 1.5 timers arbeid.
# scrape the data - http://pena.lt/y/2014/07/24/mathematically-optimising-fantasy-football-teams/
data_df =data.frame()
for(i in 1:1170){
        # Scrape responsibly kids, we don't want to ddos
        Sys.sleep(5)
        data = GET(paste0("https://fantasy.vg.no/drf/leagues-classic-standings/319?phase=1&le-page=1&ls-page=",i))
        warn_for_status(data)
        data_content = fromJSON(content(data,"text"))
        data_df = bind_rows(data_df,data_content[[3]][[3]])
}


test = GET("https://fantasy.premierleague.com/drf/my-team/36536/")


