#data-henting

#kan jeg hente data direkte fra VGs nettside?

library(httr)

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
