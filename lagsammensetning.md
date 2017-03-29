Eliteserien Fantasy Football med R
================

Hva gjør du når kjentfolk inviterer deg til Eliteserien Fantasy Football, når Solskjær var stor i Norge sist du interesserte deg for norsk ligafotball? Prøver å analysere deg fram til noe fornuftig, selvsagt.

Målet med spillet er å maksimere antallet poeng, ved å velge spillerne som gir deg flest mulig poeng totalt. Spillere mottar poeng utifra spilletid, mål scort, målgivende pasning, og for å holde nullen. Forsvarsspillere får flere poeng for å holde nullen og score mål. Kort, selvmål, straffemiss og mange baklengsmål gir minuspoeng. Se poengsystemet på [hjelpesida](https://fantasy.vg.no/a/help) for mer info.

Første utfordring er å sette sammen et fornuftig lag. En må velge 15 spillere (fordelt på 2 keepere, 5 forsvarsspillere, 5 midtbanespillere og tre angrepsspillere), verdien kan ikke overstige 100 fantasimillioner (fantasillioner?), og en kan maks velge tre spillere fra et enkelt lag. Alt [i følge reglene](https://fantasy.vg.no/a/help).

Det vil antakeligvis være strategiske valg involvert i hvilke deler av laget en prioriterer høyest - gir det flere poeng å ha en solid keeper og et solid forsvar, eller gode angrepsspillere? Er det bedre med et par dyre spillere og mange billige, eller at alle er omlag jevngode? Er det bedre med dyre enkeltspillere fra middelmådige lag eller middels spillere fra gode lag? Er et godt lag et lag som scorer mange mål eller slipper inn få (hvis den forskjellen gir mening i eliteserien)?

Uansett - dette er et opimeringsproblem med diverse begresninger. Så hvis jeg kan finne datamateriale over alle spillere, deres fantasipengeverdi, posisjon, lagtilhørighet, popularitet og track record for 2016, bør det gå an å sette sammen noe som er ok - eller bedre enn rein gjetning, i hvert fall.

Datagrunnlaget
--------------

Heldigvis liger datagrunnlaget VG bruker tilgjengelig i json-format i et API (?), og kan hentes med GET.

Vil VG protestere på slik bruk av dataene? Dette er jo ikke dokumentert noe sted, så vidt jeg ser. Terms of Service omtaler at spillet ikke kan kopieres, men det står ingenting om datagrunnlaget. Siden hver skifting av hvilke spillere som vises utløser et nytt slikt kall, bør ikke et par kall fra denne algoritmen være feil.

Hver enkelt spillers kampoppsett ligger også tilgjengelig, men ettersom henting av alle disse dataene kan være litt mer krevende (og gi et litt flerdimensjonert datasett), så det lar jeg ligge inntil videre.

Dette ga ved første nedlasting i mars et datasett på 400 observasjoner og 50 variabler. Dataene tyder på at en av spillerne er solgt ut av Norge, og derfor ikke lenger er tilgjengelig. Så dette er dermed hele settet av spillere.

Et par dager senere ga det 402 observasjoner - datasettet oppdateres tydeligvis av og til. Derfor la jeg til en dato-variabel for å holde oversikt.

``` r
#de første observasjonene og variabelliste
glimpse(df)
```

    ## Observations: 406
    ## Variables: 51
    ## $ id                           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11...
    ## $ photo                        <chr> "62022.jpg", "124186.jpg", "21865...
    ## $ web_name                     <chr> "Lie", "Heigre", "Sodergren", "Ra...
    ## $ team_code                    <int> 2745, 2745, 2745, 2745, 2745, 274...
    ## $ status                       <chr> "a", "a", "a", "a", "a", "a", "a"...
    ## $ code                         <int> 62022, 124186, 218658, 52335, 640...
    ## $ first_name                   <chr> "Andreas", "Pal", "Sondre", "Kaj"...
    ## $ second_name                  <chr> "Lie", "Vestly Heigre", "Sodergre...
    ## $ squad_number                 <int> 1, NA, 32, NA, 5, 2, 4, 22, 3, 37...
    ## $ news                         <chr> "", "", "", "", "", "", "", "", "...
    ## $ now_cost                     <int> 45, 45, 40, 50, 45, 55, 45, 45, 5...
    ## $ chance_of_playing_this_round <lgl> NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ chance_of_playing_next_round <lgl> NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ value_form                   <chr> "0.0", "0.0", "0.0", "0.0", "0.0"...
    ## $ value_season                 <chr> "0.0", "0.0", "0.0", "0.0", "0.0"...
    ## $ cost_change_start            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ cost_change_event            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ cost_change_start_fall       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ cost_change_event_fall       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ in_dreamteam                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE...
    ## $ dreamteam_count              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ selected_by_percent          <chr> "5.0", "0.7", "3.8", "1.9", "2.4"...
    ## $ form                         <chr> "0.0", "0.0", "0.0", "0.0", "0.0"...
    ## $ transfers_out                <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ transfers_in                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ transfers_out_event          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ transfers_in_event           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ loans_in                     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ loans_out                    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ loaned_in                    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ loaned_out                   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ total_points                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ event_points                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ points_per_game              <chr> "0.0", "0.0", "0.0", "0.0", "0.0"...
    ## $ ep_this                      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ ep_next                      <chr> "2.0", "2.0", "1.0", "2.0", "2.0"...
    ## $ special                      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE...
    ## $ minutes                      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ goals_scored                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ assists                      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ clean_sheets                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ goals_conceded               <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ penalties_saved              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ penalties_missed             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ yellow_cards                 <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ red_cards                    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ saves                        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ bonus                        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ element_type                 <int> 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, ...
    ## $ team                         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ dato                         <date> 2017-03-29, 2017-03-29, 2017-03-...

Blant variablene ser følgende ut til å være relevante

-   id-variabel fra 1 til 400 (men også en code med 400 verdier)
-   en team\_code og en team, som med 16 nivåer bør tilsvare lagene.
-   now\_cost som bør være nåkostnad
-   element\_type som ser ut til å være posisjon
-   status som koder hvorvidt en spiller er relevant eller ikke, og er skadet
-   selected\_by\_percent

``` r
#datautvelgelse og grunnleggende omkoding

#velger ut variabler relevant for å velge et lag her. filtrerer vekk status==u og i - vil ikke ha skadede spillere blant de første femten fra start
temp = select(filter(df,status=="a"|status=="d"),id,web_name,first_name,second_name,team,element_type,now_cost, selected_by_percent,status)

#konverterer tall
temp$selected_by_percent = parse_number(temp$selected_by_percent)

#lager et fullt navn
temp$navn = paste(temp$first_name,temp$second_name)

#koder om posisjonskode til posisjonsnavn
# 1 = keepere, 2 = forsvarsspillere, 3 = midtbanespillere, 4 = angrep
temp$posisjon = temp$element_type
temp$posisjon = factor(temp$element_type,labels=c("Keeper","Forsvar","Midtbane","Angrep"))
table(levels(as.factor(temp$posisjon)),levels(as.factor(temp$element_type)))
```

    ##           
    ##            1 2 3 4
    ##   Angrep   0 0 0 1
    ##   Forsvar  0 1 0 0
    ##   Keeper   1 0 0 0
    ##   Midtbane 0 0 1 0

``` r
#Koder om lagkode til lagnavn
temp$team_navn = temp$team
temp$team_navn = factor(temp$team,labels=c("AAFK","BRA","FKH","KBK","LSK","MOL","ODD","RBK","SAN","SO8","SOG","STB","SIF","TIL","VIF","VIK"))
table(levels(as.factor(temp$team)),levels(as.factor(temp$team_navn)))
```

    ##     
    ##      AAFK BRA FKH KBK LSK MOL ODD RBK SAN SIF SO8 SOG STB TIL VIF VIK
    ##   1     1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   10    0   0   0   0   0   0   0   0   0   0   1   0   0   0   0   0
    ##   11    0   0   0   0   0   0   0   0   0   0   0   1   0   0   0   0
    ##   12    0   0   0   0   0   0   0   0   0   0   0   0   1   0   0   0
    ##   13    0   0   0   0   0   0   0   0   0   1   0   0   0   0   0   0
    ##   14    0   0   0   0   0   0   0   0   0   0   0   0   0   1   0   0
    ##   15    0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0
    ##   16    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1
    ##   2     0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   3     0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0
    ##   4     0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0
    ##   5     0   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0
    ##   6     0   0   0   0   0   1   0   0   0   0   0   0   0   0   0   0
    ##   7     0   0   0   0   0   0   1   0   0   0   0   0   0   0   0   0
    ##   8     0   0   0   0   0   0   0   1   0   0   0   0   0   0   0   0
    ##   9     0   0   0   0   0   0   0   0   1   0   0   0   0   0   0   0

Anna nyttig for framtidige analyser inkluderer ulike variabler for endring av verdi, pris, poeng, poeng per kamp og en streng med nyheter. Flesteparten av disse er enda ikke i bruk.

### Data om poeng i 2016

Litt googling leder meg fram til [dette foruminnlegget](http://vgd.no/sport/fotball-spill/tema/1836871/tittel/vgd-eliteserien-fantasy-17/innlegg/45924375/), der forumdeltakeren Fpl\_star har laget et datasett på 2016-data for 140 spillere. Dessverre får jeg ikke til å full\_join dette sammen med det øvrige datasettet p.g.a. noe krøll med karakterene, så det må litt mekking til i excel før det kan merges inn.

I tillegg til info jeg allerede har, gir datasettet info om 137 spilleres kamper, mål, assist og cleen sheets i 2016, hvor mange poeng dette hadde gitt hver for seg og totalt, samt beregning av hvor mange poeng per kamp dette tilsvarer og hvor mange poeng per fantasimillion dette tilsvarer.

Ettersom det bare er et subset av alle spillerne som har fått denne informasjonen, og siden Fpl\_star ikke ser ut til å forklare hvorfor han har tatt disse spillerne, må jeg ta en titt for å se om dette er et tilfeldig utvalg.

| as.factor(mer\_info) |  andel\_keepere|  andel\_forsvar|  andel\_midtbane|  andel\_angrep|
|:---------------------|---------------:|---------------:|----------------:|--------------:|
| ikke-vg-data         |       0.1616541|       0.3383459|        0.3421053|      0.1578947|
| vg-data              |       0.0000000|       0.3138686|        0.4744526|      0.2116788|

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-9-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-9-2.png)

Utvalget ser ikke veldig representativt ut. Som vi ser her er fordelinga av spillere i datasettet fra Fpl\_star ganske annerledes fra hele datasettet: det er ingen keepere, om lag samme andel forsvarere, flere angripere og langt flere midtbanespillere. Spillerne er langt jevnere i verdi, noe som særlig gjelder midtbanespillerne - det er langt færre av de lavest prisede. Det er også langt færre av de minst valgte spillerne som har poengdata. Det er allikevel mer informasjon enn vi hadde uten - så får vi se hva vi gjør med det.

### Datautforsking av spillerne

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-11-1.png)

Det tydeligste mønsteret herifra er at spennet i priser øker med offensiviteten (hvis det er et ord) til spillerne - spisser har større variasjon og koster mer enn midtbanespillere, midtbanespillere mer enn forsvarsspillere og forsvarsspillere mer enn keepere.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-12-1.png)

Et tilsvarende mønster ser ikke ut til å være i valgene av spillere - det ser ut til at det er flere forsvarsspillere og midtbanespillere som er valgt av færre, og noen valg av angrepsspiller som nesten alle har gjort.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 266 rows containing non-finite values (stat_bin).

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-13-1.png)

Hvordan er poengene fordelt? Ganske jevnt og nesten klokkeforma, faktisk, særlig for Midtbanespillerne (som utgjør nesten halvparten av spillerne vi har informasjon om). Kanskje litt overraskende sett opp imot prisbildet over, så er det særlig forsvarsspillere som hanker inn flest poeng, fulgt av midtbane og angrep.

    ## Warning: Ignoring unknown parameters: method

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-14-1.png)

    ## 
    ## Call:
    ## lm(formula = selected_by_percent ~ now_cost, data = temp)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -10.290  -2.288  -1.045   0.598  33.782 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -8.84242    1.06395  -8.311 1.49e-15 ***
    ## now_cost     0.22862    0.01879  12.164  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.114 on 401 degrees of freedom
    ## Multiple R-squared:  0.2695, Adjusted R-squared:  0.2677 
    ## F-statistic:   148 on 1 and 401 DF,  p-value: < 2.2e-16

Når det gjelder samvariasjon mellom popularitet og pris, ser vi at denne stort sett følger en lineær trend - jo høyere pris, jo høyere popularitet. Det er imidlertid flere spillere som ligger over (og under), noe som kan bety at de er bedre enn prisen tilsier, eller at fantasy-deltakerne overvurderer dem. Sagt på en anna måte: innafor hver priskategori er det stor variasjon i hvor populære de enkelte spillerne er, noe som ikke forklares av pris alene.

    ## `geom_smooth()` using method = 'loess'

    ## Warning: Removed 266 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 266 rows containing missing values (geom_point).

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-15-1.png)

    ## `geom_smooth()` using method = 'loess'

    ## Warning: Removed 266 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 266 rows containing missing values (geom_point).

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-15-2.png)

    ## 
    ## Call:
    ## lm(formula = Poeng ~ selected_by_percent + now_cost, data = temp)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -70.53 -18.37   3.76  20.93  49.13 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         59.74005   10.15986   5.880  3.1e-08 ***
    ## selected_by_percent  1.46403    0.36821   3.976 0.000114 ***
    ## now_cost             0.08915    0.16906   0.527 0.598821    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.14 on 134 degrees of freedom
    ##   (266 observations deleted due to missingness)
    ## Multiple R-squared:  0.1515, Adjusted R-squared:  0.1388 
    ## F-statistic: 11.96 on 2 and 134 DF,  p-value: 1.665e-05

Det er også (i hovedsak) en positiv sammenheng mellom popularitet og beregna poeng for 2016, og mellom nåkostnad og beregna poeng: jo høyere pris og jo høyere popularitet, jo flere poeng ville spillerne ha sanka i 2016. (For de 137 spillerne vi har data på). Lineær regresjon viser at sammenhengen er sterkest med popularitetsindikatoren, og at ingen av variablene forklarer mye av variasjonen i poengsummen. Sagt på en anna måte: det er ikke sikkert poengsummen for 2016 har vært viktig for popularitet og prissetting for årets sesong.

På den ene sida er det litt urovekkende for modelleringa her - hvis kostnad og popularitet i liten grad forutsier poeng, så ligger jeg tynt an. Det at de andre dataanalyseeksemplene jeg har sett har vært poengmaksimeringsforsøk gir også dårlige vibber. På den andre sida er poengene beregna for 2016 - så kanskje ekspertene og folk flest vet noe om årets sesong som Poengene ikke tar høyde for?

Indikator for dyktighet
-----------------------

Siden jeg ikke aner, og siden jeg har lite data å se på tidligere prestasjoner med, tenker jeg at i første omgang så bør:

-   prisen være en slags ekspertvurdering av dyktighet (noe VG selv sier),
-   hvor mange som har valgt en spiller være folk-flests vurdering av dyktighet,
-   fantasy-poeng for 2016 gi en indikasjon på potensiale,
-   et lags gjennomsnittlige dyktighet påvirke en enkeltsspillers poengpotensial.

### Pris og popularitet

Kan det være fordeler å hente ved å kombinere prisen - som proxy for ekspert-vurderinger - og populariteten, som proxy for folks vurderinger? Dette forutsetter i så fall litt normalisering av variablene, slik at de ligger på samme skala.

Siden antallet spillere på hver posisjon er satt, skulle en kanskje mene at det gir mening å normaliserer innafor hver spillerkategori - en kan uansett ikke fylle opp laget med dyre angrepsspillere. På den måten ville vi vite (forhåpentligvis) noe om hvem som er den beste keeperen, men mindre om hvor mye bedre en angrepsspiller er enn en keeper). På den andre siden skal vi jo ha det beste totale laget - og da må optimeringsalgoritmen kunne vurdere om det gir flere potensielle poeng å kjøpe dyrere forsvarsspillere eller dyrere angrepsspillere. Derfor normaliserer vi mot totalen.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-16-1.png)

### Poeng fra 2016

Nå gjelder det og også bake inn poengene fra 2016. Hva gjør vi spillerne uten poeng? Lar vi dem stå uten poeng, gir vi dem medianen eller bruker vi andre estimeringsteknikker?

Siden jeg delvis gjør dette for å teste ut div. estimeringsteknikker bruker jeg et decision tree. Ikke egentlig en god grunn for å bruke en metode - men.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-17-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-17-2.png)

    ## `geom_smooth()` using method = 'loess'

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-17-3.png)

Dette ser litt pussig ut, men ok. Vi slenger det inn i verdi-indikatoren.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-18-1.png)

### Lagstyrke

Videre kan det også være verdt å se på om spillere på lag som vurderes som sterkere bør få ekstra poeng. Vi ser i hvert fall at det er forskjell på kostnadsfordeling og popularitet blant lagene. For å få det enda tydeligere fram beregner jeg en sum og et gjennomsnitt av indikatoren vi til nå har beregna.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-19-1.png)

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-19-2.png)

| team\_navn |       mean|       sum|
|:-----------|----------:|---------:|
| RBK        |  0.2788592|  6.971480|
| ODD        |  0.1900910|  4.942365|
| BRA        |  0.1775711|  4.084136|
| MOL        |  0.1747365|  4.717885|
| VIF        |  0.1490922|  4.025490|
| SIF        |  0.1472265|  4.122341|
| LSK        |  0.1097600|  2.524480|
| VIK        |  0.1088610|  2.503803|
| FKH        |  0.1043280|  2.608200|
| SO8        |  0.1010362|  2.525906|
| AAFK       |  0.0908877|  2.272193|
| TIL        |  0.0879357|  2.110457|
| STB        |  0.0756730|  1.967499|
| KBK        |  0.0747387|  2.017944|
| SOG        |  0.0730159|  2.044444|
| SAN        |  0.0565487|  1.187522|

Jeg lar gjennomsnittet telle inn i totalindikatoren, og lar den telle 20 %. Kostnad og popularitet veier allerede 30 % hver, og (estimert) poengsum for 2016 veier 20 %.

Utvelgelse av laget
-------------------

Heldigvis har Premier League Fantasy Football vært ei greie ganske lenge, slik at det ligger en del [pekere](https://llimllib.github.io/fantasypl/) \[og andres forsøk\] (<http://pena.lt/y/2014/07/24/mathematically-optimising-fantasy-football-teams/>) rundt. Analysen er basert på slike ting funnet på nettet.

For å løse dette, ser jeg på det som et lineært maksimeringsproblem med begrensninger - som kan løses med lpSolve-pakken.

``` r
# OBJECTIVE FUNCTION: popularitet, pris og lagstyrke
# antar at det er dette som skal maksimeres (eller minimeres) - 
f.obj <- temp$verdi_teamjustert

x <- lp ("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)
x
```

    ## Success: the objective function is 7.174593

``` r
kable(temp[which(x$solution==1),c(3,11,13,8,9,26,10,12,37)])
```

|   id| navn                     | team\_navn |  now\_cost|  selected\_by\_percent|      Poeng| status | posisjon |  verdi\_teamjustert|
|----:|:-------------------------|:-----------|----------:|----------------------:|----------:|:-------|:---------|-------------------:|
|   26| Piotr Leciejewski        | BRA        |         55|                   22.1|  105.75000| a      | Keeper   |           0.4455205|
|   29| Amin Nouri               | BRA        |         55|                    7.2|  117.00000| a      | Forsvar  |           0.3708425|
|   64| Frederik Gytkjaer        | FKH        |         75|                   11.0|  103.16667| a      | Angrep   |           0.4024772|
|  120| Ruben Gabrielsen         | MOL        |         60|                   15.4|  105.75000| a      | Forsvar  |           0.4039475|
|  129| Babacar Sarr             | MOL        |         50|                    4.9|   64.00000| a      | Midtbane |           0.2470309|
|  143| Etzaz Hussain            | MOL        |         60|                   23.0|  105.75000| a      | Midtbane |           0.4500081|
|  144| Sondre Rossbach          | ODD        |         55|                   25.8|  103.16667| a      | Keeper   |           0.4713259|
|  146| Espen Ruud               | ODD        |         65|                   39.8|  151.00000| a      | Forsvar  |           0.6579037|
|  154| Ardian Gashi             | ODD        |         45|                    8.9|   71.47059| a      | Midtbane |           0.2895852|
|  181| Mike Jensen              | RBK        |        105|                   26.1|  118.00000| a      | Midtbane |           0.7417602|
|  184| Fredrik Midtsjo          | RBK        |         85|                   25.1|  107.00000| a      | Midtbane |           0.6498336|
|  194| Nicklas Bendtner         | RBK        |        125|                   49.5|  103.16667| a      | Angrep   |           0.9335648|
|  258| Fredrik Flo              | SOG        |         45|                    5.2|   85.78947| a      | Angrep   |           0.2170253|
|  298| Jonathan Parr            | SIF        |         60|                   20.1|  104.00000| a      | Forsvar  |           0.4116942|
|  299| Lars-Christopher Vilsvik | SIF        |         60|                   26.9|  125.00000| a      | Forsvar  |           0.4820729|

``` r
temp$solution = x$solution
```

### Vurdering

![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-24-1.png)![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-24-2.png)![](lagsammensetning_files/figure-markdown_github/unnamed-chunk-24-3.png)

Jeg tar sjansen på å velge laget som er beregna ut i fra popularitet, pris, poeng og lag. Antakeligvis kunne jeg brukt *mye* mer tid på å se på følsomhet for normalisering og ulike løsninger.

``` r
lag_start = filter(temp,solution==1)
data_start = temp
write.csv2(lag_start,"data/startlag.csv",row.names=F)
write.csv2(lag_start,"data/datagrunnlag_start.csv",row.names=F)
```
