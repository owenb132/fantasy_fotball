Fantasy-eliteserie - modellevaluering
================

Ok, 1. april gjorde jeg siste justeringer av [laget mitt](https://github.com/gardenberg/fantasy_fotball/blob/master/lagsammensetning.md). Nå er de første kampene spilt.

Hva var laget mitt igjen? Jo:

``` r
kable(arrange(select(filter(data_start,solution==1),id,navn,posisjon,team_navn,verdi_teamjustert),posisjon,desc(verdi_teamjustert)),caption="Første 15 spillere")
```

|   id| navn                     | posisjon | team\_navn |  verdi\_teamjustert|
|----:|:-------------------------|:---------|:-----------|-------------------:|
|  194| Nicklas Bendtner         | Angrep   | RBK        |           0.9388889|
|  365| Mohammed Abdellaoue      | Angrep   | VIF        |           0.5254672|
|  258| Fredrik Flo              | Angrep   | SOG        |           0.2051946|
|  146| Espen Ruud               | Forsvar  | ODD        |           0.6502138|
|  299| Lars-Christopher Vilsvik | Forsvar  | SIF        |           0.5014311|
|  298| Jonathan Parr            | Forsvar  | SIF        |           0.4295948|
|   29| Amin Nouri               | Forsvar  | BRA        |           0.3526969|
|   31| Ruben Kristiansen        | Forsvar  | BRA        |           0.3318149|
|  169| André Hansen             | Keeper   | RBK        |           0.5861408|
|  144| Sondre Rossbach          | Keeper   | ODD        |           0.4651871|
|  184| Fredrik Midtsjø          | Midtbane | RBK        |           0.6913667|
|  132| Sander Svendsen          | Midtbane | MOL        |           0.4487115|
|  143| Etzaz Hussain            | Midtbane | MOL        |           0.3719081|
|   41| Sivert Heltne Nilsen     | Midtbane | BRA        |           0.2960077|
|  154| Ardian Gashi             | Midtbane | ODD        |           0.2828410|

Dataene med poengsummene til de nå ca. 61 000 lagene ser ut til å være tilgjengelig. Til nå har jeg bare klart å få dem ut 50 av gangen - dvs. at en frekk loop spør 1200 ganger, over 1.5 time for å unngå at VG får DDOs-følelse.

Her ser vi at med 34 poeng ligger jeg på 48 416 plass. Det er mange delte plasseringer, men det er altså noen tusen lagkonfigurasjoner som er bedre.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](modellevaluering_files/figure-markdown_github/unnamed-chunk-4-1.png)

Det trengs et par runder til før jeg vil begynne å se på måter å finne gode overganger på, men det kan være interessant å sette opp en kjapp vurdering av hvordan modellen for å finne det beste laget gjorde det. Verdi\_teamjustert var min indikator for poeng. Er fordelinga av den lik fordelinga av totalpoengene så langt? Ved å normalisere totalpoengsummen på samme vis som verdi-indikatoren kan vi sammenlikne fordelingene.

    ## No encoding supplied: defaulting to UTF-8.

![](modellevaluering_files/figure-markdown_github/unnamed-chunk-5-1.png)

Fordelinga er (etter første runde) ikke helt lik. Indikatoren heller mer mot høyre, mens totalpoengene er mye mer høyreskjev og har mange i den lave enden av fordelinga. Dette er kanskje naturlig - etter hvert vil enkelte formodentlig bevege seg mer mot høyre og høyere poengsummer.

Er det sammenheng mellom verdien jeg predikerte for hver enkelt spiller og poengene de til nå har samla inn? Verdi-indikatoren var en lineær modell av fire variabler. Totalpoeng-indikatoren kan ikke sies å være korrelert med input-faktorene på samme måte. Indikatoren på lagstyrke ser til og med ut til å være negativt korrelert med poengene.

``` r
modell_1 = lm(verdi_teamjustert~now_cost_norm+selected_norm+poeng_norm+verdi_meanteam_norm,data=df)
summary(modell_1)
```

    ## 
    ## Call:
    ## lm(formula = verdi_teamjustert ~ now_cost_norm + selected_norm + 
    ##     poeng_norm + verdi_meanteam_norm, data = df)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -8.251e-16 -2.324e-16  3.910e-18  2.233e-16  6.713e-16 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error    t value Pr(>|t|)    
    ## (Intercept)         -5.843e-17  4.310e-17 -1.356e+00    0.176    
    ## now_cost_norm        3.000e-01  1.204e-16  2.493e+15   <2e-16 ***
    ## selected_norm        3.000e-01  1.439e-16  2.084e+15   <2e-16 ***
    ## poeng_norm           2.000e-01  9.844e-17  2.032e+15   <2e-16 ***
    ## verdi_meanteam_norm  2.000e-01  7.661e-17  2.611e+15   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.013e-16 on 396 degrees of freedom
    ## Multiple R-squared:      1,  Adjusted R-squared:      1 
    ## F-statistic: 1.664e+31 on 4 and 396 DF,  p-value: < 2.2e-16

``` r
modell_2 = lm(Totalpoeng_norm~now_cost_norm+selected_norm+poeng_norm+verdi_meanteam_norm,data=df)
summary(modell_2)
```

    ## 
    ## Call:
    ## lm(formula = Totalpoeng_norm ~ now_cost_norm + selected_norm + 
    ##     poeng_norm + verdi_meanteam_norm, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.24391 -0.05511 -0.03505  0.02032  0.83895 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.08079    0.01748   4.621 5.18e-06 ***
    ## now_cost_norm        0.22157    0.04882   4.538 7.53e-06 ***
    ## selected_norm        0.23166    0.05839   3.968 8.62e-05 ***
    ## poeng_norm           0.03344    0.03993   0.838    0.403    
    ## verdi_meanteam_norm -0.03809    0.03108  -1.226    0.221    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1222 on 396 degrees of freedom
    ## Multiple R-squared:  0.1789, Adjusted R-squared:  0.1706 
    ## F-statistic: 21.57 on 4 and 396 DF,  p-value: 4.106e-16

``` r
qplot(verdi_teamjustert,Totalpoeng_norm,data=df,color=factor(solution),geom=c("point","smooth")) +
        geom_abline(intercept = 0, slope = 1)
```

    ## `geom_smooth()` using method = 'loess'

![](modellevaluering_files/figure-markdown_github/unnamed-chunk-7-1.png)

Det samme ser vi når vi plotter verdi-indikatoren mot totalpoengene. Dette ser, per 2. april, ikke spesielt lovende ut. Det er en viss positiv sammenheng, men ikke stor - og langt ifra en perfekt linje. I tråd med fordelinga vi så over, ser vi at de fleste har fått færre poeng enn jeg antok.

Hvordan vurdere løsninga fra lp\_solve opp mot andre løsninger? En måte ville vært å finne alle gyldige løsninger (alle gyldige kombinasjoner er kanskje et bedre navn på det - ikke bare de med høyest poengsum, men alle som oppfyller begrensningene), og sammenliknet disse på poeng. Enn så lenge har jeg ikke funnet en måte å liste ut alle gyldige kombinasjoner på, uten å kode masse - så det står vi over. I stedet ser vi på hva som nå ville vært det beste laget.

``` r
# OBJECTIVE FUNCTION: popularitet, pris og lagstyrke
# antar at det er dette som skal maksimeres (eller minimeres) - 
f.obj <- df_spillerdata$total_points

x <- lp ("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)
x
```

    ## Success: the objective function is 145

``` r
kable(arrange(df_spillerdata[which(x$solution==1),c(1,3,50,51,11,22,5,32)],element_type,desc(total_points)))
```

|   id| web\_name    |  element\_type|  team|  now\_cost| selected\_by\_percent | status |  total\_points|
|----:|:-------------|--------------:|-----:|----------:|:----------------------|:-------|--------------:|
|  342| Sandberg     |              1|    15|         50| 3.8                   | a      |              7|
|  169| Hansen       |              1|     8|         55| 30.8                  | a      |              6|
|  223| Rosted       |              2|    10|         56| 11.2                  | a      |             17|
|  348| Näsberg      |              2|    15|         50| 3.1                   | a      |             11|
|   33| Ròlantsson   |              2|     2|         60| 6.1                   | a      |              9|
|  323| Wangberg     |              2|    14|         50| 7.9                   | a      |              9|
|  127| Gregersen    |              2|     6|         50| 3.7                   | a      |              7|
|  181| Jensen       |              3|     8|        105| 26.0                  | a      |             12|
|  238| Zachariassen |              3|    10|         56| 7.8                   | a      |              9|
|  187| Jevtovic     |              3|     8|         76| 8.3                   | a      |              8|
|  307| Nguen        |              3|    13|         70| 9.1                   | a      |              7|
|  330| Pedersen     |              3|    14|         55| 5.5                   | a      |              6|
|  278| Omoijuanfo   |              4|    12|         70| 21.6                  | d      |             16|
|  319| Tagbajumi    |              4|    13|         75| 6.8                   | a      |             12|
|  368| Finne        |              4|    15|         90| 9.0                   | a      |              9|

``` r
df_spillerdata$solution = x$solution
```

Oppsummert
----------

Etter første runde ser det ikke ut til at modellen traff videre bra. Det kan derfor bli behov for å gjøre justeringer utover sesongen.
