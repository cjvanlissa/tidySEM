#dat <- mxGenerateData(mix4)
dat <- readRDS("c:/tmp/dat.RData")
names(dat) <- c("burdened", "trapped", "negaffect", "loneliness")
#res <- mx_profiles(dat, classes = 1:6)
#res <- readRDS("c:/tmp/res.rdata")
cp <- class_prob(res[[4]], "individual")
#dat <- readRDS("c:/tmp/dat.rdata")
dat$class <- cp$individual[,5]
res_final <- mx_profiles(dat, classes = 4)
res_final <- mx_switch_labels(res_final, param = "M[1,2]", decreasing = FALSE)
plot_bivariate(res_final)

dat$sex <- rbinom(nrow(dat), size = 1, prob = c(.75,.75, .85, .75)[dat$class])
dat$sex <- factor(dat$sex, levels = c(0,1), labels = c("male", "female"))
# table(dat$sexr, dat$class)/matrix(rep(table(dat$class), each = 2), nrow = 2, ncol = 4)
dat$sexpatient <- rbinom(nrow(dat), size = 1, prob = c(.53,.53, .59, .37)[dat$class])
dat$sexpatient <- factor(dat$sexpatient, levels = c(0,1), labels = c("male", "female"))
dat$cohabiting <- rbinom(nrow(dat), size = 1, prob = c(0.31,.60, .36, .70)[dat$class])

dat$distance <- rnorm(nrow(dat), mean = 36.03, sd = sqrt(2107.35))
dat$distance[dat$class == 2] <-
  rnorm(length(dat$distance[dat$class == 2]),
        mean = 25.53, sd = sqrt(1032.89))
dat$distance[dat$class == 3] <-
  rnorm(length(dat$distance[dat$class == 3]),
        mean = 30.65, sd = sqrt(803.45))
dat$distance[dat$class == 4] <-
  rnorm(length(dat$distance[dat$class == 4]),
        mean = 29.68, sd = sqrt(1567.42))
dat$distance <- scales::rescale(dat$distance, to = c(1, 300))
dat$distance[dat$cohabiting] <- 0
dat$freqvisit <- rnorm(nrow(dat), mean = 3.83, sd = sqrt(2.39))
dat$freqvisit[dat$class == 2] <-
  rnorm(length(dat$freqvisit[dat$class == 2]),
        mean = 4.51, sd = sqrt(.88))
dat$freqvisit[dat$class == 3] <-
  rnorm(length(dat$freqvisit[dat$class == 3]),
        mean = 4.13, sd = sqrt(1.48))
dat$freqvisit[dat$class == 4] <-
  rnorm(length(dat$freqvisit[dat$class == 4]),
        mean = 4.35, sd = sqrt(2.01))
dat$freqvisit <- as.integer(factor(cut(dat$freqvisit, 6)))
table(dat$freqvisit)

dat$relationship[dat$class == 1] <-
  sample(
    x = c("other", "family", "child", "parent", "partner"),
    size = sum(dat$class == 1),
    replace = TRUE,
    prob = c(0.872342509041332, 0.163728636259228, 0.113211492193707, 0.353880686089874, 0.241521694498523))
dat$relationship[dat$class == 2] <-
  sample(
    x = c("other", "family", "child", "parent", "partner"),
    size = sum(dat$class == 2),
    replace = TRUE,
    prob = c(0.954367041808942, 0.0890309254567818, 0.238957887843694, 0.175168058106366, 0.4512101704021))
dat$relationship[dat$class == 3] <-
  sample(
    x = c("other", "family", "child", "parent", "partner"),
    size = sum(dat$class == 3),
    replace = TRUE,
    prob = c(0.992612088696723, 0.158040790207195, 0.108624354249184, 0.447120898378048, 0.278826045862296))
dat$relationship[dat$class == 4] <-
  sample(
    x = c("other", "family", "child", "parent", "partner"),
    size = sum(dat$class == 4),
    replace = TRUE,
    prob = c(0.962685723146261, 0.0302530007494751, 0.31355284291551, 0.133032352071506, 0.48584752740977))

zegwaard_carecompass <- dat
usethis::use_data(zegwaard_carecompass)

Medemens Ondersteuner
Class 1: Lonely
Class 2: Balanced? Moderately lonely and burdened, not really trapped or negative affect
Class 3: Imbalanced? Moderately lonely and burdened, but otherwise similar to entrapped
Class 4: Entrapped

De medemens
Men is tevreden over de eigen rol als mantelzorger. De energie die in de ondersteuning wordt gestoken en de
energie die het oplevert zijn redelijk in evenwicht. Op momenten van crisis is de persoon niet altijd uit de
gedachten, maar de mantelzorger ligt er niet (vaak) wakker van.
- De ondersteuner
Het is logisch dat men voor de naaste is gaan zorgen. De zorg voor de naaste is in de afgelopen jaren ingepast
in het eigen leven en daarin een redelijk maar kwetsbaar evenwicht gevonden. De energie die in de
ondersteuning wordt gestoken en de energie die het oplevert zijn echter niet altijd in evenwicht.
- De eenzame mantelzorger
De zorg voor naaste wordt gezien als iets dat op het pad is gekomen en waarvan beide de gevolgen moeten
dragen. De mantelzorger is de hele dag in touw om alles zo goed mogelijk te organiseren. Men voelt zich niet
ongelukkig, maar wel eenzaam.
- De gevangen mantelzorger
Een leven zonder de ander kan men zich niet voorstellen, maar men voelt ook dat men verstrikt is geraakt in
de zorg. Op dit moment is het zwaar om de zorg vol te houden.
>> Wil je weten welke type past bij de mantelzorger? De dienstenbemiddelaar van ZuidZorg Extra is opgeleid
om met behulp van de welzijnsmeter deze rol te kunnen bepalen.

Class 1 is hte least trapped
De rode naasten ondersteunen de persoon vanuit liefde, genegenheid en plicht. Zij ondersteunen de ander vanuit loyaliteit en omdat de relatie de moeite waard gevonden wordt. Het geeft hen zelf echter weinig voldoening. Zij voelen zich een beetje nuttig. Ze proberen de situatie vooral te nemen zoals hij is. Zij voelen zich behoorlijk belast en zij ervaren geen vrijheid om te kunnen stoppen met de ondersteuning. Als ondersteuner vinden zij zichzelf maar matig geschikt. In de ondersteuning gaan zij het conflict met de persoon die ondersteunt wordt niet uit de weg. In het continueren van de ondersteuning staat deze rode naaste liever niet al te veel stil bij het feit of ze geschikt zijn (want dat is te emotioneel?). Het gevoel belast te zijn wordt daardoor wel versterkt. Ze voelen zich enigszins eenzaam ( in de relatie met de persoon voor wie zij zorgen) maar zij voelen zich niet eenzaam in contact met anderen. De rode naaste zegt steun te krijgen van anderen.
In de rode klasse zijn de zorgontvangers (sexr(eceiver?)  voor het merendeel vrouw. De rode naasten fiftyfifty man-vrouw uw en tussen de 60-65% van hen woont niet in 1 huis met de persoon die door hen gesteund wordt. De meeste behandeling komt vanuit de huisarts en er is niet nauwelijks ondersteuning vanuit de GGZ of woonbegeleiding. De ondersteuning die verre weg het meest door hen gegeven wordt is emotionele ondersteuning en de ander helpen bij het opbouwen en / of onderhouden van een eigen netwerk en het huishouden. Controle op medicatie inname in bijna de helft van de gevallen en bij bijna een kwart is er gebruik van middelen en zelfbeschadiging.  Ongeveer de helft van de naasten ondersteunen hun partner. Kinderen en ouders worden % gezien veel minder vaak door rood ondersteund.

                                          Diagnose en bijkomende zaken:
                                            Het merendeel van de naasten in de rode klasse lijkt te maken te hebben met een persoon met een relatief rustig en voorspelbaar beloop van de ziekte. Gevaarscriteria zijn er nauwelijks, wanen in mindere mate. Wel zijn er geheugen klachten die een rol kunnen spelen en ook wel wordt passiviteit en over- activiteit en storend gedrag gezien. Bijna de helft van het aantal naasten ervaart in ieder geval gedrag wat als moeilijk in de omgang ervaren. Depressie, angst, psychische klachten en in mindere mate schizofrenie in combinatie met in kleine mate lichamelijke klachten en ouderdom zijn het meest actueel.


                                          2.	De blauwe klasse
                                          De blauwe naaste ervaart het geven van de ondersteuning vooral als een plicht waar mogelijk eerder ook liefde was. Zij hebben nou eenmaal een sociale relatie met de persoon. Er zijn regelmatig conflicten met de persoon voor wie zij zorgen en geven aan dat de kwaliteit van de relatie met de persoon die zij ondersteunen slecht is. De persoon die de blauwe naaste ondersteunt wil het liefst door hen geholpen worden en de naaste probeert daar aan tegemoet te komen. In hun rol van ondersteuner voelen zij zich echter nauwelijks tot niet nuttig en ook ervaart men geen voldoening op basis van het geven van de steun. De naasten in de blauwe klasse zijn ernstig belast en voelen zich erg beperkt in hun vrijheid en zijn behoorlijk gevangen. De groep geeft vaak aan te willen ontsnappen aan de situatie. Zij voelen zich behoorlijk eenzaam wat de belasting fors verhoogd. Zij piekeren over zichzelf en over hun competenties als ondersteuner. Zij krijgen wel wat steun of wisselende steun van anderen.
                                          Het merendeel van de blauwe zorgontvangers is vrouw. Ook is de naaste is vaker een vrouw. Ongeveer 60% woont in het zelfde huis met de persoon die door hen ondersteunt wordt. De meeste behandeling komt vanuit de huisarts en er is niet nauwelijks ondersteuning vanuit de GGZ of woonbegeleiding. Het leven wordt het meest beïnvloed door het geven van heel veel emotionele steun en in iets mindere mate het helpen bij het opbouwen en / of onderhouden van een eigen netwerk en het huishouden. Controle op medicatie inname in ruim 25% van de gevallen en in mindere mate is er sprake van misbruik van middelen en zelfbeschadiging.

                                          Diagnose en bijkomende zaken:
                                            In de blauwe klasse zijn de meeste naasten 10 jaar jonger dan de persoon voor zij zorgen. Depressie en psychische klachten en angst komen het meest voor. Bij iets meer dan een kwart van het aantal zorgontvangers speelt ouderdom en / of heeft lichamelijke klachten. Storend- en moeilijk gedrag komen vaak voor. Daarbij is er sprake van wanen, problemen met ondernemen van activiteiten, geheugen, storend gedrag en mogelijkheden in de omgang gescoord.


                                          3.	De groene klasse
                                          Groen geeft de ondersteuning vooral uit liefde en genegenheid en daarbij is er ook sprake van plichtgevoel. Deze groene naasten ervaren wel een behoorlijke belasting en geven aan dat zij dat er wel voor over hebben. Zij ervaren een in mindere mate een afname in vrijheid en de belasting is goed te hanteren. Zij voelen zich een beetje nuttig maar ook deze naaste geeft aan uit de ondersteuning zelf geen voldoening te halen. Zij doen het vooral voor de ander. Zij voelen zich aardig competent in het vervullen van de rol van ondersteuner en denken nauwelijks na over hun invulling van deze rol. Zij geven aan dat zij zich erg eenzaam te voelen in de relatie met de ander en krijgen geen steun van anderen.
                                          De zorgontvanger is in 75% van de situaties een vrouw. De ondersteuning wordt door bijna evenveel mannen als vrouwen gegeven. In ruim 60% van de situaties wonen zij in een huis. De huisarts is de voornaams betrokken behandelaar. Deze naasten geven voornamelijk emotionele ondersteuning en in de helft van de situaties helpen zij bij huishouden, administratie en sociale contacten. Controle op medicatie inname in ruim 25% van de gevallen en in mindere mate is er sprake van misbruik van middelen en zelfbeschadiging.
                                          Diagnose en bijkomende zaken:
                                            De psychiatrische problematiek is net als psychische klachten, lichamelijke- en geheugen klachten of ouderdom minder dan 25% van de situaties gescoord. Wel worden wanen, problemen ten aanzien van activiteit, geheugen en moeilijkheden in de omgang aangegeven. De typen relaties variëren van partner, kind, ouder en familie.

                                          4.	De paarse klasse (doet het meest op alles)
                                          De naasten in de paarse klasse geven de ondersteuning uit gevoel van plicht in plaats van liefde en genegenheid. Zij voelen zich ernstig belast en zijn ernstig beperkt in hun vrijheid. De kwaliteit van de relatie met de persoon die zij ondersteunen is slecht. Zij geven de ondersteuning omdat de persoon niet door iemand anders geholpen wil worden. De ondersteuning geeft hen totaal geen voldoening en daarbij voelen zij zichzelf niet nuttig. Zij piekeren veel over hoe zij hun rol als ondersteuner vervullen. Zij voelen zich namelijk niet competent om deze rol te vervullen. Ook spelen er veel conflicten.  Gek genoeg is paars naaste is het minst eenzaam. Zij krijgen wel steun van anderen.
                                          De zorgontvanger is merendeel vrouw. De mannelijke naaste is in de meerderheid. Bijna driekwart woont niet in het zelfde huis. Huisarts is meest betrokken hulpverlener, dan thuiszorg, GGZ en ziekenhuis. Daarbij opgeteld geven deze naaste niet alleen veel emotionele steun maar doen zij ook veel in het huishouden, de administratie, helpen de zorgontvanger bij sociale contacten. Daarbij de medicijn controle, controle op middelen en is er in meer situaties sprake van zelfbeschadiging.

                                          Diagnose en bijkomende zaken:
                                            Meeste illnesses. Deze naasten scoren op alle symptomen en daarbij zijn de personen voor wie zij zorgen gevaarlijker. Het zijn meest partners, dan kinderen en las laatst wordt de ouder gescoord.
