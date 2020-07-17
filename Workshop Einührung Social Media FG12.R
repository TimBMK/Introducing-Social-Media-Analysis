########################################################
## Workshop: Einführung in Social Media Analyse mit R ##
########################################################

setwd("C:\\Users\\koenig\\Documents\\R\\Workshop Einführung Social Media FG12")

library(tidyr)


            ######################
            ## Teil 1: Scraping ##
            ######################


  # Twitter-Scraping mit rtweet

library(rtweet)

  # benötigt Zugriff auf die Twitter APIs, welcher (mittlerweile) von Twitter geprüft und ggf. gewährt wird

    #   * Anfrage für API-Zugriff kann auf developer.twitter.com (benötigt Twitter Account) gestellt werden

    #   * Bei korrekt und vollständig ausgefüllten Formularen zum Verwendungszweck bisher (relativ) 
    #       problemloser Zugriff

    #   * Facebook hat den API Zugriff für Forschungszwecke mittlerweile eingestellt, 
    #       auch im Zusammenhang mit Cambridge Analytica

    #   * Plattformen wie Youtube besitzen ebenfalls APIs mit Möglichkeiten zum Scrapen



load("C:\\Users\\koenig\\Documents\\R\\twittertoken.RDa")

    # -> Twittertoken, enthält Zugriffsdaten für die APIs



    #   Twitter-APIs:
    #
    #     - STREAM API (schneidet live alle Tweets zu z.B. einem Hashtag mit, funktionier wie die 
    #           Timeline auf Twitter)

    #         * geeignet, um via Server über längere Zeiträume Twitter Konversationen mitzuschneiden

    #         * greift maximal 1% des globalen Twitter-Aufkommens ab

    #         * da Tweets im Moment ihres Auftauchens in der Timeline gesichert werden, ist die Stream API 
    #               nicht geeignet, um Retweets / Likes etc. eines einzelnen Tweets zu sichern und zu bewerten 


  q <- "#valentinstag"
  stream <- stream_tweets(q, timeout = 120, token = twitter_token)
    
          
    #     - REST API (Search, funktioniert wie die Suche auf Twitter)
          
    #         * Zugriff auf Accountdaten wie Follower, Tweets, Profilinformationen
          
    #         * Zugriff auf Tweets, Auswahl nach Suchbegriffen (z.B. Hashtags)
          
    #             -> Tweetsuche zeitkritisch, da Tweets nur auf ~7-10 Tage alte Tweets zugegriffen werden kann 
    #                 und Ratelimits (18.000 Tweets pro call bzw. alle 15 Minuten) bestehen
          
    #             -> für größere Themen über Zeit idR sinnvoll, mehrere Suchen täglich über Tage/Wochen 
    #                 durchzuführen, um Ratelimits zu umgehen und Tweets für den gesamten Beobachtungszeitraum 
    #                 abzugreifen
          
  
  search <- search_tweets(q, n = 17000, token = twitter_token)
    
          
    #     => beide APIs mit unbekanntem Bias, da Twitter die Ergebnisse anhand unbekannter Variablen filtert
          
    #         * auch bei identischen Suchtermen unterschiedliche Ergebnisse für Stream und Search
          
    #         * sowohl Stream als auch Search liefern einige Tweets, die die jeweils andere API nicht liefert
          
    #         * unterm Strich scheint die Stream API einige Tweets mehr abzugreifen, bietet jedoch schlechtere 
    #               Datenqualität und weniger Flexibilität (Retweets/Likes nicht verwendbar, bei Retweets 
    #               original Text nicht enthalten, nur live Stream...)
    
          
          
    #     - FIREHOSE API (nicht ohne weiteres verfügbar)
          
    #         * greift GESAMTEN globalen Tweetstream ab, d.i. alle Tweets zu einem Zeitpunkt
          
    #         * beste Datenqualität, Zugriff ist jedoch beschränkt
          
    #         * Zugriff idR. nur von einigen wenigen Firmen, Daten müssen gekauft werden
          
    #         * extrem ressourcenintensiv
    
          

    #   Twitterdaten werden von rtweet als R-Dataframe gespeichert 
          
    #     d.i. eine Art von Tabelle, in der für jede Beobachtung (jeden Tweet) eine Reihe von Variablen 
    #       (Text, User ID...) gespeichert wird (Reihen = Tweets, Spalten = Variablen)

          
  View(search)
  
  View(stream)

  
  # User Profile
  
    # Profile mit Namen, letztem Tweet etc.
      
  fg12 <- c("thothiel", "sebezero", "niklowski", "tmknig", "veza_cs")  # Liste mit FG12 Twitter Accounts
    
  fg12_profile <- lookup_users(fg12, token = twitter_token)  



    # Timelines von Usern (bis zu 3.200 Twets eines Users)

  fg12_tweets <- get_timelines(fg12, n = 3200, token = twitter_token)



    # Freunde (wem folgt der User?) von Profilen

  fg12_friends <- get_friends(user=fg12, token = twitter_token)



    # Follower eines Profils

  weizenbaum_followers <- get_followers(user="jwi_berlin", token = twitter_token)


  
    # IDs der Follower nachschlagen für mehr Informationen

  weizenbaum_followers <- lookup_users(weizenbaum_followers$user_id, token = twitter_token)


  
  

            ####################
            ## Teil 2: Metrik ##
            ####################

  library(ggplot2)

  # Tweets & Friends der FG12

  tweets <-   data.frame(table(fg12_tweets$screen_name))
  names(tweets) <- c("user", "tweets")
  tweets$user <- tolower(tweets$user)
  
  
  friends <- data.frame(table(fg12_friends$user))
  names(friends) <- c("user", "friends")
  
  
  fg12_metrics <- merge(tweets, friends)
  View(fg12_metrics)


##

twitter_data <- fg12_tweets

# twitter_data <- search

##


  # Hashtags

  hashtags <- data.frame(table(tolower(unlist(twitter_data$hashtags))))
  
  View(hashtags)


    # Top Hashtags
  hashtags_top <- tail(hashtags[order(hashtags$Freq), ], 10)



    # Zeitverlauf

    # Tweetaufkommen
  ggplot(data = twitter_data, aes(x = created_at)) +
    geom_line(stat="bin", bins = 45) + 
    labs(x = "Time", y ="Tweets") +
    theme_bw(base_size=15)
  
  
      # Tweetaufkommen nach User
  ggplot(data = twitter_data, aes(x = created_at, color = screen_name)) +
    geom_line(stat="bin", bins = 45) + 
    labs(x = "Time", y ="Tweets") +
    theme_bw(base_size=15)



    ## Anwendungsfall #metwo

  library(tidyr)
  library(data.table)
  library(lubridate)

  load("C:\\Users\\koenig\\Documents\\R\\#metwo\\metwo_tweets_final.RDa")
  
    # Twitter Daten zu #metwo. Über ca 4 Wochen unter dem Hashtag #metwo gesampled


  metwo_hashtags <- table(tolower(unlist(metwo_tweets$hashtags)))
  metwo_hashtags <- data.frame(metwo_hashtags)
  metwo_hashtags_top <- metwo_hashtags[!(metwo_hashtags$Var1=="metwo"),]
  metwo_hashtags_top <- tail(metwo_hashtags_top[order(metwo_hashtags_top$Freq), ], 10)
  View(metwo_hashtags_top)


    # Zeitverlauf

  metwo_hashtags_date <- subset(metwo_tweets, select=c(hashtags, created_at)) %>% as.data.table()
  metwo_hashtags_date <-  unnest(metwo_hashtags_date, hashtags)
  metwo_hashtags_date$hashtags <- tolower(metwo_hashtags_date$hashtags)
  metwo_hashtags_date <- subset(metwo_hashtags_date, !metwo_hashtags_date$hashtags=="metwo")
  
  metwo_hashtags_date <- subset(metwo_hashtags_date, subset=(hashtags %in% metwo_hashtags_top$Var1)) 
  
  metwo_hashtags_date <- table(metwo_hashtags_date) %>% as.data.table()
  metwo_hashtags_date$created_at <- as_date(metwo_hashtags_date$created_at)
  metwo_hashtags_date <- aggregate(N ~ hashtags + created_at, FUN = sum, data=metwo_hashtags_date)

    # plot: Hashtags im Zeitverlauf
  ggplot(metwo_hashtags_date) +
    geom_line(aes(x=as.POSIXct(created_at), y=N, colour=hashtags), stat="identity") +
    scale_y_continuous("Number of tweets") +
    scale_x_datetime(date_breaks="1 day") +
    scale_color_brewer(type="qual", palette="Paired") +
    theme_bw() +
    theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
    labs(x="", title="#metwo Hashtags over time \n")


    # Zeitverlauf der Hashtags:

    #   * Abbildung von Themen im Zeitverlauf (etwa #mequeer oder #kochallenge)

    #   * intensives Auftauchen (und Verschwinden) von #kochallenge legt gezielten Versuch der 
    #       Diskursbeeinflussung nahe
    #       -> gleichzeitiges hohes Aufkommen von #afd legt Verbindung nahe

  View(metwo_tweets)


  
  
            #######################
            ## Teil 3: Botscores ##
            #######################

      # Botometer (University of Indiana)
      
      #   * u.a. verwendet in: 
      #       Bessi & Ferrara 2016 Social bots distort the 2016 U.S. Presidential election online discussion
      #       Keller & klinger 2018 Social Bots in Election Campaigns
      
      #   * berechnet Wahrscheinlichkeit eines Users, ein Bot zu sein (zwischen 0 und 1)
      
      #   * verwendet Userdaten zu:
      #
      #         - User features (Follower, Tweets, Profilinformationen)
      #     
      #         - Friends features (Beziehung eines Users zu Followern und Freunden, etwa Mentions, Retweets etc.)
      #
      #         - Netzwerk features (Netzwerkposition eines Users und Eigenschaften des Netzwerks eines Users )
      #     
      #         - Temporale features (Userverhalten im Zeitverlauf, d.i. etwa Tweetaufkommen, 
      #             Tag/Nacht-Rhythmus etc.)
      #
      #         - Inhalt / Sprache (Länge und Sinnhaftigkeit von Tweets)
      #
      #         - Sentiment (Emtionoalität von Tweets)
      
      #   * Score nicht determinstisch, sondern Einschätzung bot-artigen Verhaltens 
      #       - nicht alle Accounts mit Score > 0.5 sind Bots!
      
      #   * nur für englischsprachige Tweets optimiert, d.i. Inhalts- und Sentimentanalysen für 
      #       z.B. deutsche Tweets nicht verwendbar
      
      #   * genaue Berechungen des Botometers nicht offen gelegt
      
      #   * da Botometer recht zeitaufwändig und benötigter Zugriff auf Twitter API Ratelimits mitsichbringt, 
      #        in der Praxis häufig nur aktive User (z.B. mind. 1 Tweet/Tag) überprüft
      #       (wen interessieren schon Bots, die nicht aktiv sind? -  gerade in themenbezogenen Samples)
      
      #   * Zugriff auf Botscores zeitkritisch, da potentielle Botprofile von Twitter oder den Usern gelöscht 
      #       werden können und so nicht mehr via Botometer überprüfbar sind
      
      
      
      # Funktion zum Call der Botometer API 
      
      #   -> relevante Informationen wie Username, Timeline und Mentions werden von der Twitter API bezogen,
      #         via html an die Botometer API weitergeleitet und deren Ergebnisse in R eingefügt
      
      #   benötigt Twittertoken (API Zugriff) und Mashape (API Plattform, mit u.a. Botometer) Key 
      
  load("C:\\Users\\koenig\\Documents\\R\\MashapeKey.RDa")

  library(httr)
  library(RJSONIO)
  library(data.table)
  library(ggplot2)
  
  botometer <- function(screen_name, twitter_token, MashapeKey) {
    
    user.url <- "https://api.twitter.com/1.1/users/show.json?screen_name="
    timeline.url <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
    mentions.url <- "https://api.twitter.com/1.1/search/tweets.json?q=%40"
    
    user <- httr::GET(paste0(user.url, screen_name), twitter_token)
    timeline <- httr::GET(paste0(timeline.url, screen_name, "&count=200&include_rts=true"), twitter_token)
    mentions <- httr::GET(paste0(mentions.url, screen_name, "&count=100"), twitter_token)
    
    payload <- list(
      user = content(user, type="application/json")[1:4],
      timeline = content(timeline, type="application/json"),
      mentions = content(mentions, type="application/json"))
    
    payload.json <- RJSONIO::toJSON(payload, auto_unbox = T)
    
    result = httr::POST("https://osome-botometer.p.mashape.com/2/check_account",
                        add_headers(
                          "X-Mashape-Key"=MashapeKey,
                          "Content-Type" = "application/json",
                          "Accept" = "application/json"),
                        body=payload.json,
                        encode = "json")
    
    result = httr::content(result, as = "parsed")
    return(result)
  }
      

      # FG12 botscores
        
  botscores <- data.table()
  pb = txtProgressBar(min = 0, max = nrow(fg12_profile), initial = 0) 
  for(i in 1:length(fg12_profile$screen_name)) {
    user <- fg12_profile$screen_name[i]
    result <- botometer(user, twitter_token, MashapeKey)
    score <- data.table(t(sapply(unlist(result),c)))
    botscores <- rbind(botscores, score, fill=T, use.names=T)
    setTxtProgressBar(pb, i)
  }
      
      # convert scores to numeric
  botscores[,1:18] <- lapply(botscores[,1:18],as.numeric)      
  
  View(botscores)
      # => "universal" score sprachunabhänig (geeignet für z.B. deutsche Tweets/User)
    
  
  ggplot(data = botscores, aes(x = user.name, y = scores.universal))  +
    geom_bar(stat = "identity")
      
  
  
      # #metwo Botscores
      
  load("C:\\Users\\koenig\\Documents\\R\\#metwo\\metwo_botscores.RDa")
  
  # "Aktive User" bei #metwo User mit mindestens 20 Tweets im Untersuchungszeitraum von ca 5 Wochen
  
  tweet_freq <- data.frame(table(metwo_tweets$screen_name))
  names(tweet_freq) <- c("screen_name", "tweets")
  active_users <- subset(tweet_freq, tweets > 19)
  
  
  # Density plot (Verteilung der Botscores)
  
  DensityBotscoreGraph <- ggplot(data = metwo_botscores) + geom_density(aes(x=scores.universal))
  DensityBotscoreGraph
  
  
  # Tweetaktivität von (vermeintlichen) Bots und nicht-Bots
  
  active_bots <- metwo_botscores
  active_bots$bot <- NA
  
    # Errors (Account gelöscht etc.) als "deleted" setzen, botscore dort auf -1
  active_bots$bot[!is.na(active_bots$error)] <- "account deleted"
  active_bots$scores.universal[is.na(active_bots$scores.universal)] <- -1
  
    # Hier Klassifikation von Usern mit Botscore > 0.5 als "Bots"
  active_bots$bot[active_bots$scores.universal >= 0.5] <- "bot"
  active_bots$bot[active_bots$scores.universal <= 0.5 & !active_bots$scores.universal == -1] <- "human"
  
  table(active_bots$bot)
  
  
    # Klassifikation mit Tweet Data mergen (nur aktive User)
  
  metwo_bots <- subset(metwo_tweets, screen_name %in% active_users$screen_name)
  metwo_bots <- merge(metwo_bots, active_bots, by.x = "screen_name", by.y = "user.screen_name") %>% subset(select=c("screen_name", "bot", "created_at"))
  
  table(metwo_bots$bot)
  
  
    # Plotten
  
  ggplot(data = metwo_bots, aes(x = created_at, color = bot)) +
    geom_line(stat="bin", bins = 45) + 
    scale_y_continuous("Number of tweets") +
    scale_x_datetime(date_breaks="1 day") +
    scale_color_brewer(type="qual", palette="Paired") +
    theme_bw() +
    theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
    labs(x="", title="#metwo bot activity over time \n")
  

  
  
            #############################
            ## Teil 4: Netzwerkanalyse ##
            #############################

  
    # metwo Netzwerke
  
  load("C:\\Users\\koenig\\Documents\\R\\#metwo\\metwo_tweets_final.RDa")

  
    # Zufallssample von ~10% der daten, um Berechnungszeiten zu reduzieren. Nur Tweets von "aktiven Usern"
  
  metwo_sample <- subset(metwo_tweets, screen_name %in% active_users$screen_name)
  metwo_sample <- metwo_sample[sample(nrow(metwo_sample), 20000), ]
  
  
  
    # Twitterdaten zu Graph-Daten mit graphtweets
  
  library(igraph)
  library(graphTweets)
  library(dplyr)
  library(rgexf)
  
  
    # Mentions-Graph: wer erwähnt / retweetet / quoted / replied wen?
  
  metwo_sample %>%
    gt_edges(screen_name, mentions_screen_name) %>%
    gt_nodes() %>%
    gt_graph() -> mentions_graph
  
  
  
  # Export nach Gephi
  
  
    # Dataframe aus Graphdaten
  
  
    # Nodes (Knoten, hier: User)
  
  nodes_df <- data.frame(ID = c(1:vcount(mentions_graph)), NAME = V(mentions_graph)$name)
  
  
    # Attribute-Dataframe (extra Informationen) für Nodes
  
  nodes_att <- data.frame(n = V(mentions_graph)$n)
  
      # * n = Vorkommnisse eines Nodes (Knotens) im Sample
  
  
    # Bot-Klassifikation als Attribut hinzufügen
  
  nodes_att$user.screen_name <- V(mentions_graph)$name
  
  active_bots$user.screen_name <- tolower(active_bots$user.screen_name)
  
  nodes_att <- left_join(nodes_att, active_bots) %>% select(n, cap.universal, bot)
  

  
    # Edges (Verbindungen)
  
  edges_df <- as.data.frame(get.edges(mentions_graph, c(1:ecount(mentions_graph))))
  
  
    # Attribute-Dataframe (extra Informationen) für Edges
  
  edges_att <- data.frame(n = E(mentions_graph)$n)
  
      # * n = Vorkommnisse einer Verbindung im Sample
  
  
    # Gephi File schreiben
  
  write.gexf(nodes_df, edges_df, nodesAtt = nodes_att, edgesAtt = edges_att, output = "mentions_graph_sample.gexf")
  
  
  
  
            #########################
            ## Teil 5: Textanalyse ##
            #########################
  
  
  # Topic Modelling von #metwo Tweets
  
  load("C:\\Users\\koenig\\Documents\\R\\#metwo\\metwo_tweets_final.RDa")
  
  library(stm)
  library(tidytext)
  library(textstem)
  library(stringr)
  library(dplyr)
  library(ggplot2)
  
  
  
  # Structural Topic Modelling  
  
  #   * Methode des Machine Learning, basiert auf Korrelation von Dokumenten und 
  #       (in Dokumenten enthaltenen) Wörtern mit einer Anzahl Topics; generativer Prozess
  #
  #   * Im Gegensatz zu andern Topic Modelling-Ansätzen erlaubt STM die Einbindung von Metadaten zur Beeinflussung
  #       des Prozesses
  #
  #   * Hier: Hashtags und Zeitpunkt (Tag des Tweets) als Metadaten
  #
  #   * Anzahl Topics muss per Hand gesetzt werden
  
  
  
  # Erstellen des Textkorpus
  
  
  # Entfernen von Retweets & nicht-deutschsprachigen Tweets, reformatieren 
  text_noRT <- subset(metwo_tweets, is_retweet==F & lang == "de", select = c("text", "status_id", "screen_name")) %>% unnest_tokens(word, text)
  text_noRT$word <- iconv(text_noRT$word, to="UTF-8")
  
  
  # Entfernen von Stopwords (inhaltlich nicht-relevante Wörter wie der/die/das, aber, dass....)
  
  # generelle Stopwords aus library
  stopwords_de <- get_stopwords(language = "de")
  stopwords_en <- get_stopwords(language = "en")
  
  # weitere, Twitter-spezifische und in library nicht vorhandene / aktuelle Stopwords
  more_stopwords <- tibble(word = c("metwo", "t.co", "https","amp", "dass", "beim", "mal"))
  text_noRT <- text_noRT %>% anti_join(stopwords_de) %>% anti_join(stopwords_en) %>% anti_join(more_stopwords)
  
  # Usernamen (aus Mentions, z.B. @thothiel) entfernen
  names <- tibble(word = metwo_tweets$screen_name)
  text_noRT <- text_noRT %>% anti_join(names)
  
  
  
  # Stemmen und Lemmatizen
  
  #   = Reduzieren der Wörter auf Wortstamm
  
  # Verwendung der TreeTagger Engine (Uni München) zum generieren der Wortstämme
  # (http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)
  
  
  library(koRpus.lang.de)   # koRpus package für deutsch
  
  # Generieren des lemma-Dictionaries aus den Wörtern im Textkorpus
  dictionary_de <- make_lemma_dictionary(text_noRT$word, engine = "treetagger", lang="de")
  
  
  # einige Tokens wurden doppelt generiert. Entfernen der unpassenden Interpretationen (Wortstämmen)
  
  dictionary_de[duplicated(dictionary_de$token),]
  
  # linke = links (instead of "linken")
  dictionary_de <- subset(dictionary_de, token != "linke" & lemma != "linken")
  
  # passierte = passieren (instead of "passiert)
  dictionary_de <- subset(dictionary_de, token != "passierte" & lemma != "passiert")
  
  # studierte = studiert (statt studieren)
  dictionary_de <- subset(dictionary_de, token != "studierte" & lemma != "studieren")
  
  
  # lemmatizen: Zuordnen  der Wortstämme zu den Worten
  
  text_noRT$lemmatized <- lemmatize_words(text_noRT$word, dictionary = dictionary_de)
  
  
  # Kleinschreibung (vermeidet Fehler)
  
  text_noRT$lemmatized <- tolower(text_noRT$lemmatized)
  
  
  
  # Wordcounts
  
  words <- count(text_noRT, lemmatized, sort = T)
  View(words)
  
  
  # Wordcount pro document (tweet) - für Topic Modelling benötigt
  
  word_count <- count(text_noRT, status_id, screen_name, lemmatized, sort = TRUE)
  
  
  
  # Structural Topic Modelling 
  
  
  # Daten vorbereiten
  
  # Document-Term-Matrix erstellen
  dtm <- cast_dtm(data=word_count, term=lemmatized, document=status_id, value=n)
  
  # Corpus einlesen
  stm_data <- readCorpus(dtm, type="slam")
  
  
  # Metadaten einlesen
  
  # Subset: Tweets ohne Retweets & nur deutsche Tweets (Dokumente (Tweets) müssen konsistent bleiben)
  noRT <- subset(metwo_tweets, is_retweet==F & lang == "de") 
  
  # Dokumente (Tweets) entfernen, die bei Entfernung der Stopwords gelöscht wurden (kurze Antworten etc.)
  noRT <- noRT[noRT$status_id %in% word_count$status_id,]
  
  # Metadaten aus Subset extrahieren
  metadata <- subset(noRT, select = c("created_at", "hashtags", "status_id"))  
  
  # Formatieren 
  metadata$hashtags <- vapply(metadata$hashtags, paste, collapse = " ", character(1L))
  metadata$hashtags <- tolower(metadata$hashtags)
  
  #  #metwo Hashtag (definieren das Sample) und NA Einträge sowie Kommas entfernen 
  metadata$hashtags <- str_remove_all(metadata$hashtags, "metwo")
  metadata$hashtags <- str_remove_all(metadata$hashtags, "na")
  metadata$hashtags <- str_remove_all(metadata$hashtags, ",")
  
  # Format an STM-Erwartungen anpassen
  colnames(metadata)[colnames(metadata)=="status_id"] <- "docname" 
  metadata$hashtags <- as.factor(metadata$hashtags)
  
  # Daten als Nummer des Tages im Sample
  metadata$created_at <- as.Date(metadata$created_at)
  startdate <- as.Date("2018-07-27")
  metadata$day <-  difftime(metadata$created_at, startdate, units = "days") %>% as.integer()
  
  
  # Metadaten mit Corpus kombinieren und präparieren
  
  # überprüfen, welche Auswirkungen die lower threshhold (Mindestanzahl an Dokumenten, in denen ein Wort vorkommen
  #   muss) hat: wieivele Dokumente / Worte  werden entfernt? 
  plotRemoved(stm_data$documents, lower.thresh = seq(1, 50, by=10))  # check words removed for threshold 
  
  # Lower Threshhold auf 10 gesetzt, um insgesamt eher irrelevante Worte zu entfernen und Rechendauer 
  #   handlebar zu machen
  prep_stm <- prepDocuments(documents=stm_data$documents, vocab=stm_data$vocab, meta=metadata, lower.thresh = 10)
  
  # -> 5429 terms (removed 69.441), 415.025 tokens, 42.331 Documents / Tweets (removed 275)
  
  
  # Berechnen der Topic Models
  
  # Statistische Verfahren, um optimale Anzahl Topics zu ermitteln 
  Ks <- searchK(documents = prep_stm$documents, vocab = prep_stm$vocab, K = c(3,5,10,20),  gamma.prior = "L1",
                prevalence =~ hashtags + s(day), data = prep_stm$meta, init.type = "Spectral")
  plot(Ks)
  plot(Ks$results$K,Ks$results$exclus)
  
  # * Können helfen, Anzahl Topics festzulegen. 
  #     Sind jedoch nur Hilfsmittel und sagen nichts über inhaltliche Schlüssigkeit aus!
  
  
  # Modelle
  
  # Modell mit 20 Topics
  
  stm20 <- stm(documents = prep_stm$documents, vocab = prep_stm$vocab, K = 20, prevalence =~ hashtags + s(day), 
               data = prep_stm$meta, gamma.prior = "L1", init.type = "Spectral" )
  
  # Vorbereitetes Topicmodel (lange Berechnungsdauer)
  load("C:\\Users\\koenig\\Documents\\R\\#metwo\\metwo_stm20.RDa")
  
  plot(stm20, type="summary")
  
  
  # Modell mit 10 Topics
  
  stm10 <- stm(documents = prep_stm$documents, vocab = prep_stm$vocab, K = 10, prevalence =~ hashtags + s(day), 
               data = prep_stm$meta, gamma.prior = "L1", init.type = "Spectral" )
  
  # Vorbereitetes Topicmodel (lange Berechnungsdauer)
  load("C:\\Users\\koenig\\Documents\\R\\#metwo\\metwo_stm10.RDa")
  
  plot(stm10, type="summary")
  
  
  # Alternative Visualisierung: sortiert nach beta (Assoziation eines Worts mit einem Topic)
  stm10_td <- tidy(stm10)
  
  top_terms <- stm10_td %>% group_by(topic) %>% top_n(5, beta) %>% ungroup() %>% arrange(topic, -beta)
  
  stm_topics_10 <- top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ topic, scales = "free") +
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1))
  
  stm_topics_10
  
  
  stm20_td <- tidy(stm20)
  
  top_terms <- stm20_td %>% group_by(topic) %>% top_n(5, beta) %>% ungroup() %>% arrange(topic, -beta)
  
  stm_topics_20 <- top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ topic, scales = "free") +
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1))
  
  stm_topics_20
  
  
  # Für Topics einschlägigste Dokumente (Tweets)
  
  # Tweetdaten sortieren und anpassen (bei prepDocuments entfernte Dokumente ebenfalls entfernen, 
  #   gleiche Reihenfolge)
  corpus_data <- noRT[order(match(noRT$status_id,dtm$dimnames$Docs)),]
  corpus_data <- corpus_data[-prep_stm$docs.removed,]
  
  findThoughts(stm10, texts = corpus_data$text, topics = c(1:10), n = 20)   
  
  findThoughts(stm20, texts = corpus_data$text, topics = c(1:20), n = 5)
  
  
  # Sortierung der Wörter eines Topics nach verschiedenen Scores
  
  labelTopics(stm10)
  
  labelTopics(stm20)
  
  # see
  help(labelTopics)
  
