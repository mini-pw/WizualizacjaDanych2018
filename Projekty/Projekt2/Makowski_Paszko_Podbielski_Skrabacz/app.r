library("shiny")
library("dplyr")
library("ggplot2")
library('scales')
library(RColorBrewer)
library("waffle")
library(shinythemes)

# -------------------------- Dodatkowe dane ---------------------------
df1 <- data.frame(name=c("Bonds", "Car Towing", "Lawyer Fees", "Probation Fees", "Court Fees", "Surcharge", "Interlock Devise", "Paperwork", "Fines"), val =c(20, 200, 5000, 1440, 400, 3000, 950, 390, 2000))
df2 <- data.frame(lang=c("Python", "Java", "C++", "Ruby","PHP", "C", "JavaScript", "C#", "Perl", "Clojure", "Scala", "Objective C", "TCL"), val =c(29.8, 25.8, 12.6, 9.6, 7.3, 4.9, 3.9, 2.5, 2, 0.8, 0.6, 0.1, 0.02), 
                  type=c("interpretable", "compiled", "compiled", "interpretable", "interpretable", "compiled", "interpretable", "compiled", "interpretable", "other",  "compiled", "compiled", "interpretable"))
df3 <- data.frame(year=c(1997, 1998, 1999, 2000, 2001), cnt=c(25, 30, 20, 10, 60))
df4 <- data.frame(value=c(11, 42, 5, 42), Item=c('A', 'B', 'C', 'D'))
df5 <- data.frame(name=c("PiS", "KE", "Wiosna", "Konfederacja", "Kukiz", "Lewica Razem", "INNI"), val =c(42.4, 39.1, 6.6, 6.1, 4.1, 1.3, 0.4))
df6 <- c("Radość"=16, "Zadowolenie"=32, "Obojętność"=20, "Zaskoczenie"=14,
         "Rozczarowanie"=16, "Obawa"=16, "Niepewność"=10, "Zniechęcenie"=5, "Złość"=5, "Nie mam zdania"=1)
df7 <- data.frame(procent=c(43, 31, 4, 22), Item=c("zdecydowanie przeciw", "przeciw" ,"zdecydowanie za", "za"),
                  fill=as.factor(c(1,1,0,0)))  
df7$Item <- factor(df7$Item, levels=unique(df7$Item))

df8 <- data.frame(attacks=c(140, 98, 74, 41, 33, 36, 16, 43, 9, 276), 
                  
                  deadly_attacks=c(28, 21, 2,0,0,4,0,2,0,9),
                  shark_name=c("Tiger shark", "Bull shark", "Sandtiger shark", "Hammerhead shark",
                               "Blacktip shark", "Blue shark","Blacktip reef shark",
                               "Shortfin Mako", "Grey reef shark", "Others"))
df8 <- data.frame(attacks=c(112, 77, 72, 41, 33, 32, 16, 41, 9, 265, 28,21,2,0,0,4,0,2,0,9),
                  typ= as.factor(rep(c("Bez skutku śmiertelnego", "Z skutkiem śmiertelnym"),each=10)),
                  shark_name=rep(c("Tiger shark", "Bull shark", "Sandtiger shark", "Hammerhead shark",
                                   "Blacktip shark", "Blue shark","Blacktip reef shark",
                                   "Shortfin Mako", "Grey reef shark", "Others"),2))
df8_gr <- df8 %>% group_by(shark_name, typ) %>% summarise(n=sum(attacks))
attack_sum <- df8_gr %>% group_by(shark_name) %>% summarise(sum(n))
percs <-df8_gr %>% mutate(freq = n / sum(n)) %>% filter(typ=="Z skutkiem śmiertelnym") %>% select(freq)
        
p1_g <- ggplot(df1, aes(x = reorder(name, -val), y = val)) +
    geom_bar(stat="identity") +
    theme_classic() +
    labs(x="", y="Koszt") +
    theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 16)) +
    geom_text(aes(label = val, y = val+130), size = 5) +
    scale_y_continuous(expand = c(0, 0))

p2_g <- ggplot(df2, aes(x = lang, y = val, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_classic() +
    labs(x="", y="Wykorzystanie języka [%]", color="Typ języka", title="Języki programowania wykorzystywane w 2013 r.") +
    theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 16)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 32)) +
    scale_x_discrete(limits = df2 %>% arrange(type, -val) %>% pull(lang)) +
    geom_text(aes(label = val, y = val+1), size = 5)

p4_g <- ggplot(df4, aes(x="", y=value, fill=Item)) + 
    geom_bar(width = 1, stat='identity') +
    coord_polar('y', start=0) + 
    theme_minimal()+
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
    ) +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = 100 - cumsum(value) + 0.5*value, 
                  label = percent(value/100)), size=5)

p5_g <- ggplot(df5, aes(x = reorder(name, -val), y = val)) +
  geom_bar(stat="identity") +
  theme_classic() +
  labs(x="", y="Liczba punktów procentowych") +
  theme(axis.text.x = element_text(angle=45, hjust=1), text = element_text(size = 16)) +
  geom_text(aes(label = val, y = val/2), fontface = "bold", size = 5, colour="red")

pallete = colorRampPalette(brewer.pal(11, "Set3"))(11)
pallete[11] <- "#ffffff"
p6_g <- waffle(df6, rows = 10, size = 1, 
                colors = pallete, legend_pos = "bottom",
               title = "Co czuli polacy po wyborze Andrzeja Dudy?",
               xlab = "1 kwadrat = 1 ankietowana osoba\n(w każdej kolumnie jest 10 kwadratów)")

p7_g <- ggplot(df7, aes(x=Item, y=procent,fill=fill)) + geom_bar(stat="identity") + 
  geom_text(aes(label=procent), hjust=-1) + coord_flip() + 
  scale_fill_manual(values=c("#005b96", "#851e3e")) + theme(legend.position = "none",axis.title.y=element_blank())

p8_g <- ggplot(data=df8, aes(y=attacks, x=shark_name, fill=typ)) +
  geom_bar(stat="identity") + coord_flip() + geom_text(label=paste0(round(rep(percs$freq,2), 2)*100, "%"), 
                                                       aes(y=rep(attack_sum$`sum(n)`, 2),
                                                           x=rep(attack_sum$shark_name, 2)),
                                                       hjust=-1) + theme(axis.title.y=element_blank())+
  ylab("Liczba ataków")



# -------------------- Wykresy złe  --------------------
# list("<typ>", "<pytanie>", <odpowiedź>, <wykres/url>)
p1_bad <- list(
    "img", "Ile wynoszą opłaty sądowe (Court fees)?", 400,
    "https://raw.githubusercontent.com/bpaszko/WD-p2/master/dwi_costs.jpg")
p2_bad <- list(
    "img", "Które języki były częściej wykorzystywane w 2013 roku: interpretowalne czy kompilowalne?", "interpretowalne",
    "https://raw.githubusercontent.com/bpaszko/WD-p2/master/coding_languages.jpg")
p3_bad <- list(
    "img", "Ile wynosi CNT w 1998r", 30,
    "https://support.sas.com/kb/24/addl/fusion_24875_1_g24875.gif")
p4_bad <- list(
    "img", "Porównaj B względem D (odp: '1': >, '2': <, '3': =)", "3",
    "https://upload.wikimedia.org/wikipedia/commons/8/88/Misleading_Pie_Chart.png")
p5_bad <- list(
    "img", "Czy PiS uzyskał prawie dwukrotnie lepszy wynik od KE?", "nie",
    "https://raw.githubusercontent.com/bpaszko/WD-p2/master/sondaz_europarlament.png")
p6_bad <- list(
    "img", "Czy ludzi rozczarowanych jest więcej niż obawiających się?", "nie",
    "https://raw.githubusercontent.com/bpaszko/WD-p2/master/andrzej_duda_sondaz.png")
p7_bad <- list(
    "img", "Jaki procent Polaków jest przeciw lub zdecydowanie przeciw przyjęcia uchodźców z państw bliskiego wschodu?", "74",
    "https://github.com/bpaszko/WD-p2/raw/olaf_wykresy/tvp_sondaz.png")
p8_bad <- list(
    "img", "Wykres przedstawia liczbę ataków śmiertelnych na łączną liczbę ataków rekinów. Podaj który rekin ma największą śmiertelność", "Bull shark",
    "http://quantup.pl/wp-content/uploads/blog/20140709-jak-poprawic-wizualizacje-danych/figure/rys2.png")


# -------------------- Wykresy poprawne --------------------
p1_good <- list(
    "plot", "Ile wynoszą opłaty sądowe (Court fees)?", 400,
    p1_g)
p2_good <- list(
    "plot", "Które języki były częściej wykorzystywane w 2013 roku: interpretowalne czy kompilowalne?", "interpretowalne",
    p2_g)
p3_good <- list(
    "plot", "Ile wynosi CNT w 1998r", 30,
    ggplot(df3, aes(x=year, y=cnt)) + geom_bar(stat='identity'))
p4_good <- list(
    "plot", "Porównaj B względem D (odp: '1': >, '2': <, '3': =)", "3",
    p4_g)
p5_good <- list(
    "plot", "Czy PiS uzyskał prawie dwukrotnie lepszy wynik od KE?", "nie",
    p5_g)
p6_good <- list(
    "plot", "Czy ludzi rozczarowanych jest więcej niż obawiających się?", "nie",
    p6_g)
p7_good <- list(
    "plot", "Jaki procent Polaków jest przeciw lub zdecydowanie przeciw przyjęcia uchodźców z państw bliskiego wschodu?", "74",
    p7_g)
p8_good <- list(
  "plot", "Podaj który rekin ma największą śmiertelność", "Bull shark",
  p8_g)



plots <- list(
    p1_bad, p2_bad, p3_bad, p4_bad, p5_bad, p6_bad, p7_bad, p8_bad,
    p1_good, p2_good, p3_good, p4_good, p5_good, p6_good, p7_good, p8_good
)


# -------------------- Zdefiniowanie układu strony -------------------- 
input_fields <- c("input_n")  # nazwy pól które użytkownik aktualizuje 

ui <- fluidPage(theme = shinytheme("united"), 
    sidebarLayout(
        
        # Panel boczny z polem do wpisywania odczytów
        sidebarPanel(
            textInput("input_n", "Odpowiedź", value = NULL),  # Pole do wprowadzenia wartości
            actionButton("save_input", "Zapisz"),             # Zapisanie wpisanej wartości
            actionButton("clear_last_input", "Cofnij"),
            h3("Podane odpowiedzi"),
            tableOutput("user_inputs"),
            actionButton("clear_input", "Wyczyść odpowiedzi")       # Wyczyszczenie zapisanych wartości
        ),
        
        mainPanel(
            h1("Jak (nie)robić wykresów"),
            textOutput("plot_number"),
            textOutput("plot_question"),
            textOutput("score"),
            tableOutput("score_table"),
            uiOutput("current_img"),
            plotOutput("current_plot")
        )
    )
)


server <- function(input, output, session) {
    
    # Reactive w którym trzymamy odpowiedzi użytkownika
    reVals <- reactiveValues(answers = NULL, current_plot = 1)
    
    # Zapisywanie odpowiedzi użytkownika
    saveData <- function(data) {
        if (length(data) == 1) { data <- t(data) }
        data <- as.data.frame(data)
        if (is.null(reVals[["answers"]])) {
            reVals[["answers"]] <- data
        } else {
            reVals[["answers"]] <- rbind(reVals[["answers"]], data)
        }
        colnames(reVals[["answers"]]) <- "input_n"
    }
    
    # Czyszczenie całości / ostatniego wiersza odpowiedzi użytkownika
    clearData <- function(last = FALSE) {
        # Domyślnie kasujemy zapisane wartości, dodatkowe dwie opcje to obsłużenie pojedynczej odpowiedzi i wyczyszczonych odpowiedzi
        if (!last || is.null(reVals[["answers"]]) || nrow(reVals[["answers"]]) == 1) {
            reVals[["answers"]] <- NULL 
        } else {
            answers_old <- reVals[["answers"]][1:(nrow(reVals[["answers"]])-1), ]  # Zostawiamy wiersze oprócz ostatniego
            names(answers_old) <- rep("input_n", length(answers_old))              # Nadajemy nazwy, żeby zachować spójność
            clearData()                                                            # Czyścimy obecnie zapisane dane
            saveData(answers_old)                                                  # Zapisujemy zachowane wiersze
        }
    }
    
    # Funkcja do wypisywania tabeli z odpowiedziami użytkownika
    printInputTable <- function() {
        output[["user_inputs"]] <- renderTable({
            user_data <- reVals[["answers"]]
            validate(need(nrow(user_data) > 0, "Lista odpowiedzi jest pusta"))
            user_data_formatted <- cbind(paste("Wykres", 1:nrow(user_data)), user_data)
            names(user_data_formatted) <- c("", "Odpowiedź")
            user_data_formatted
        })
    }
    
    # Funkcja rysująca kolejne strony
    printPlot <- function(plot_change = NULL, score = FALSE) {
        current_plot <- isolate(reVals[["current_plot"]]) # odczytanie wartości z reactive'a
        if (is.null(plot_change) || (current_plot + plot_change == 0)) {  # Pierwsze użycie i cofanie się z pierwszego wykresu
            current_plot  <- 1
        } else {
            current_plot  <- current_plot  + plot_change
        }
        reVals[["current_plot"]] <- current_plot  # Zapisanie zaktualizowanej wartości do reactive'a
        
        if (score) {
            output[["current_plot"]]  <- NULL
            output[["current_img"]] <- NULL
            output[["plot_number"]] <- NULL
            output[["plot_question"]] <- NULL
        } else {

            # Numer wykresu i pytanie do niego
            output[["plot_number"]] <- renderText(paste("Wykres nr", current_plot))
            output[["plot_question"]] <- renderText(plots[[current_plot]][[2]])
            
            # Wyświetlamy obrazek, wykres albo nic (na koniec)
            if (plots[[current_plot]][[1]] == "plot" && !score) {
                output[["current_plot"]] <- renderPlot({ plots[[current_plot]][[4]] })
                output[["current_img"]] <- NULL
            } else if (plots[[current_plot]][[1]] == "img" && !score) {
                output[["current_plot"]]  <- NULL
                output[["current_img"]] <- renderUI({ tags[["img"]](src = plots[[current_plot]][[4]]) })
            }
        }
    }
    
    # Początkowe pokazanie się tabelki i wykresu
    printInputTable()
    printPlot()
    
    # Odczytuje dane wprowadzone przez użytkownika
    getUserData <- reactive({ sapply(input_fields, function(x) input[[x]]) })
    
    # Liczba udzielonych odpowiedzi
    getAnswersNumber <- reactive({ ifelse(is.null(reVals[["answers"]]), 0, nrow(reVals[["answers"]])) }) 
    
    # Zapisuje podaną wartość
    observeEvent(input[["save_input"]], {
        current_plot <- reVals[["current_plot"]]
        if (getAnswersNumber() < 16) { saveData(getUserData()) } # Po 16 przestajemy przyjmować input
        
        printInputTable()                                                                     # Musimy odświeżać po kliknięciach
        printPlot(
            plot_change = current_plot <= 16,  # ostatnia zmiana na current_plot = 17, żeby cofanie dobrze działało
            score = getAnswersNumber() == 16)      # jeśli mamy komplet odpowiedzi to wykres
        
    }) 
    
    # Czyści ostatnią podaną wartość
    observeEvent(input[["clear_last_input"]], {
        clearData(last = TRUE)
        printInputTable()
        printPlot(plot_change = -1)
    })
    
    # Czyści podane wartości
    observeEvent(input[["clear_input"]], {
        clearData()
        printInputTable()
        printPlot()
    })
    
    score <- reactive({
        validate(need(getAnswersNumber() == 16, ""))  # Zapewnienie, że liczba odpowiedzi jest taka jak trzeba
        
        user_answers <- isolate(reVals[["answers"]])
        plot_names <- c(paste("Wykres", 1:8), paste("Wykres", 1:8, "(poprawiony)"))
        correct_answers <- sapply(plots, function(p) p[[3]])
        answers_correctness <- c("Źle... :(", "OK! :)")[1 + (sapply(user_answers, casefold) == casefold(correct_answers))]
        
        score <- as.data.frame(cbind(plot_names, correct_answers, user_answers, answers_correctness))
        colnames(score) <- c("", "Poprawny wynik", "Podana odpowiedź", "Zaliczone?")
        score
    })
    
    
    # Wypisanie wyników
    output[["score_table"]] <- renderTable({
        score()
    })
    
    output[["score"]] <- renderText({
        paste("Liczba poprawnych odpowiedzi:", sum(score()["Zaliczone?"] == "OK! :)"))
    })
    
}

shinyApp(ui = ui, server = server)
