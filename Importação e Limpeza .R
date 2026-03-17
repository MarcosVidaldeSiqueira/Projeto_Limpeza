# Fazendo a integração com Git ----------------------------------------------
#instalo o pacote usethis
install.packages("usethis")

#cód. p/ configurar o GIT com minhas credenciais
usethis::use_git_config(user.name = "Marcos Vidal",
                        user.email = "232029443@aluno.unb.br")

#cód. para abrir o GITHUB e copiar o token
usethis::create_github_token()

#cód. para abrir o arquivo com minhas "senhas" tokens
usethis::edit_r_environ()

#cód. para ver se deu certo, tem que ter <discovered> na linha • Personal access token for "https://github.com"
usethis::git_sitrep()


# Bíbliotecas  ------------------------------------------------------------
installed.packages('plotly')
library(plotly)
library(readr)
library(lubridate)
library(tidyverse)
library(usethis)
library(here)

Reports <- read_csv2(here::here("reports_10_2026-02-02.csv") , na = "-")

str(Reports)

Reports$`Desde quando está se sentindo mal?` <- dmy(Reports$`Desde quando está se sentindo mal?`)

Reports$Ind_Febre <- str_detect(Reports$`Quais sintomas você sentindo neste momento?`, "Febre")

table(Reports$Ind_Febre)

Reports$Sindrome_Gripal <- str_detect(Reports$`Quais sintomas você sentindo neste momento?`,
                                      "Febre|Coriza|Diminuição do apetite")

Reports$Sindrome_Diarreica <- str_detect(Reports$`Quais sintomas você sentindo neste momento?`,
                                        "Diarreia|Dor abdominal|Vômito|Diminuição do apetite")

Reports$Sindrome_Exantematica <- str_detect(Reports$`Quais sintomas você sentindo neste momento?`,
                                            "Bolhas, espinhas ou descamação|Coceira no corpo")

Reports$Sindrome_IST <- str_detect(Reports$`Quais sintomas você sentindo neste momento?`,
                                   "Ferida ou verruga na genital|Corrimento genital|Dor ou ardência ao urinar|Dor ou Sangramneto na relação sexual")

Reports$Sindrome_Conjuntiva <- str_detect(Reports$`Quais sintomas você sentindo neste momento?`,
                                         "Olhos vermelhos|Lacrimejamento|Ardência e coceira nos olhos")

Reports$Sintomaticos_Inespecificos <- Reports$Sindrome_Conjuntiva == FALSE &
                                                   Reports$Sindrome_Diarreica == FALSE &
                                                   Reports$Sindrome_IST == FALSE &
                                                   Reports$Sindrome_Exantematica == FALSE &
                                                   Reports$Sindrome_Gripal == FALSE

Reports$Sintomaticos_Inespecificos2 <- !is.na(Reports$`Quais sintomas você sentindo neste momento?`) &
  Reports$Sindrome_Conjuntiva == FALSE &

  Reports$Sindrome_Diarreica == FALSE &

  Reports$Sindrome_IST == FALSE &

  Reports$Sindrome_Exantematica == FALSE &

  Reports$Sindrome_Gripal == FALSE

# Criando a coluna de Semana epidemiológica a partir da col. desde quando...  --------
Reports$Semana_Epi <- epiweek(Reports$`Desde quando está se sentindo mal?`)
Reports$Ano_Epi <- epiyear(Reports$`Desde quando está se sentindo mal?`)

#Dataframe de resumo de semana epi. por reports
Resumo_Semanal <- Reports|>
                mutate(Semana_Continua = paste(Ano_Epi, Semana_Epi, sep = "-"))|>
                group_by(Semana_Continua)|>
                summarise(Total_Gripal = sum(Sindrome_Gripal, na.rm = TRUE),
                Total_Diarreica = sum(Sindrome_Diarreica, na.rm = TRUE),
                Total_Conjuntiva = sum(Sindrome_Conjuntiva, na.rm = TRUE),
                Total_Exantematica = sum(Sindrome_Exantematica, na.rm = TRUE),
                Total_IST = sum(Sindrome_IST, na.rm = TRUE))|>
                pivot_longer(starts_with("Total_"), names_to = "Síndromes", values_to = "casos")

#criando o gráfico com barra única interativo com ggplotly
ggplotly(
  ggplot(Resumo_Semanal,
       aes(x = Semana_Continua, y = casos, group = Síndromes, fill = Síndromes))+
  labs(x = "SE", y = "Nº de Sintomáticos")+
  scale_x_discrete(labels = function(x) str_remove(x, ".*-"))+
  geom_col(),
  tooltip = c('x','y'))

#criando gráfico em pontos interativo
ggplotly(
ggplot()+
geom_point(data = Resumo_Semanal, mapping = aes(x = Semana_Continua, y = casos, color = Síndromes))+
  labs(x = "SE", y = "Nº de Sintomáticos")+
  scale_x_discrete(labels = function(x) str_remove(x, ".*-"))
  )

#criando o gráfico com barras p. cada síndrome
ggplot(Resumo_Semanal,
       aes(x = Semana_Continua, y = casos, group = Síndromes, fill = Síndromes))+
      labs(x = "SE", y = "Nº de Sintomáticos", fill = "Síndromes")+
      scale_x_discrete(labels = function(x) str_remove(x, ".*-"))+
  geom_col(position = "dodge")

