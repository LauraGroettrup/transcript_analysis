#Mittelwerte pro gruppe ausgeben
dialogTable_gender_role %>%
  group_by(Gender_From, Gender_To, Season) %>%
  get_summary_stats(Sentiment, type = "mean_sd")

dialogTable_gender_role %>%
  group_by(Season) %>%
  get_summary_stats(Sentiment, type = "mean_sd")

#plots davon
plot_gender_from <- ggboxplot(dialogTable_gender_role, x = "Gender_From", y = "Sentiment", add = "point")
plot_gender_from
plot_gender_to <- ggboxplot(dialogTable_gender_role, x = "Gender_To", y = "Sentiment", add = "point")
plot_gender_to