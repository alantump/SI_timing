#exclude subjects with avgConfidence = 100%

filterConfidence <- finalSheet %>% group_by(playerNr) %>% 
  summarise(avgConfidence= mean(confidence)) %>% filter(avgConfidence==100)

finalSheet <- finalSheet %>% filter(!playerNr %in% filterConfidence$playerNr[filterConfidence$avgConfidence==100])


#exclude subjects that were too late more than 20 times

filterTooLate <- finalSheet %>% group_by(playerNr) %>% summarise(instancesTooLate = sum(tooLate)) %>% filter(instancesTooLate>10)

finalSheet <- finalSheet %>% filter(!playerNr %in% filterTooLate$playerNr[filterTooLate$instancesTooLate>10])


#exlcude accuracy <= 50% ---> doesnt work check with Alan

filterAccuracy <- finalSheet %>% group_by(playerNr) %>% summarise(accuracy = mean(correctChoice, na.rm = T)) %>% filter(accuracy<=0.50)

finalSheet <- finalSheet %>% filter(!playerNr %in% filterAccuracy$playerNr[filterAccuracy$accuracy<=0.50])


#exclude subjects that didnt see all squares

filterNoSquare <- finalSheet %>% group_by(playerNr) %>%  
  mutate(allSquaresShown2 = case_when(squareCheck[period==120]==1~"yes",squareCheck[period==120]==2~"no",is.na(squareCheck[period==120]) ~ "not checked"))  %>% 
summarise(allSquaresShown = allSquaresShown2[period==120]) %>% filter(allSquaresShown=="no")

finalSheet <- finalSheet %>% filter(!playerNr %in% filterNoSquare$playerNr[filterNoSquare$allSquaresShown=="no"])

