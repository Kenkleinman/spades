splitarpa <- function(sim) {
  sim$respire %>% group_by(newid, Term, `Class Session Code`, `Tuition Residency Group`  ) %>%
    summarise(ncredits = sum(`Credit Hours`)) %>% 
    pivot_wider(names_from = c(Term, `Class Session Code`), 
              names_sep = ".", values_from = ncredits) %>%
    mutate(across(where(is.numeric),coalesce,0)) %>%
    mutate(spring.uww = sum(`Spring 2019.C1`, `Spring 2019.CD1`), 
         fall.uww = sum(`Fall 2018.C1`, `Fall 2018.CD1`  ),
         splitter.fall = (`Fall 2018.U1` < 12) & fall.uww > 0 & 
           (`Fall 2018.U1` + fall.uww + `Fall 2018.M1` > 11),
         splitter.spring = (`Spring 2019.U1` < 12) & spring.uww > 0 & 
           (`Spring 2019.U1` + spring.uww + `Spring 2019.M1` > 11),
         arpa.fall = (`Fall 2018.U1` > 11 & fall.uww > 0),
         arpa.spring = (`Spring 2019.U1` > 11) & spring.uww > 0) %>%
    filter(splitter.fall + splitter.spring > 0) -> 
    psps.wuww

  splitter.fall <- table(psps.wuww$`Tuition Residency Group`,psps.wuww$splitter.fall)[,2]
  splitter.spring <- table(psps.wuww$`Tuition Residency Group`,psps.wuww$splitter.spring)[,2]
  arpa.fall <- table(psps.wuww$`Tuition Residency Group`,psps.wuww$arpa.fall)[,2]
  arpa.spring <- table(psps.wuww$`Tuition Residency Group`,psps.wuww$arpa.spring)[,2]

  return(data.frame(splitter.fall, splitter.spring, arpa.fall, arpa.spring))
}



