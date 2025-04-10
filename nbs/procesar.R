library(tidyverse)
impago <- read.csv(file="https://juandmontoro.github.io/icml/data/credit_card.csv")
names(impago) <- tolower(names(impago))

impago <- impago %>% 
  mutate(respuesta=pay_0>0,
         tratamiento=pay_6>0,
         importeAbril=bill_amt6,
         importeMayo=bill_amt5,
         importeJunio=bill_amt4,
         importeJulio=bill_amt3,
         importeAgosto=bill_amt2)

# descartamos variables que no utilizaremos
impago <- impago %>% select(-starts_with("pay_"),-starts_with("bill_amt"),
                            -default.payment.next.month,-id)
names(impago)
variables <- names(impago)[6:8]
names(impago) <- c("limite_bal","genero","edu","ecivil","edad",variables)
names(impago)

impago$ecivil <- as.factor(impago$ecivil)
impago$edu <- as.factor(impago$edu)
