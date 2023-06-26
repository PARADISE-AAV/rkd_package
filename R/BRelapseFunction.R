#' @title BRelapse (DEPRECATED)
#' @author Yagmur Dogay
#' Version:
#' Date: 16-March-23
#' Objective: Inputing the Relapse patient
#'
#' @param RKD_data RKD data from Demographic Filter RKD Data
#' @return The data with the patient in relapse
#' @import dplyr
#' @import forcats
#' @export
BRelapseFunction <- function(RKD_data) {
  # Step 1: Flare: consider three columns:
  # 1a.	"Adjudicated probability of relapse," "Definite"/"high probability" as "Yes" and "no"/ "Possible" as "No"
  # 2.	For empty cells go to the "Do you think Vasculitis is relapsing in this encounter" and "Definite"/"high probability" as "Yes" and "Unknown"/"Possibly"/"BLANK" go to step 3
  # 3a.	"Disease activity since the last visit" = "Active OR Low disease activity" > "Yes" and "Remission"/"BLANK" > "No"
  # Step 1:
  data <- RKD_data
  m=nrow(data)
  for(j in 1:m){
      if(as.character(data$Adjudicated.probability.of.relapse[j])=="" & is.na(data$Adjudicated.probability.of.relapse[j])==F){
        data$Adjudicated.probability.of.relapse[j]=NA
      }
  }
  
  data$IntFlare <-
    with(data,
         ifelse(
           !is.na(`Adjudicated.probability.of.relapse`),
           `Adjudicated.probability.of.relapse`,
           ifelse(
             `Do.you.think.Vasculitis.is.relapsing.in.this.encounter` == "High Probability",
             `Do.you.think.Vasculitis.is.relapsing.in.this.encounter`,
             ifelse(
               `Do.you.think.Vasculitis.is.relapsing.in.this.encounter` == "Possibly",
               `Do.you.think.Vasculitis.is.relapsing.in.this.encounter`,
               ifelse(
                 `Do.you.think.Vasculitis.is.relapsing.in.this.encounter` == "No",
                 `Do.you.think.Vasculitis.is.relapsing.in.this.encounter`,
                 NA
               )
             )
           )
         ))
  #table(data$IntFlare)
  
  
  # Complete the missing in the "IntFlare" with "Disease activity since last return"
  data$IntFlare <-
    with(data,
         ifelse(
           !is.na(IntFlare),
           IntFlare,
           `Disease.activity.since.last.return`
         ))
  #table(data$IntFlare)
  
  
  # Coding the observation of flare
  
  data$Flare_Edit_1 <- forcats::fct_recode(
    data$IntFlare,
      "No" = "No Relapse",
      "Remission" = "No Relapse",
      "Possibly" = "Possible Relapse",
      "Possible" = "Possible Relapse",
      "Active" = "Definite Relapse",
      "Low disease activity" = "Possible Relapse",
      "Definite" = "Definite Relapse",
      "High Probability" = "Definite Relapse"
  )
  #table(data$Flare_Edit_1)
  
  
  # Step 2: determine the visit dates with interval less than 3 months of the date of diagnosis
  
  data <- data %>%
    mutate(
      Flare_Edit_2 = ifelse(
        `Interval.from.diagnosis..months.` <= 3 &
          Flare_Edit_1 == "Definite Relapse" |
          `Interval.from.diagnosis..months.` <= 3 &
          Flare_Edit_1 == "Possible Relapse" |
          `Interval.from.diagnosis..months.` <= 3 &
          is.na(Flare_Edit_1)
        ,
        "No Relapse",
        Flare_Edit_1
      )
    )
  #table(data$Flare_Edit_2)
  
  
  # Change definite and possible flares with distance form each others less than 60 days to remission
  # Step 3:
  # change the encounters' situation from 2 to 0
  data$`Date.Of.Visit` <-
    as.Date(data$`Date.Of.Visit`, "%d/%m/%Y")
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(Lagvisit = ifelse(
      is.na(difftime(
        `Date.Of.Visit`, lag(`Date.Of.Visit`), units = "days"
      )),
      "0",
      difftime(`Date.Of.Visit`, lag(`Date.Of.Visit`), units =
                 "days")
    ))
  data$Lagvisit <- as.numeric(data$Lagvisit)
  
  
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(
      Exc_Lagvisit = ifelse(
        Flare_Edit_2 == "Definite Relapse" &
          lag(Flare_Edit_2) == "Definite Relapse" &
          Lagvisit < 60,
        Lagvisit,
        NA
      )
    ) %>%
    mutate(Flare_Edit_3 = ifelse(!is.na(Exc_Lagvisit), "No Relapse", Flare_Edit_2))
  #sum(!is.na(data$Exc_Lagvisit))
  
  
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(
      Exc_Lagvisit0 = ifelse(
        Flare_Edit_3 == "Possible Relapse" &
          lag(Flare_Edit_3) == "Possible Relapse" &
          Lagvisit < 60,
        Lagvisit,
        NA
      )
    ) %>%
    mutate(Flare_Edit_3 = ifelse(!is.na(Exc_Lagvisit0), "No Relapse", Flare_Edit_3))
  #sum(!is.na(data$Exc_Lagvisit0))
  
  
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(
      Exc_Lagvisit1 = ifelse(
        Flare_Edit_3 == "Possible Relapse" &
          lead(Flare_Edit_3) == "Definite Relapse" &
          lead(Lagvisit) < 60,
        lead(Lagvisit),
        NA
      )
    ) %>%
    mutate(Flare_Edit_3 = ifelse(!is.na(Exc_Lagvisit1), "No Relapse", Flare_Edit_3))
  #sum(!is.na(data$Exc_Lagvisit1))
  #table(data$Flare_Edit_3)
  
  
  # Step 4: filling missing:
  # Determine the occasion of missing; missing between two remission consider as a remission
  
  data <- data %>%
    mutate(
      Flare_Edit_4 = ifelse(
        is.na(Flare_Edit_3) &
          lag(Flare_Edit_3) == "No Relapse" & lead(Flare_Edit_3) == "No Relapse",
        "No Relapse",
        Flare_Edit_3
      )
    )
  
  #
  # Making the criteria as a symptom of flare
  # CRP: if it is more  than 5 is a symptom
  
  data  <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(ExactCRP = ifelse(is.na(Flare_Edit_4), CRP, NA)) %>%
    mutate(SymCRP = ifelse(ExactCRP > 5, 1, 0))
  #table(data_5$SymCRP)
  
  
  # Creatinine: rising 20 percent
  
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(RisCr = ifelse(
      is.na(Flare_Edit_4),
      100 * (Creatinine - lag(Creatinine, 1)) / lag(Creatinine, 1),
      NA
    )) %>%
    mutate(SymCr = ifelse(RisCr > 20 , 1, 0))
  #table(data$SymCr)
  
  
  # ANCA more than 4x
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(ExactMPO = ifelse(
      is.na(Flare_Edit_4),
      100 * (`Anti.MPO.level` - lag(`Anti.MPO.level`)) / lag(`Anti.MPO.level`),
      NA
    )) %>%
    mutate(ExactPR3 = ifelse(
      is.na(Flare_Edit_4),
      100 * (`Anti.PR3.level` - lag(`Anti.PR3.level`)) / lag(`Anti.PR3.level`, 1),
      NA
    )) %>%
    mutate(PreANCA = ifelse(
      is.na(Flare_Edit_4) & lag(`ANCA.IF`) == "Negative",
      `ANCA.IF`,
      NA
    )) %>%
    mutate(ANCA = ifelse(!is.na(ExactMPO), ExactMPO, ExactPR3)) %>%
    mutate(SymANCA = ifelse(ANCA > 400, 1,
                            ifelse(
                              PreANCA == "P", 1,
                              ifelse(PreANCA == "C", 1, 0)
                            )))
  #table(data$SymANCA)
  
  
  # Uninalysis Blood: more than 2
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(ExactUniBlood = ifelse(is.na(Flare_Edit_4), `Uninalysis.Blood`, NA)) %>%
    mutate(SymUniBlood = ifelse(ExactUniBlood == ">=+3", 1, 0))
  #table(data$SymUniBlood)
  
  
  # Uninalysis Protein: more than 2
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(ExactUniPro = ifelse(is.na(Flare_Edit_4), `Uninalysis.Protein`, NA)) %>%
    mutate(SymUniPro = ifelse(ExactUniPro == ">=+3", 1, 0))
  #table(data$SymUniPro)
  
  
  # CD163: rising 20 percent "Urine sCD163 (ng/mmmol), Euroimmun"
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(RisCD163 = ifelse(
      is.na(Flare_Edit_4) & `Urine.sCD163..ng.mmmol...Euroimmun` > 250
      ,
      100 * (
        `Urine.sCD163..ng.mmmol...Euroimmun` - lag(`Urine.sCD163..ng.mmmol...Euroimmun`, 1)
      ) / lag(`Urine.sCD163..ng.mmmol...Euroimmun`, 1),
      NA
    )) %>%
    mutate(SymCD163 = ifelse(RisCD163 > 20 , 1, 0))
  #table(data$SymCD163)
  
  
  
  # The last number of patient with previous symptoms
  data$SumSym <-
    rowSums(data[, c("SymCRP",
                     "SymCr",
                     "SymANCA",
                     "SymUniBlood",
                     "SymUniPro",
                     "SymCD163")], na.rm = TRUE)
  
  data <- data %>%
    mutate(Flare_Edit_5 = ifelse(
      !is.na(Flare_Edit_4),
      Flare_Edit_4,
      ifelse(
        is.na(Flare_Edit_4) & SumSym > 2,
        "Definite Relapse",
        ifelse(
          is.na(Flare_Edit_4) & SumSym == "1",
          "No Relapse",
          ifelse(
            is.na(Flare_Edit_4) & SumSym == "2",
            "Possible Relapse",
            ifelse(
              is.na(Flare_Edit_4) &
                SymCRP == "0" &
                SymCr == "0" &
                SymANCA == "0" & SymUniBlood == "0" & SymUniPro == "0" &
                SymCD163 == "0",
              "No Relapse",
              NA
            )
          )
        )
      )
    ))
  table(data$Flare_Edit_5)
  
  
  # Repeat Step 3:
  # Change the encounters' situation from 2 to 0
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(
      Exc_Lagvisit_R = ifelse(
        Flare_Edit_5 == "Definite Relapse" &
          lag(Flare_Edit_5) == "Definite Relapse" & Lagvisit < 60,
        Lagvisit,
        NA
      )
    )
  #sum(!is.na(data$Exc_Lagvisit_R))
  # [1] 0
  
  
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(
      Exc_Lagvisit_R0 = ifelse(
        Flare_Edit_5 == "Possible Relapse" &
          lag(Flare_Edit_5) == "Possible Relapse" & Lagvisit < 60,
        Lagvisit,
        NA
      )
    )
  sum(!is.na(data$Exc_Lagvisit_R0))
  # [1]0
  
  
  # Change the possible relapse in the interval less than sixty days of definite relapse
  data <- data %>%
    group_by(`RKD.ID`) %>%
    mutate(
      Exc_Lagvisit_R1 = ifelse(
        Flare_Edit_5 == "Possible Relapse" &
          lead(Flare_Edit_5) == "Definite Relapse" &
          lead(Lagvisit) < 60,
        lead(Lagvisit),
        NA
      )
    )
  #sum(!is.na(data$Exc_Lagvisit_R1))
  # [1]0
  
  
  # Determine the occasion of missing; missing between two remission consider as a remission
  data <- data %>%
    mutate(
      Relapse = ifelse(
        is.na(Flare_Edit_5) &
          lag(Flare_Edit_5) == "No Relapse" &
          lead(Flare_Edit_5) == "No Relapse",
        "No Relapse",
        Flare_Edit_5
      )
    )
  
  
  data$Relapse_Code <-
    forcats::fct_recode(as.factor(data$Relapse),
              "No Relapse" = "0",
              "Possible Relapse" = "1",
              "Definite Relapse" = "2")
  #table(data$Relapse_Code)
  
  return(data)
  
}