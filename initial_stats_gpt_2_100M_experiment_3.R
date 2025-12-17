library(lme4)
library(lmerTest)
library(dplyr)


setwd("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/results_experiment_3")
### change this file according to the location of this folder. 

# Load data
dat <- read.csv("results_experiment_3_gpt_2_100M.csv", check.names = TRUE)

# Fix categories and factors.
# Creased two new columns for the conditions Regularity and Plurality. 
dat <- dat %>% 
  mutate(
    regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular"),
    plurality  = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    regularity = factor(regularity, levels = c("Regular", "Irregular")),
    plurality  = factor(plurality,  levels = c("Singular", "Plural")) 
  )
# Taking the example of the first line: regularity = ifelse(grepl("Irregular", Category), "Irregular", "Regular")
# regularity = .... creating/overwriting the column regularity.
# ifelse(test, value_if_TRUE, value_if_FALSE): vectorized “if”.
# grepl("Irregular", Category): checks each row’s Category text; returns TRUE if it contains the substring "Irregular", else FALSE.
# "Irregular", "Regular"  what gets assigned when TRUE,FALSE in the "if", in the column of the regularity. 


# Create 'set' and 'pair'. Creates two new columns for the set and pair. 
dat <- dat %>%
  mutate(
    set = case_when(
      Non.Head %in% c("goose","geese","swan","swans") ~ "set_goose_swan",
      Non.Head %in% c("ox","oxen","cow","cows") ~ "set_ox_cow",
      Non.Head %in% c("louse","lice","flea","fleas") ~ "set_louse_flea",
      Non.Head %in% c("mouse","mice","rat","rats") ~ "set_mouse_rat",
      Non.Head %in% c("foot","feet","leg","legs") ~ "set_foot_leg",
      Non.Head %in% c("tooth","teeth","bone","bones") ~ "set_tooth_bone",
      Non.Head %in% c("child","children","adult","adults") ~ "set_child_adult",
      Non.Head %in% c("woman","women","girl","girls") ~ "set_woman_girl",
      Non.Head %in% c("man","men","boy","boys") ~ "set_man_boy",
      Non.Head %in% c("salesman","salesmen","retailer","retailers") ~ "set_salesman_retailer",
      Non.Head %in% c("nobleman","noblemen","aristocrat","aristocrats") ~ "set_nobleman_aristocrat",
      Non.Head %in% c("boatman","boatmen","shipmate","shipmates") ~ "set_boatman_shipmate",
      Non.Head %in% c("craftsman","craftsmen","labourer","labourers") ~ "set_craftsman_labourer",
      Non.Head %in% c("fireman","firemen","lifeguard","lifeguards") ~ "set_fireman_lifeguard",
      
      TRUE ~ NA_character_ # for rows that don't match the search above. 
    )
  )
# %in% : Is this value in this list?
# ~ : if the left side is true, assign the right side in the new column. 


### MODEL SELECTION LOGIC

# Define Random Slopes
rs <- c("",
        " + regularity",
        " + plurality",
        " + regularity + plurality",
        " + regularity * plurality")

rs_set  <- sapply(rs, \(x) paste0("(1", x, " | set)")) |> unname()
rs_head <- sapply(rs, \(x) paste0("(1", x, " | Head)")) |> unname()
# sapply applies a function to each element of a vector or list. 
# rs is the vector which is looped over.
#  \(x) paste0("(1", x, " | set)")) is an anonym function. \(x) is the same as function(x): it means function with the argument x.  
# unnamed removves some labels. Later, check better why it is important. 


###Creating all the combinations, ready to be tested. 

rs_both <- expand.grid(rs_set, rs_head) # 
# Generates all combinations of the random-effects terms for set and for Head (5 × 5 = 25 rows).

rs_both <- apply(rs_both, 1, \(x) paste(x, collapse = " + "))
# For each row (that’s the 1), it joins the two terms into one string.
  
fr_both <- sapply(rs_both, \(x) paste0("Surprisal.head ~ 1 + regularity * plurality + ", x) |> as.formula()) |> unname()
# For each combined random-effects string, it builds the full model formula (fixed part + that random part)
# and converts it into an R formula object. So you get a vector/list of 25 formulas ready to feed into lmer().

### Convergence and singularity check. 
hasConverged <- function (mm) {
  if ( !inherits(mm, "merMod")) stop("Error: must pass a lmerMod object")
  retval <- NULL
  if(is.null(unlist(mm@optinfo$conv$lme4))) {
    retval = 1
  }
  else {
    if (isSingular(mm)) {
      retval = 0
    } else {
      retval = -1
    }
  }
  return(retval)
}


fit_model <- function(x){
  m <- lmer(x, dat, REML = FALSE) 
  
  output <- list(summary = summary(m),
                 convergence = hasConverged(m),
                 AIC = AIC(m))
  return(output)
}

results_list <- lapply(fr_both, fit_model)

results_df <- do.call(rbind, lapply(results_list, function(x) data.frame(AIC = x$AIC, convergence = x$convergence)))
results_df$formula <- as.character(fr_both)
results_df <- results_df[order(results_df$AIC), ]

print(results_df)


###################################################################################################################
# Trials

final_model <- lmer(
  Surprisal.head ~ 1 + regularity * plurality + (1 | set) + (1 + plurality | Head), 
  data = dat, 
  REML = TRUE
)

summary(final_model)

final_model <- lmer(
  Surprisal.head ~ 1 + regularity / plurality + (1 | set) + (1 + plurality_num * regularity_num || Head), 
  data = dat,
  REML = TRUE 
)

summary(final_model)

##########################

final_model_2 <- lmer(
  Surprisal.head ~ 1 + regularity * plurality + (1 + regularity | set) + (1 + plurality | Head), 
  data = dat,
  REML = TRUE 
)

summary(final_model_2)


final_model_2 <- lmer(
  Surprisal.head ~ 1 + regularity / plurality + (1 + regularity | set) + (1 + plurality | Head), 
  data = dat,
  REML = TRUE 
)

summary(final_model_2)


###################################

final_model <- lmer(
  Surprisal.head ~ 0 + regularity + (1 | set) + (1 | Head), 
  data = dat, 
  REML = TRUE
)

summary(final_model)



