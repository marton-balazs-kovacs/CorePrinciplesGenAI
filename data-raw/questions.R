# TODO: Add transform questions from spreadsheets
# Checklist questions
questions <- jsonlite::read_json(path = "inst/app/www/questions.json")

source("R/assign_id.R")

# Get UI elements for the checklist
# Use the assign_id() func if there are unnamed functions
checklist <- rlang::list2(
  headList = questions$Head,
  sectionsList = questions$Sections,
  answerList = questions$Answers
)

# Save list as internal data
usethis::use_data(checklist, overwrite = TRUE, internal = TRUE)

# Get all the possible question dependencies for testing
depends <-
  map(long$sectionsList, ~flatten(.x["Questions"])) %>% 
  flatten(.) %>% 
  flatten(.) %>% 
  keep(.p = stringr::str_detect(names(.), "Depends"))
