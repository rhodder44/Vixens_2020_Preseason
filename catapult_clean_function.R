
##########################
# Catapult Clean Function
##########################

CleanCatapult <- function(df) {
  
  # Rename variables, seperate Name into name and drill variables
  clean1 <- df %>%
    mutate(Name  = str_replace(Name, "15 on - 15 off", "15 on")) %>%
    separate(col = Name, into = c("name", "drill"), sep = "\\-") %>%
    rename(playerload                 = Average.Player.Load..Session.,
           duration                   = Average.Duration..Session.,
           jumps                      = Jumps,
           jumps_min                  = Jumps.per.Minute,
           jump_ratio                 = Jump.Ratio,
           explosive_efforts          = Explosive.Efforts,
           explosive_efforts_min      = Explosive.Efforts.per.Minute,
           accelerations              = Accelerations,
           accelerations_min          = Accelerations.Per.Min,
           decelerations              = Decelerations,
           decelerations_min          = Decelerations.Per.Min,
           change_of_directions       = Change.Of.Directions,
           change_of_directions_min   = Change.of.Directions.per.Minute)  %>%
    mutate_if(is.numeric, round, digits = 2)
  
  # reformat duration into usable minutes variable
  clean2 <- separate(clean1, duration, c("hours", "minutes", "seconds"), sep = ":")
  clean2$hours   <- as.numeric(clean2$hours)     
  clean2$minutes <- as.numeric(clean2$minutes)
  clean2$seconds <- as.numeric(clean2$seconds)
  
  clean3 <- clean2 %>%
    mutate(total_seconds = (minutes * 60) + seconds) %>%
    mutate(duration      = round((total_seconds / 60) + (hours * 60))) %>%
    select(c(name, drill, duration, everything())) %>% 
    select(-c(hours, minutes, seconds, total_seconds))
  
  return(clean3)
  
}