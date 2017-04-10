#R code for Fantasy Football (soccer) 
#forked https://gist.github.com/gardenberg/e90ad05bc53754483c71baa6ee6c13c7

teamchooser = function(temp,incremental=FALSE){
        library(lpSolve)
        library(dplyr)
        if(incremental==FALSE){
                #bør først ha en sjekk av at temp har temp$element_type, temp$team, temp$total_points, temp$now_cost
                #Setter først felles constraints mm
                #basert på https://gist.github.com/gardenberg/e90ad05bc53754483c71baa6ee6c13c7
                #prøve https://github.com/gardenberg/penalty/blob/master/fantasy_football_optimiser/optimiser.R ser mer effektiv ut?
                # CONSTRAINTS:
                num_goalkeepers <- 2
                num_defenders <- 5
                num_midfielders <- 5
                num_strikers <- 3
                max_team_cost <- 1000
                # set "max_player_from_a_team <- 15" to ignore this constraint
                max_player_from_a_team <- 3 	
                # Create constraint vectors
                goalkeepers <- temp$element_type==1
                goalkeepers[goalkeepers==T] <- 1
                goalkeepers[goalkeepers==F] <- 0
        
                defenders <- temp$element_type==2
                defenders[defenders==T] <- 1
                defenders[defenders==F] <- 0
        
                midfielders <- temp$element_type==3
                midfielders[midfielders==T] <- 1
                midfielders[midfielders==F] <- 0
        
                strikers <- temp$element_type==4
                strikers[strikers==T] <- 1
                strikers[strikers==F] <- 0
        
                # constraint for max # players from a team
                clubs <- sort(unique(temp$team))
        
                team_constraint_vector <- c()
                team_constraint_dir <- c()
                team_constraint_rhs <- c()
        
                for (i in 1:length(clubs)){
                        temp_2 <- temp$team==as.character(clubs[i])
                        temp_2[temp_2==T] <- 1
                        temp_2[temp_2==F] <- 0
                
                        team_constraint_vector <- c(team_constraint_vector, temp_2)
                        team_constraint_dir <- c(team_constraint_dir, "<=")
                        team_constraint_rhs <- c(team_constraint_rhs, max_player_from_a_team)
                }
        
                f.con <- matrix (c(goalkeepers, defenders, midfielders, strikers, temp$now_cost, team_constraint_vector), nrow=(5+length(clubs)), byrow=TRUE)
                f.dir <- c("=", "=", "=", "=", "<=", team_constraint_dir)
                f.rhs <- c(num_goalkeepers, num_defenders, num_midfielders, num_strikers, max_team_cost, team_constraint_rhs)
                # OBJECTIVE FUNCTION
                f.obj <- temp$total_points
                x <- lp ("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)
                beste_lag = arrange(temp[which(x$solution==1),],element_type,desc(total_points))
                temp$solution_full = x$solution
                list_optimized = list(x,temp,beste_lag)
                return(list_optimized)
        }
        if(incremental==TRUE){
                # CONSTRAINTS:
                num_goalkeepers <- 2
                num_defenders <- 5
                num_midfielders <- 5
                num_strikers <- 3
                num_firstteamers = 13
                max_team_cost <- 1000
                # set "max_player_from_a_team <- 15" to ignore this constraint
                max_player_from_a_team <- 3 	
                # Create constraint vectors
                goalkeepers <- temp$element_type==1
                goalkeepers[goalkeepers==T] <- 1
                goalkeepers[goalkeepers==F] <- 0
                
                defenders <- temp$element_type==2
                defenders[defenders==T] <- 1
                defenders[defenders==F] <- 0
                
                midfielders <- temp$element_type==3
                midfielders[midfielders==T] <- 1
                midfielders[midfielders==F] <- 0
                
                strikers <- temp$element_type==4
                strikers[strikers==T] <- 1
                strikers[strikers==F] <- 0
                
                firstteamers <- temp$first_team ==1
                firstteamers[firstteamers==T] <- 1
                firstteamers[firstteamers==F] <- 0
                
                # constraint for max # players from a team
                clubs <- sort(unique(temp$team))
                
                team_constraint_vector <- c()
                team_constraint_dir <- c()
                team_constraint_rhs <- c()
                
                for (i in 1:length(clubs)){
                        temp_2 <- temp$team==as.character(clubs[i])
                        temp_2[temp_2==T] <- 1
                        temp_2[temp_2==F] <- 0
                        
                        team_constraint_vector <- c(team_constraint_vector, temp_2)
                        team_constraint_dir <- c(team_constraint_dir, "<=")
                        team_constraint_rhs <- c(team_constraint_rhs, max_player_from_a_team)
                }
                
                f.con <- matrix (c(goalkeepers, defenders, midfielders, strikers, firstteamers, temp$now_cost, team_constraint_vector), nrow=(6+length(clubs)), byrow=TRUE)
                f.dir <- c("=", "=", "=", "=", "=", "<=", team_constraint_dir)
                f.rhs <- c(num_goalkeepers, num_defenders, num_midfielders, num_strikers, num_firstteamers, max_team_cost, team_constraint_rhs)
                # OBJECTIVE FUNCTION
                f.obj <- temp$total_points
                x <- lp ("max", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)
                x
                beste_lag = arrange(temp[which(x$solution==1),],element_type,desc(total_points))
                temp$solution_incremental = x$solution
                list_optimized = list(x,temp,beste_lag)
                return(list_optimized)
        }
}