## static analysis
## todo : simulations

## input : vector of the balls
consecutive_nbr <- function(balls) {
        nbr_balls <- length(balls)
        balls <- sort(balls)
        
        #list of orders
        orders <- vector("numeric")       
        # order of consecutive numbers
        order <- 1      
        i <- 1
        while (i < nbr_balls) {
                if (abs(balls[i] - balls[i + 1]) == 1) {
                        order <- order + 1
                }
                else if (order > 1) {
                        orders[length(orders) + 1] <- order
                        order <- 1
                }
                i <- i + 1
        }
        
        if (order > 1) {
                orders[length(orders) + 1] <- order
        }
        
        orders
}


## At most 2 orders of consecutive numbers
## thus the default normalized orders of consecutive nbr should
## be (1, 1)
## descending orders the orders of consecutive nbr
## input is a numeric vector
norm_conseq_nbr <- function(conseq_nbr) {
        normed_conseq_nbr <- conseq_nbr
        while (length(normed_conseq_nbr) < 2) {
                normed_conseq_nbr <- append(normed_conseq_nbr, 1)
        }
        
        sort(normed_conseq_nbr, decreasing = TRUE)
}


## convert a list of pairs to a data frame
## first and second element of the pair are put in separate column
conv_list_to_df <- function(list_pairs) {
        matrix <- numeric()
        count <- 0
        for (i in list_pairs) {
                matrix <- rbind(matrix, i, deparse.level = 0)
        }
        
        data.frame(matrix)
}


## input : vector of the balls
## output: vector (even, odd)
split_even_odd <- function(balls) {
        odd <- sum(balls %% 2)
        even <- length(balls) - odd
        c(even, odd)
}


## count how many couple (n, m) in the draw
## input : 
##        - data frame of the balls
##        - couple (n, m) to count
count_couple <- function(balls, n, m) {
        found <- 0
        draws <- nrow(balls)
        i <- 1
        while(i <= draws) {
                bb <- balls[i, ]
                found_n <- FALSE
                found_m <- FALSE
                j <- 1
                ll <- length(bb)
                
                # for efficiency, stop the loop when found instead
                # of going through each element
                while ((!found_n | !found_m) & j <= ll) {
                        if (!found_n & bb[j] == n) {
                                found_n <- TRUE
                        }
                        if (!found_m & bb[j] == m) {
                                found_m <- TRUE
                        }
                        j <- j + 1
                }
                
                if (found_n & found_m) {
                        found <- found + 1
                }
                i <- i + 1
        }
        found
}


count_couple <- function(balls) {
        matrix <- matrix(0, ncol = 49, nrow = 49)
        i <- 1
        while (i <= nrow(balls)) {
                bb <- balls[i, ]
                ll <- length(bb)
                j <- 1
                while (j < ll) {
                        k  <- j + 1
                        while (k <= ll) {
                                n1 <- min(bb[[j]], bb[[k]])
                                n2 <- max(bb[[j]], bb[[k]])
                                matrix[n1, n2] <- matrix[n1, n2] + 1
                                k <- k + 1
                        }
                        j <- j + 1
                }
                i <- i + 1
        }
        
        # create a data frame out of the results containing the couples
        # occurings and their number of appearance
        df <- data.frame(couples = character(), occurings = numeric(), 
                         stringsAsFactors = FALSE)
        couple_idx <- 1
        for (i in 1:nrow(matrix)) {
                for (j in i:ncol(matrix)) {
                        if (i == j) {
                                next
                        }
                        
                        df[couple_idx, ] <- list(sprintf("(%d, %d)", i, j), 
                                              matrix[i, j])
                        couple_idx <- couple_idx + 1
                }
        }
        
        invisible(df)
}


conv_to_decimal <- function(draw) {
        for (i in 1:nrow(draw)) {
                draw[i, ] <- draw[i, ] %/% 10
        }
        draw
}


sort_decimal_patterns <- function(dec_balls) {
        df <- data.frame(boule_1 = numeric(), boule_2 = numeric(), boule_3 = numeric(),
                         boule_4 = numeric(), boule_5 = numeric(), occurings = numeric())
        records <- 0
        
        for (i in 1:nrow(dec_balls)) {
                dec <- sort(dec_balls[i, ])
                idx <- which(df$boule_1 == dec[[1]] & df$boule_2 == dec[[2]] & 
                             df$boule_3 == dec[[3]] & df$boule_4 == dec[[4]] & 
                             df$boule_5 == dec[[5]])
                if (length(idx) > 0) {
                        df[idx, 6] <- df[idx, 6] + 1
                }
                else {
                        records <- records + 1
                        df[records, ] <- c(dec[1], dec[2], dec[3], dec[4], 
                                           dec[5], 1)
                }
                
        }
        
        invisible(df)
}


check_winning_double <- function(draws) {
        for (i in 1:nrow(draws)) {
                draws[i, ] <- sort(draws[i, ])
        }
        
        for (i in 1:nrow(draws)) {
                d <- draws[i, ]
                idx <- which(draws$boule_1 == d[[1]] & draws$boule_2 == d[[2]] & 
                             draws$boule_3 == d[[3]] & draws$boule_4 == d[[4]] &
                             draws$boule_5 == d[[5]])
                             
                if (length(idx) == 0) {
                        print("a problem occured")
                }
                else if (length(idx) > 1) {
                        print(d)
                }
        }
}


count_conseq_lucky <- function(lucky_nbrs) {
        df <- data.frame(conseq = numeric(), occurings = numeric())
        conseq <- numeric(10)
        occurings <- numeric(10)
        conseq_temp <- 1
        luckynbr_temp <- 1
        
        for (i in lucky_nbrs) {
                if (i != luckynbr_temp) {
                        if (conseq[luckynbr_temp] < conseq_temp) {
                                conseq[luckynbr_temp] <- conseq_temp
                                occurings[luckynbr_temp] <- 1
                        }
                        else if (conseq[luckynbr_temp] == conseq_temp) {
                                occurings[luckynbr_temp] <- occurings[luckynbr_temp] + 1
                        }
                        luckynbr_temp <- i
                        conseq_temp <- 1
                }
                else {
                        conseq_temp <- conseq_temp + 1
                }
        }
        
        data.frame(conseq, occurings)
}


count_conseq_seq <- function(lucky_nbr, len) {
        ll <- list()
        # position where the sequence starts
        ps <- 1
        
        while (ps < length(lucky_nbr) - len) { # / 2 in the first time
                seq <- lucky_nbr[ps:(ps + len - 1)]
                
                # position to look for the sequence
                pcheck <- ps + 1
                count <- 1
                while (pcheck < length(lucky_nbr) - len) {
                        found <- TRUE
                        for (i in 0:(len - 1)) {
                                if (lucky_nbr[pcheck + i] != seq[i + 1]) {
                                        found <- FALSE
                                }
                        }
                        if (found) {
                                count <- count + 1
                        }
                        pcheck <- pcheck + 1
                }
                
                if (count > 1) {
                        ll <- append(ll, list(list(seq, count)))
                }
                
                ps <- ps + 1
        }
        ll
}


calc_avg_lucky_appearing <- function(lucky_num) {
        appearings <- numeric(10)
        averages <- list(vector())

        
        for (i in lucky_num) {
                if (is.null(averages[i][[1]])) {
                        averages[[i]] <- c(appearings[i])
                }
                else {
                        averages[[i]] <- append(averages[[i]], appearings[i])       
                }
                
                for (j in 1:10) {
                        if (j == i) {
                                appearings[j] <- 0
                        }
                        else {
                                appearings[j] <- appearings[j] + 1
                        }
                }
                
        }
        averages
}



calc_avg_balls_appearing <- function(draws) {
        
}

# generate_dec_pattern <- function() {
#         
# }


## returns the indexes of the positive rows
# multiple <- function(balls) {
#         multiple <- 0
#         i <- 1
#         while (i < nbr_balls) {
#                 j <- i + 1
#                 while (j <= nbr_balls) {
#                         if (balls[j] %% balls[i] == 0) {
#                                 multiple <- multiple + 1
#                         }
#                         j <- j + 1
#                 }
#                 i <- i + 1
#         }
#         multiple
# }

analyze <- function() {
        ## Cleaning data
        # source from www.fdj.fr
        file <- read.csv("nouveau_loto.csv", header = TRUE, sep = ';')
        
        # get the columns indexes for every ball
        nbr_draw <- nrow(file)
        colIndexes <- grep("boule_", colnames(file))
        nbr_balls <- length(colIndexes)
        
        all_balls <- file[, colIndexes]
        lucky_num <- file$numero_chance
        
        # consecutive numbers
        conseq_res <- apply(all_balls, 1, consecutive_nbr)
        conseq_normed <- lapply(conseq_res, norm_conseq_nbr)
        conseq_df <- conv_list_to_df(conseq_normed)
        names(conseq_df) <- c("most_conseq", "least_conseq")
        
        # even / odd
        even_odd <- apply(all_balls, 1, split_even_odd)
        # convert matrix to list
        even_odd <- split(even_odd, col(even_odd))
        even_odd_df <- conv_list_to_df(even_odd)
        names(even_odd_df) <- c("even", "odd")
        
        balls <- data.frame(all_balls, conseq_df, even_odd_df)
        
        ## Analysing data -- the whole idea being to find a pattern in the randomness
        ## of the draw / results
        
        # wining balls having multiple consecutive numbers
        sprintf("\nTotal draw : %d", nbr_draw)
        cat("\n2 consecutive numbers : ", sum(balls$most_conseq == 2))
        cat("\n3 consecutive numbers :", sum(balls$most_conseq == 3))
        cat("\n4 consecutive numbers :", sum(balls$most_conseq == 4))
        cat("\n5 consecutive numbers :", sum(balls$most_conseq == 5))
        
        cat("\n2+2 consecutive numbers : ", sum(balls$most_conseq == 2 & balls$least_conseq == 2))
        cat("\n3+2 consecutive numbers : ", sum(balls$most_conseq == 2 & balls$least_conseq == 2))
        
        # wining balls being twice another ball
        ## useless
        
        # even / odd balance
        cat("\neven ball average : ", mean(balls$even))
        cat("\nodd ball average : ", mean(balls$odd))
        cat("\nonly even balls : ", sum(balls$even == 5))
        cat("\n4 even balls : ", sum(balls$even == 4))
        cat("\n3 even balls : ", sum(balls$even == 3))
        
        cat("\nonly odd balls : ", sum(balls$odd == 5))
        cat("\n4 odd balls : ", sum(balls$odd == 4))
        cat("\n3 odd balls : ", sum(balls$odd == 3))
        
        
        # mean of balls
        cat("\naverage ball 1 : ", mean(balls$boule_1))
        cat("\naverage ball 2 : ", mean(balls$boule_2))
        cat("\naverage ball 3 : ", mean(balls$boule_3))
        cat("\naverage ball 4 : ", mean(balls$boule_4))
        cat("\naverage ball 5 : ", mean(balls$boule_5))
        
        # statistics about couples
        couples <- count_couple(all_balls)
        ordered_couples <- couples[order(couples$occurings, decreasing = TRUE), ]
        cat("\n10 most appearing coulpes : ")
        cat("\n")
        head(ordered_couples, 10)
        cat("\n10 least appearing couple : ")
        cat("\n")
        tail(ordered_couples, 10)
        
        # decimal categories
        dec_balls <- conv_to_decimal(all_balls)
        sort_dec_balls <- sort_decimal_patterns(dec_balls)
        sort_dec_balls <- sort_dec_balls[order(sort_dec_balls$occurings, decreasing = TRUE),]
        cat("5 most appearing categories :")
        head(sort_dec_balls, 5)
        cat("5 least appearing categories :")
        tail(sort_dec_balls, 5)
        cat("average of decimal category : ", mean(rowMeans(dec_balls)))
        # todo do it the all the possible combination to detect those being 0

        # draw coming out twice
        cat("draw that came out more than once : ")
        check_winning_double(all_balls)



        # lucky number
		# from how many elements in a row a pattern is broken
        cat("most occurings of consecutive lucky num : ")
        count_conseq_lucky(lucky_num)
        cat("lucky num sequence occuring more than once :")
        l5 <- count_conseq_seq(lucky_num, 5)   
        cat("sequence of 5 balls :", length(l5)
        l5
        l4 <- count_conseq_seq(lucky_num, 4)        
        cat("sequence of 4 balls :", length(l4))
        l4
        l3 <- count_conseq_seq(lucky_num, 3)        
        cat("sequence of 3 balls :", length(l3))
        l3
        
        # average of number appearing
        avg_lucky <- calc_avg_lucky_appearing(lucky_num)
        cat("Average lucky num appearance : ")
        lapply(avg_lucky, mean))
        cat("min number of draws before appearing again :")
        lapply(avg_lucky, min)
        cat("max number of draws before appearing again :")
        lapply(avg_lucky, max)
        
        
		# todo : tendencies for each category, ex : 10s, 20s, other?
		# check how next batch appear compared to average and how average is evolving
		# average of ball numbers / batch and stability over time and if equilibrium is maintained

}


report()



