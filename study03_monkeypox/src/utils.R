library(glue)
library(broom)
library(broom.mixed)
library(data.table)


increase_digits <- function(x, tidy_estimates, digits) {
    if (sum(as.numeric(x) == 0)) {
        idx <- which(as.numeric(x) == 0)
        temp_fmt <- paste0('%#.', digits + 1, 'f')
        temp_b <- sprintf(tidy_estimates$estimate[idx], fmt = temp_fmt)
        x[idx] <- temp_b
        if (sum(as.numeric(x) == 0)) {
            idx <- which(as.numeric(x) == 0)
            zeros <- rep(0, length(idx))
            temp_fmt <- paste0('%#.', digits, 'f')
            x[idx] <- sprintf(zeros, fmt = temp_fmt)
        }
        return(x)
    } else {
        return(x)
    }
    
}


parse_format <- function(output_format) {
    out <- list(b = FALSE, se = FALSE, lower = FALSE, upper = FALSE, statistic = FALSE, p = FALSE, df = FALSE, es = FALSE)
    
    for (n in names(out)) {
        if (grepl(paste0("{", n, "}"), output_format, fixed = TRUE)) {
            out[[n]] <- TRUE
        }
    }
    return(out)    
}


check_rounding <- function(x) {
    x_temp <- gsub("0", "", x)
    x_temp <- gsub("-.", "", x_temp)
    if (sum(x_temp == "")) {
        idx <- which(x_temp == "")
        x[idx] <- gsub("-", "", x[idx])
    }
    return(x)
}

get_param <- function(param, df_params, fmt, digits, adjust) {
    if (!param %in% names(df_params)) {
        df_params[, (param) := NA]
    }
    
    # degrees of freedom is sometimes called parameter
    if (param == "df" & ("parameter" %in% names(df_params))) {
        df_params[, (param) := parameter]
    }
    
    p_fmt <- sprintf(df_params[, get(param)], fmt = fmt)
    if (adjust) {
        p_fmt <- increase_digits(p_fmt, df_params, digits)    
    }
    p_fmt <- check_rounding(p_fmt)
    return(p_fmt)
}


sumh <- function(model, digits = 2, conf.int = TRUE, conf.level = 0.95, pval_digits = 3, table = FALSE, verbose = FALSE, adjust = FALSE, exponentiate = FALSE) {
    if (verbose) {
        message("Available parameters: {b}, {se}, {lower}, {upper}, {statistic}, {df}, {p}, {es}")
    }
    
    dt0 <- data.table(tidy(model, conf.int = conf.int, conf.level = conf.level, effects = "fixed", exponentiate = exponentiate))
    if (table) return(dt0)
    
    output_format <- tryCatch(get("output_format", envir = .GlobalEnv), 
                              error = function(e) {
                                  output_format = "b = {b} [{lower}, {upper}], p = {p}"
                              }
    ) 
    out_fmt <- parse_format(output_format)
    
    if (verbose & !table) message(paste0("Format: ", output_format))
    
    fmt <- paste0('%#.', digits, 'f')
    
    # get requested parameters
    if (out_fmt$b) {
        b <- get_param("estimate", dt0, fmt, digits, adjust)    
    }
    if (out_fmt$se) {
        se <- get_param("std.error", dt0, fmt, digits, adjust)
    }
    if (out_fmt$statistic) {
        statistic <- get_param("statistic", dt0, fmt, digits, adjust)
    }
    
    if (out_fmt$df) {
        df <- get_param("df", dt0, "%.0f", 0, adjust)
    }
    
    if (conf.int & out_fmt$lower) {
        lower <- get_param("conf.low", dt0, fmt, digits, adjust)
    }
    if (conf.int & out_fmt$upper) {
        upper <- get_param("conf.high", dt0, fmt, digits, adjust)
    } 
    
    if (out_fmt$p) {
        p_fmt <- paste0('%#.', pval_digits, 'f')
        p <- get_param("p.value", dt0, p_fmt, digits, adjust)
        p <- gsub("0.", ".", p, fixed = TRUE)
        p <- ifelse(dt0$p.value < 0.001, ".001" , p)
        idx_smallp <- which(dt0$p.value < 0.001)
    }
    
    res <- glue(output_format)
    dt1 <- data.table(term = dt0$term, res = res)
    
    # fix pvalues
    if (out_fmt$p) {
        if (length(idx_smallp) > 0) {
            ps <- dt1[idx_smallp, res]
            ps <- gsub("= .001", "< .001", ps)
            ps <- gsub("=.001", "<.001", ps)
            dt1 <- as.data.frame(dt1)
            dt1[idx_smallp, "res"] <- ps
        }
    }
    
    return(data.table(dt1))
}



# 
# output_format <- "b = {b} [{lower}, {upper}], p = {p}"
# output_format <- "p = {p}, beta = {b} [lower_puneet = {lower}], sgtandard errorr rrrr = {se}"
# m <- lm(mpg ~ vs, mtcars)
# sumh(m, pval_digits = 5)
# output_format <- "b = {b} [{lower}, {upper}]"
# sumh(bayesianmod, table = F)
# mod
