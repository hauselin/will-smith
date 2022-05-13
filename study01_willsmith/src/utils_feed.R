library(data.table); library(dplyr)

get_data <- function(src, output = NULL, n_entities = 120) {
  eng <- fread(src)
  eng <- eng[condition != "null"]
  message(paste0(n_distinct(eng$user_id)), " users")
  
  # recode time columns to seconds since first event 
  eng[time_stamp >= 0, timestamp := as.numeric((time_stamp - min(time_stamp, na.rm = T))) / 1000]
  eng[time_engagement >= 0, time_engage := as.numeric(time_engagement - min(time_engagement, na.rm = T)) / 1000]
  eng[time_content_viewed >= 0, time_view := as.numeric(time_content_viewed - min(time_content_viewed, na.rm = T)) / 1000]
  eng[, `:=` (time_stamp = NULL, time_engagement = NULL, time_content_viewed = NULL)]
  
  # mobile/desktop
  eng[, mobile := 0]
  eng[grepl("[Mm]obile", user_agent), mobile := 1]
  eng$user_agent <- NULL
  
  # headline veracity
  eng[grepl("/t_", entity_url), veracity := 1]
  eng[grepl("/f_", entity_url), veracity := 0]
  eng$entity_url <- NULL
  
  # new entity-cat column
  eng[, entity_category2 := entity_category]
  eng[entity_category2 == "news", 
      entity_category2 := ifelse(veracity == 1, 'news_true', 'news_false')]
  
  # remove users without n_entities
  to_exclude <- eng[, .(unique_entities = n_distinct(entity_id)), user_id]
  to_exclude <- to_exclude[unique_entities != n_entities][order(-unique_entities)]
  message(paste0("Remove ", nrow(to_exclude),  " users without ", n_entities, " unique entities"))
  print(to_exclude)
  eng <- eng[!user_id %in% to_exclude$user_id]
  message(paste0(n_distinct(eng$user_id)), " users with ", n_entities, " unique entities")
  
  # convert all int columns to numeric
  cols2convert <- colnames(eng)[sapply(eng, class) %in% c("integer64", "integer")]
  for (c in cols2convert) {
    eng[, (c) := as.numeric(get(c))]
  }
  eng[, condition := as.numeric(condition)]
  
  # sort
  eng <- eng[order(user_id, item_order, time_view, time_engage, timestamp)]
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(eng, output)
  }
  return(data.table(eng))
}


make_dwell_data <- function(dat, output = NULL, max_dwell_ms = 30000, min_dwell_ms = NULL, exclude_start_end = c(3, 3)) {
  message("Note: Multiple dwells for a single entity will be summed.")
  message(paste0("Note: Dwell times for first ", exclude_start_end[1], " and last ", exclude_start_end[2], " entities convert to NA."))
  
  dt_dwell <- dat[engagement_type == "dwell"]
  dt_nondwell <- dat[engagement_type != "dwell"]
  dt_dwell[, response := as.numeric(response)]
  # rename
  setnames(dt_dwell, c("response"), c("dwell"))
  
  n_long_dwells <- dt_dwell[dwell >= max_dwell_ms, .N]
  perc_long_dwells <- round(n_long_dwells / nrow(dt_dwell) * 100, 2)
  message(paste0(n_long_dwells, " (", perc_long_dwells, "%) dwells >", max_dwell_ms, " ms"))
  
  # compute dwell for each user and entity
  dt_dwell2 <- dt_dwell[, .(dwell = sum(dwell, na.rm = T), 
                            n_dwells = .N,
                            mobile = mean(mobile, na.rm = T),
                            btn_likes = mean(likes, na.rm = T),
                            btn_read_later = mean(read_later, na.rm = T),
                            btn_shares = mean(shares, na.rm = T),
                            condition = mean(condition, na.rm = T),
                            item_order = mean(item_order, na.rm = T)), 
                    keyby = .(user_id, ip_address, entity_id, veracity, entity_category, entity_category2, entity_set, experiment_id)]
  dt_dwell2 <- dt_dwell2[order(user_id, item_order)]
  
  # exclude outliers after summing (or before summing?)
  dt_dwell2[dwell > max_dwell_ms, dwell := NA]
  # convert all dwells < min_dwell_ms to min_dwell_ms
  if (!is.null(min_dwell_ms)) {
    dt_dwell2[!is.na(dwell) & dwell < min_dwell_ms, dwell := min_dwell_ms]
  }
  
  # remove dwell times for first/last n entities 
  item_orders <- dt_dwell2[, unique(item_order)]
  trim1 <- head(item_orders, exclude_start_end[1])
  trim2 <- tail(item_orders, exclude_start_end[2])
  dt_dwell2[item_order %in% c(trim1, trim2), dwell := NA]
  
  # add 1ms before taking log to avoid log(0) later
  dt_dwell2[, dwell := dwell + 1]
  dt_dwell2[, ldwell := log(dwell)]
  
  # calculate engaements (e.g., likes, shares, unlikes, unshares)
  nondwell_count <- dt_nondwell[, .(n = .N), keyby = .(user_id, entity_id, engagement_type, response)]
  nondwell_wide <- dcast(nondwell_count, user_id + entity_id ~ engagement_type + response, value.var = c("n"), fill = 0)
  
  eng3 <- data.table(dplyr::left_join(dt_dwell2, nondwell_wide))
  replace_na_columns <- names(select(eng3, -(user_id:ldwell)))
  eng3[, (replace_na_columns) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = replace_na_columns]
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(eng3, output)
  }
  return(data.table(eng3))
}


get_all_dwells <- function(dat, output = NULL, max_dwell_ms = 30000, min_dwell_ms = NULL, exclude_start_end = c(3, 3)) {
  message("Note: Multiple dwells for a single entity will NOT be summed.")
  message(paste0("Note: Dwell times for first ", exclude_start_end[1], " and last ", exclude_start_end[2], " entities convert to NA."))
  dt_dwell <- dat[engagement_type == "dwell"]
  dt_nondwell <- dat[engagement_type != "dwell"]
  dt_dwell[, response := as.numeric(response)]
  # rename
  setnames(dt_dwell, c("response", "likes", "read_later", "shares", "accuracy_score"),
                     c("dwell", "btn_likes", "btn_read_later", "btn_shares", "btn_accuracy_score"))
  
  
  n_long_dwells <- dt_dwell[dwell >= max_dwell_ms, .N]
  perc_long_dwells <- round(n_long_dwells / nrow(dt_dwell) * 100, 2)
  message(paste0(n_long_dwells, " (", perc_long_dwells, "%) dwells >", max_dwell_ms, " ms"))
  
  # exclude outliers after summing (or before summing?)
  dt_dwell[dwell > max_dwell_ms, dwell := NA]
  # convert all dwells < min_dwell_ms to min_dwell_ms
  if (!is.null(min_dwell_ms)) {
    dt_dwell[!is.na(dwell) & dwell < min_dwell_ms, dwell := min_dwell_ms]
  }
  
  # remove first/last n entities
  item_orders <- dt_dwell[, unique(item_order)]
  trim1 <- head(item_orders, exclude_start_end[1])
  trim2 <- tail(item_orders, exclude_start_end[2])
  dt_dwell[item_order %in% c(trim1, trim2), dwell := NA]
  
  # add 1ms before taking log to avoid log(0) later
  dt_dwell[, dwell := dwell + 1]
  dt_dwell[, ldwell := log(dwell)]
  
  # calculate engaements (e.g., likes, shares, unlikes, unshares)
  nondwell_count <- dt_nondwell[, .(n = .N), keyby = .(user_id, entity_id, time_view, engagement_type, response)]
  nondwell_wide <- dcast(nondwell_count, user_id + entity_id + time_view ~ engagement_type + response, value.var = c("n"), fill = 0)
  
  eng3 <- data.table(dplyr::left_join(dt_dwell, nondwell_wide))
  
  # replace NAs with 0 in the engagement columns
  replace_na_columns <- names(select(eng3, -(user_id:ldwell)))
  eng3[, (replace_na_columns) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = replace_na_columns]
  
  # remove columns and unusable events
  eng3 <- eng3[!is.na(time_view)]
  eng3$time_engage <- NULL
  
  # sort
  eng3 <- eng3[order(user_id, time_view, entity_id)]
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(eng3, output)
  }
  return(data.table(eng3))
}

make_user_form <- function(src, output = NULL) {
  d <- fread(src)
  d <- d[!is.na(response) & response != ""]
  d <- d[order(user_id, field, time_stamp)]
  message(paste0(n_distinct(d$user_id), " users"))
  
  checkpoints <- d[field == "checkpoint"]
  responses <- d[field != "checkpoint"]
  responses <- distinct(responses)
  responses[, field := tolower(gsub("-", "_", field))]
  
  # select final/last response for each user_id/field (in case people responded multiple times)
  responses <- responses[, .SD[.N], keyby = .(user_id, field)]
  form_data <- dcast(responses, user_id + experiment_id ~ field, value.var = c("response"))
  
  # sum crt responses
  form_data$crt <- (form_data$crt_10_50_printer == '10') + grepl("emily", tolower(form_data$crt_emily_april_may)) + (form_data$crt_mark_adam_28 == '4') + (form_data$crt_mold_bread_40 == '39') + (form_data$crt_pass_second_place == '2') + (form_data$crt_sheep_15_8_died == '8')
  n_crt <- sum(grepl("crt_", names(form_data)))
  setDT(form_data)
  form_data[, crt := crt / n_crt]
  
  # convert type
  form_data[, `:=` (age = as.numeric(age),
                    ses_ladder = as.numeric(ses_ladder), 
                    ses_education_years = as.numeric(ses_education_years), 
                    social_media_followers = as.numeric(social_media_followers),
                    social_media_following = as.numeric(social_media_following))]
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(form_data, output)
  }
  return(data.table(form_data))
}



make_itemlevel <- function(src, output = NULL, dwell_data = NULL) {
  covs <- fread(src)
  setnames(covs, tolower(names(covs)))  # lower case col names
  setnames(covs, gsub(".", "_", names(covs), fixed = TRUE))  # convert . to _
  
  covs$response <- NULL
  covs$experiment_id <- NULL
  covs <- covs[, lapply(.SD, function(x) ifelse(x == 'null', NA, x))]  # convert nulls to NA
  
  # convert to numeric
  char_cols <- c("entity_id", "entity_url", "entity_category", "entity_set")
  num_cols <- names(covs)[!names(covs) %in% char_cols]
  covs[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  # veracity
  covs[grepl("/t_", entity_url), veracity := 1]
  covs[grepl("/f_", entity_url), veracity := 0]
  covs$time_stamp <- NULL
  
  # new entity-cat column
  covs[, entity_category2 := entity_category]
  covs[entity_category2 == "news", 
      entity_category2 := ifelse(veracity == 1, 'news_true', 'news_false')]
  
  if (!is.null(dwell_data)) {
    dwell_avg <- dwell_data[, .(times_shown = .N,  # assumes single dwell per entity per user
                                dwells_total = sum(n_dwells, na.rm = T),  # account for multiple dwells per entity
                                likes_0_sum = sum(likes_0, na.rm = T),
                                likes_1_sum = sum(likes_1, na.rm = T),
                                shares_0_sum = sum(shares_0, na.rm = T),
                                shares_1_sum = sum(shares_1, na.rm = T),
                                mean_order = mean(item_order, na.rm = T)), 
                            keyby = .(entity_id)]
    # dwell_avg[, ldwell := log(dwell)]
    covs <- data.table(left_join(covs, dwell_avg))
  }
  
  # save 
  if (!is.null(output)) {
    dirpath <- dirname(output)
    if (!file.exists(dirpath)) {
      dir.create(dirpath)
    }
    fwrite(covs, output)
  }
  return(data.table(covs))
}


entity_political_concordance <- function(dwell_data, user_form, item_level, scale_range = c(1, 6), output = NULL) {
    # if participant is democrat, reverse-code the partisanship variable; leave as it is for republicans
    
    dwell_entity <- dwell_data[, .(user_id, entity_id, veracity, entity_category, entity_category2)]
    
    user_politicalpref <- user_form[!is.na(politicalpref), .(user_id, politicalpref)]
    user_politicalpref[, sort(unique(politicalpref))]
    user_politicalpref[, politicalpref := as.numeric(substring(politicalpref, 1, 1))]
    
    entity_partisan <- item_level[!is.na(partisanship_combined), .(entity_id, partisanship_combined)]
    
    partisan_recode <- left_join(dwell_entity, user_politicalpref) %>% left_join(entity_partisan) %>% data.table()
    # reverse the score for democrats (because higher = favor republicans)
    partisan_recode[, partisanship_concordance := partisanship_combined]
    partisan_recode[politicalpref <= 3, partisanship_concordance := scale_range[2] - partisanship_combined + scale_range[1]]
    partisan_recode[is.na(politicalpref), partisanship_concordance := NA]
    
    # save 
    if (!is.null(output)) {
        dirpath <- dirname(output)
        if (!file.exists(dirpath)) {
            dir.create(dirpath)
        }
        fwrite(partisan_recode, output)
    }
    return(data.table(partisan_recode))
}



id2url <- function(eid, item_level = NULL) {
  if ("item_level" %in% ls(envir = .GlobalEnv)) {
    item_level <- get("item_level", envir = .GlobalEnv)
  } else {
    stop("item_level variabl/object not found in global environment. Provide item_level argument")
  }
  return(data.table(item_level)[entity_id == eid, entity_url])
}
