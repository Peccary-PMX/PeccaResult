# NMBehaviour:
#   - "png": juste look for a png and display if it existe
#   - "ifnopng": if the png file does not existe, create it
#   - "create": create even if it exist

# names <- c("SIMUL", "IPRED", "ID", "TIME", "OBS", "EVID", "CMT",
#            "LLOQ", "BLQ", "X_DOSE", "X_DRUG", "X_CELL", "YTYPE", "Baseline", "IDPK")
#
# dataset <- read.table(col.names = names, file = "file:///Z:/ETUDES/SPK/CLSPKPOOL/ANACIN/USERS/TDPE_CB_2/18_11_14_BiophaseMasterProject/ENCOURS_Post_Mentre_Meeting_January/PK_65/NM_VIEWER/1.00.SAEM_PK_Classic_vpc.TAB", header = T) %>%
#   as_tibble() %>%
#   filter(YTYPE == 1) %>%
#   mutate(tag = paste0(X_DOSE,X_DRUG,X_CELL))
#
#
# peccary_pred_dist(dataset = dataset, x = "TIME", y = "IPRED", group = "", obs = "OBS")
#
#
# read.table("Z:/ETUDES/S20098/CS_20098/ANACIN/PK/CS_PED_SEMI_PHYSIO/05_COVARIABLES/NM_VIEWER/04_75054_sansadda_FU1_vpc500_10p_theo/04_75054_sansadda_FU1_vpc500_10p_theo.TAB", header = T) %>%
#   as_tibble() -> dataset
# names(dataset) <- c("SIM", "ID",  "TIME",  "TAD", "AMT", "EVID",  "IPRED", "PRED",  "THEO",  "CLINT", "VC",  "KA",  "PER", "FLAG",  "BLQ", "STDY",  "DOSE",  "IIV_VC",  "IIV_CLINT", "IOV_KA",  "OBS", "DV",  "OBS10", "IPRED10", "covPUB",  "covCLASS")
#
#
# peccary_pred_dist(dataset = dataset, x = "THEO", y = "IPRED10", group = "", obs = "OBS")
#
# colnames <- c("SIMUL","ID","TIME","THEOTAD","DV","YOBS","PER","DAY","DOSET","DOSE","YTYPE","EVID","AMT","NPDE","IWRES","IPRED","CWRES","DV2","PRED","RES","WRES","AUC24D1")
# dataset <- read.table(col.names = colnames,file = "Z:/ETUDES/S95008/CS_95008/ANACIN/PKPD/NONMEM2/J0J7these/NM_VIEWER/RUNSLOPEAUC_VPC.TAB") %>%
#   as_tibble()
#
# names(dataset)[1] <- "SIM"
#
#
# peccary_pred_dist(dataset, x = "TIME", sim = "SIM" , y = "IPRED", obs = "OBS", group = "X_DOSE")

#' Plot Pred Dist
#'
#' @param dataset Name of the dataset containing simulations
#' @param sim Name of the simulation column (SIM, SIMUL...)
#' @param x Name of the x column (Time in general)
#' @param y  Name of the y column (DV, IPRED....)
#' @param obs Name of the observation column (to be displayed)
#' @param group Name of one or several group to split (e.g. "AGE + SEX")
#' @param ylog Booléen, for logarithmique scale
#' @param loq_obs Value of loq for observation
#' @param loq_simul Value of loq for simulations (on test, use with precaution)
#' @author Thibaud Derippe
#'
#' @return
#' @export
#'
#' @examples
plot_pred_dist <- function(dataset,sim,  x, y, obs = "OBS10",  group = "", ylog = T, loq_obs = 0.01, ymin_displayed = 0.0001, xlab = "Time", ylab = "Concentration", filterr = "", ...){

  if(filterr != ""){

    dataset <- dataset %>%
      filter_(filterr)

  }

  dataset %>%
    rename_(x = x) %>%
    rename_(y = y) %>%
    rename_(sim = sim) %>%
    group_by(x)-> d

  if(obs != "") d <- d %>% rename_(obs = obs)

  if(group != ""){

    if(length(group) == 1){
      group_analyse <-
        strsplit(group, "\\+")[[1]] %>%
        gsub(pattern = " ", replacement = "")

    }else{

      group_analyse <- group
    }


    if(length(group_analyse)>1){



      d %>%
        ungroup() %>%
        mutate(group =  apply(   d[ , group_analyse ] %>% imap_dfr( function(x,y) { paste0(y, ": ",x)}) , 1 , paste , collapse = " - " )) -> d

    }else{

      d <- d %>%
        ungroup() %>%
        rename_(group = group) %>%
        mutate(group = paste0(group_analyse,": ", group))

    }

    d <- d %>%
      group_by(x, group)
  }



  d %>%
    nest() %>%
    crossing(tibble(q1 = seq(0.1,0.8,0.1),
                    q2 = q1 + 0.1)) %>%
                    # {.[[1,3]]} -> data
    mutate(qx = map2_dbl(q1, data, function(q1,data){

      unname(quantile(data$y, q1))

    })) %>%
    mutate(qx2 = map2_dbl(q2, data, function(q2,data){

      unname(quantile(data$y, q2))

    })) -> qunatl





    ggplot()+
    geom_ribbon(data = qunatl, aes(x = x, ymin = qx, ymax = qx2,  alpha = paste0(q1,"-", q2)), fill = "purple")+
    geom_line(data = qunatl %>%  filter(q1 == 0.5), aes(x,  qx))+
    scale_alpha_manual(values = c(0.2, 0.4, 0.6, 1, 1, 0.6, 0.4, 0.2))+
      labs(alpha = "quantiles", x = xlab, y = ylab)+
      theme_bw() -> output

    if(ymin_displayed > 0){

      output   <- output +
        coord_cartesian(ylim = c(ymin_displayed, max(c(qunatl$qx2, dataset$obs))))


    }

    if(ylog == T)
      output <- output +
       scale_y_log10(labels = labels_log, breaks = breaks_log)

    if(obs != ""){

      data_dot <- d %>%
        filter(sim == 1 ) %>%
        mutate(BLQ = if_else(obs <= loq_obs, "Yes", "No")) %>%
        mutate(obs = if_else(obs <= loq_obs, as.double(loq_obs), as.double(obs)))

      output <- output +
        geom_point(data =  data_dot  , aes(x,obs, shape = BLQ))+
        scale_shape_manual(values = c(19,8)) #do we need the filter(SIMUL? risk of bug vs risk of heaviness)

    }

    if(loq_obs > 0)
      output <- output+
      geom_hline(yintercept = loq_obs, lty = 3)

    if(group != "")   output <- output + facet_wrap(~group, scales = "free")

    return(output)
}


# p<- c("SIMUL", "IPRED", "ID", "TIME", "OBS", "EVID", "CMT", "LLOQ", "BLQ",  "X_DOSE", "X_DRUG", "X_CELL", "YTYPE", "Baseline", "IDPK")
# dataset <- read.table( col.names = p ,file = "file:///Z:/ETUDES/SPK/CLSPKPOOL/ANACIN/USERS/TDPE_CB_2/18_11_14_BiophaseMasterProject/ENCOURS_Post_Mentre_Meeting_January/PK_55/NM_VIEWER/1.01.V1_DOSE_vpc.TAB")
#
#
# plot_pred_dist(dataset = dataset %>% filter(YTYPE == 1), group = c("X_DOSE", "X_CELL"), x = "TIME", y = "IPRED", sim = "SIMUL", obs = "OBS", loq_obs = 0.1, ymin_displayed = 0.01)
# p
# peccary_pred_dist(dataset, x = "TIME", sim = "SIMUL" , y = "IPRED", obs = "OBS", group = "X_DOSE + X_DRUG")
