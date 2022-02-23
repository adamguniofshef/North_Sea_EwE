#### Basic constructor ####
#' Basic constructor with only the number of species as dispatching argument
#' 
#' Only really used to make MizerParams of the right size and shouldn't be used
#' by user
#' @rdname MizerParams
setMethod('MizerParams', signature(object='numeric', interaction='missing'),
          function(object, min_w = 0.001, max_w = 1000, no_w = 100,  min_w_pp = 1e-10, no_w_pp = NA, species_names=1:object, gear_names=species_names){
            #args <- list(...)
            if (!is.na(no_w_pp))
              warning("New mizer code does not support the parameter no_w_pp")
            # Some checks
            if (length(species_names) != object)
              stop("species_names must be the same length as the value of object argument")
            no_sp <- length(species_names)
            
            # Set up grids:
            # Community grid
            w <- 10^(seq(from=log10(min_w),to=log10(max_w),length.out=no_w))
            dw <- diff(w)
            # Correctly defined dw by using the proper ratio (successive dw's have a fixed ratio). 
            dw[no_w] <- dw[no_w-1]*(dw[no_w-1]/dw[no_w-2])	
            
            # Set up full grid - background + community
            # ERROR if dw > w, nw must be at least... depends on minw, maxw and nw
            if(w[1] <= dw[1])
              stop("Your size bins are too close together. You should consider increasing the number of bins, or changing the size range")
            
            # For fft methods we need a constant log step size throughout. 
            # Therefore we use as many steps as are necessary to almost reach min_w_pp. 
            x_pp <- rev(seq(from=log10(min_w), log10(min_w_pp), by=log10(min_w/max_w)/(no_w-1))[-1])
            w_full <- c(10^x_pp, w)
            no_w_full <- length(w_full)
            dw_full <- diff(w_full)
            dw_full[no_w_full] <- dw_full[no_w_full-1]*(dw_full[no_w_full-1]/dw_full[no_w_full-2])	
            
            # Basic arrays for templates
            mat1 <- array(NA, dim=c(no_sp, no_w), 
                          dimnames = list(sp=species_names, w=signif(w,3)))
            mat2 <- array(NA, dim=c(no_sp, no_w, no_w_full), 
                          dimnames = list(sp=species_names, w_pred=signif(w,3), 
                                          w_prey=signif(w_full,3)))
            
            ft_pred_kernel_e <- array(NA, dim=c(no_sp, no_w_full), 
                                      dimnames = list(sp=species_names, k=1:no_w_full))
            
            # We do not know the second dimension of ft_pred_kernel_p until the species
            # parameters determining the predation kernel are know. 
            # So for now we set it to 2
            ft_pred_kernel_p <- array(NA, dim=c(no_sp, 2), 
                                      dimnames = list(sp=species_names, k=1:2))
            
            selectivity <- array(0, dim=c(length(gear_names), no_sp, no_w), 
                                 dimnames=list(gear=gear_names, sp=species_names, 
                                               w=signif(w,3)))
            catchability <- array(0, dim=c(length(gear_names), no_sp), 
                                  dimnames = list(gear=gear_names, sp=species_names))
            interaction <- array(1, dim=c(no_sp, no_sp), 
                                 dimnames = list(predator = species_names, 
                                                 prey = species_names))
            vec1 <- as.numeric(rep(NA, no_w_full))
            names(vec1) <- signif(w_full,3)
            
            # Make an empty data.frame for species_params
            # This is just to pass validity check. 
            # The project method uses the columns species z0 alpha erepro
            # so these must be in there
            # There is also a seperate function to check the dataframe that is
            # passed in by users (not used in validity check)
            species_params <- data.frame(species = species_names,
                                         z0 = NA, alpha = NA, erepro = NA)
            
            # Make an empty srr function, just to pass validity check
            srr <- function(rdi, species_params) return(0)
            
            # Make colour and linetype scales for use in plots
            # Colour-blind-friendly palettes
            # From http://dr-k-lo.blogspot.co.uk/2013/07/a-color-blind-friendly-palette-for-r.html
            # cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
            #                 "#CC79A7", "#F0E442")
            # From http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
            cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", 
                            "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
            linecolour <- rep(cbbPalette, length.out = no_sp)
            names(linecolour) <- as.character(species_names)
            linecolour <- c(linecolour, "Total" = "black", "Plankton" = "green",
                            "Background" = "grey")
            linetype <-rep(c("solid", "dashed", "dotted", "dotdash", "longdash", 
                             "twodash"), length.out = no_sp)
            names(linetype) <- as.character(species_names)
            linetype <- c(linetype, "Total" = "solid", "Plankton" = "solid",
                          "Background" = "solid")
            
            # Make the new object
            # Should Z0, rrPP and ccPP have names (species names etc)?
            res <- new("MizerParams",
                       w = w, dw = dw, w_full = w_full, dw_full = dw_full,
                       psi = mat1, initial_n = mat1, intake_max = mat1, search_vol = mat1, 
                       activity = mat1, 
                       std_metab = mat1, mu_b = mat1, ft_pred_kernel_e = ft_pred_kernel_e, 
                       ft_pred_kernel_p = ft_pred_kernel_p,
                       selectivity=selectivity, catchability=catchability,
                       rr_pp = vec1, cc_pp = vec1, sc = w, initial_n_pp = vec1, 
                       species_params = species_params,
                       interaction = interaction, srr = srr, 
                       A=as.numeric(rep(NA, dim(interaction)[1])),
                       linecolour = linecolour, linetype = linetype) 
            return(res)
          }
)

####################################################################################

#### Main constructor ####
#' Constructor that takes the species_params data.frame and the interaction matrix
#' @rdname MizerParams
setMethod('MizerParams', signature(object='data.frame', interaction='matrix'),
          function(object, interaction,  n = 2/3, p = 0.7, q = 0.8, r_pp = 10, 
                   kappa = 1e11, lambda = (2+q-n), w_pp_cutoff = 10, 
                   max_w = max(object$w_inf)*1.1, f0 = 0.6, 
                   z0pre = 0.6, z0exp = n-1,gear_names=NULL, ...){
            row.names(object) <- object$species
            # Set default values for column values if missing
            # If no gear_name column in object, then named after species
            if(!("gear" %in% colnames(object)) & is.null(gear_names))
            {
              object$gear <- object$species
              gear_names <- unique(object$gear)
            }
            no_gear <- length(gear_names)
            # If no k column (activity coefficient) in object, then set to 0
            if(!("k" %in% colnames(object)))
              object$k <- 0
            # If no alpha column in object, then set to 0.6
            # Should this be a column? Or just an argument?
            if(!("alpha" %in% colnames(object)))
              object$alpha <- 0.6
            # If no erepro column in object, then set to 1
            if(!("erepro" %in% colnames(object)))
              object$erepro <- 1
            # If no sel_func column in species_params, set to 'knife_edge'
            if(!("sel_func" %in% colnames(object))){
              message("\tNote: No sel_func column in species data frame. Setting selectivity to be 'knife_edge' for all species.")
              object$sel_func <- 'knife_edge'
              # Set default selectivity size
              if(!("knife_edge_size" %in% colnames(object))){
                message("Note: \tNo knife_edge_size column in species data frame. Setting knife edge selectivity equal to w_mat.")
                object$knife_edge_size <- object$w_mat
              }
            }
            # If no catchability column in species_params, set to 1
            if(!("catchability" %in% colnames(object)))
              object$catchability <- 1
            # Sort out h column If not passed in directly, is calculated from f0 and
            # k_vb if they are also passed in
            if(!("h" %in% colnames(object))){
              message("Note: \tNo h column in species data frame so using f0 and k_vb to calculate it.")
              if(!("k_vb" %in% colnames(object))){
                stop("\t\tExcept I can't because there is no k_vb column in the species data frame")
              }
              object$h <- ((3 * object$k_vb) / (object$alpha * f0)) * (object$w_inf ^ (1/3))
            }
            # Sorting out gamma column
            if(!("gamma" %in% colnames(object))){
              message("Note: \tNo gamma column in species data frame so using f0, h, beta, sigma, lambda and kappa to calculate it.")
              ae <- sqrt(2*pi) * object$sigma * object$beta^(lambda-2) * exp((lambda-2)^2 * object$sigma^2 / 2)
              object$gamma <- (object$h / (kappa * ae)) * (f0 / (1 - f0))
            }
            # Sort out z0 column
            if(!("z0" %in% colnames(object))){
              message("Note: \tNo z0 column in species data frame so using z0 = z0pre * w_inf ^ z0exp.")
              object$z0 = z0pre*object$w_inf^z0exp    # background natural mortality
            }
            # Sort out ks column
            if(!("ks" %in% colnames(object))){
              message("Note: \tNo ks column in species data frame so using ks = h * 0.2.")
              object$ks <- object$h * 0.2
            }
            
            # Check essential columns: species (name), wInf, wMat, h, gamma,  ks, beta, sigma 
            check_species_params_dataframe(object)
            
            no_sp <- nrow(object)
            # Make an empty object of the right dimensions
            res <- MizerParams(no_sp, species_names=object$species, 
                               gear_names=gear_names, max_w=max_w,...)
            res@n <- n
            res@p <- p
            res@lambda <- lambda
            res@q <- q
            res@f0 <- f0
            res@kappa <- kappa
            
            # If not w_min column in species_params, set to w_min of community
            if (!("w_min" %in% colnames(object)))
              object$w_min <- min(res@w)
            # Check min_w argument is not > w_min in species_params
            if(any(object$w_min < min(res@w)))
              stop("One or more of your w_min values is less than the smallest size of the community spectrum")
            
            # Add w_min_idx column which has the reference index of the size class closest
            # to w_min - this is a short cut for later on and prevents repetition.
            if (!("w_min_idx" %in% names(object))) {
              object$w_min_idx <- as.vector(
                tapply(object$w_min,1:length(object$w_min),
                       function(w_min,wx) max(which(wx<=w_min)),wx=res@w))
            }
            
            # Start filling the slots
            res@species_params <- object
            # Check dims of interaction argument - make sure it's right
            if (!isTRUE(all.equal(dim(res@interaction), dim(interaction))))
              stop("interaction matrix is not of the right dimensions. Must be number of species x number of species")
            # Check that all values of interaction matrix are 0 - 1. Issue warning if not
            if(!all((interaction>=0) & (interaction<=1)))
              warning("Values in the interaction matrix should be between 0 and 1")
            # In case user has supplied names to interaction matrix which are wrong order
            for (dim_check in 1:length(dimnames(res@interaction))){
              if (!is.null(dimnames(interaction)[[dim_check]]) & (!(isTRUE(all.equal(dimnames(res@interaction)[[dim_check]],dimnames(interaction)[[dim_check]])))))
                
                warning("Dimnames of interaction matrix do not match the order of species names in the species data.frame. I am now ignoring your dimnames so your interaction matrix may be in the wrong order.")}
            res@interaction[] <- interaction
            
            # Now fill up the slots using default formulations:
            # psi - allocation to reproduction - from original Setup() function
            res@psi[] <- unlist(tapply(res@w,1:length(res@w),function(wx,w_inf,w_mat,n){
              ((1 + (wx/(w_mat))^-10)^-1) * (wx/w_inf)^(1-n)},w_inf=object$w_inf,w_mat=object$w_mat,n=n))
            # Set w < 10% of w_mat to 0
            res@psi[unlist(tapply(res@w,1:length(res@w),function(wx,w_mat)wx<(w_mat*0.1)  ,w_mat=object$w_mat))] <- 0
            # Set all w > w_inf to 1 # Check this is right...
            res@psi[unlist(tapply(res@w,1:length(res@w),function(wx,w_inf)(wx/w_inf)>1,w_inf=object$w_inf))] <- 1
            # note sure what a and n0_mult are in get_initial_n
            
            res@intake_max[] <- unlist(tapply(res@w,1:length(res@w),function(wx,h,n)h * wx^n,h=object$h,n=n))
            res@search_vol[] <- unlist(tapply(res@w,1:length(res@w),function(wx,gamma,q)gamma * wx^q, gamma=object$gamma, q=q))
            res@activity[] <-  unlist(tapply(res@w,1:length(res@w),function(wx,k)k * wx,k=object$k))
            res@std_metab[] <-  unlist(tapply(res@w,1:length(res@w),function(wx,ks,p)ks * wx^p, ks=object$ks,p=p))
            res@mu_b[] <- res@species_params$z0
            
            Beta <- log(res@species_params$beta)
            sigma <- res@species_params$sigma
            Dx <- res@w[2]/res@w[1] - 1  # dw = w Dx
            # w_full has the weights from the smallest relevant plankton, to the largest fish
            xFull <- log(res@w_full)
            xFull <- xFull - xFull[1]
            
            # ft_pred_kernel_e is an array (species x log of predator/prey size ratio) 
            # that holds the Fourier transform of the feeding kernel in a form 
            # appropriate for evaluating the available energy integral
            res@ft_pred_kernel_e <- matrix(0, nrow = dim(res@interaction)[1], ncol=length(xFull))
            noSpecies <- dim(res@interaction)[1]
            for(i in 1:noSpecies){
              # We compute the feeding kernel terms and their fft.
              res@ft_pred_kernel_e[i, ] <- Dx*fft(exp(-(xFull - Beta[i])^2/(2*sigma[i]^2)))
            }
            
            # rr is the log of the maximal predator/prey mass ratio
            # Here we use default rr= beta + 3*sigma
            rr <- Beta + 3*sigma
            # Perturb rr so it falls on grid points
            dx <- xFull[2]-xFull[1]
            rr <- dx*ceiling(rr/dx)
            # Determine period used
            P <- max(xFull[length(xFull)] + rr)
            # Determine number of x points used in period
            no_P <- 1+ceiling(P/dx)  # P/dx should already be integer
            # vector of values for log predator/prey mass ratio
            x_P <- (-1:(no_P-2))*dx
            
            # The dimension of ft_pred_kernel_p was not know at the time the res object
            # was initialised. Hence we need to create it with the right dimension here.
            res@ft_pred_kernel_p <- matrix(0, nrow = noSpecies, ncol = no_P)
            dimnames(res@ft_pred_kernel_p) <- list(sp=rownames(res@std_metab),k=(1:no_P))
            
            for (j in 1:noSpecies){
              phi <- rep(0, no_P)
              # Our phi is a periodic extension of the normal feeding kernel.
              # For 0<=x<=P we use phi[x-P] as our
              # value of the period P extension of phi, since support(phi)=[-rr,0]
              phi[x_P-P >= -rr[j]] <- exp(-(Beta[j]-P+x_P[x_P-P >= -rr[j]])^2/(2*sigma[j]^2))
              # We also save the fft of this vector, so we don't have to use too many fft s in the time evolution
              res@ft_pred_kernel_p[j, ] <- Dx*fft(phi)
            }
            
            # Background spectrum
            res@rr_pp[] <- r_pp * res@w_full^(n-1) #weight specific plankton growth rate ##
            res@cc_pp[] <- kappa*res@w_full^(-lambda) # the resource carrying capacity - one for each mp and m (130 of them)
            res@cc_pp[res@w_full>w_pp_cutoff] <- 0      #set density of sizes < plankton cutoff size
            # Set the SRR to be a Beverton Holt esque relationship
            # Can add more functional forms or user specifies own
            res@initial_n_pp <- res@cc_pp
            res@srr <- function(rdi, species_params){
              return(rdi / (1 + rdi/species_params$r_max))
            }
            
            # Set fishing parameters: selectivity and catchability
            # At the moment, each species is only caught by 1 gear so in species_params
            # there are the columns: gear_name and sel_func.
            # BEWARE! This routine assumes that each species has only one gear operating on it
            # So we can just go row by row through the species parameters
            # However, I really hope we can do something better soon
            # for (g in 1:nrow(object)){
            #   # Do selectivity first
            #   # get args
            #   # These as.characters are annoying - but factors everywhere
            #   arg <- names(formals(as.character(object[g,'sel_func'])))
            #   # lop off w as that is always the first argument of the selectivity functions
            #   arg <- arg[!(arg %in% "w")]
            #   if(!all(arg %in% colnames(object)))
            #     stop("All of the arguments needed for the selectivity function are not in the parameter dataframe")
            #   # Check that there is only one column in object with the same name
            #   # Check that column of arguments exists
            #   par <- c(w=list(res@w),as.list(object[g,arg]))
            #   sel <- do.call(as.character(object[g,'sel_func']), args=par)
            #   # Dump Sel in the right place
            #   res@selectivity[as.character(object[g,'gear']), g, ] <- sel
            #   # Now do catchability
            #   res@catchability[as.character(object[g,'gear']), g] <- object[g,"catchability"]
            # }
            
            # Store colours and linetypes in slots if contained in species parameters
            if ("linetype" %in% names(object)) {
              linetype <- object$linetype[!is.na(object$linetype)]
              res@linetype[object$species[!is.na(object$linetype)]] <- linetype
            }
            if ("linecolour" %in% names(object)) {
              linecolour <- object$linecolour[!is.na(object$linecolour)]
              res@linecolour[object$species[!is.na(object$linecolour)]] <- linecolour
            }
            
            # Remove catchabiliy from species data.frame, now stored in slot
            #params@species_params[,names(params@species_params) != "catchability"]
            res@species_params <- res@species_params[,-which(names(res@species_params)=="catchability")]
            res@initial_n <- res@psi
            res@initial_n <- get_initial_n(res)
            res@A <- rep(1,no_sp)
            return(res)
          }
)

check_species_params_dataframe <- function(species_params){
  # Check species_params dataframe (with a function) for essential cols
  # Essential columns: species (name) # wInf # wMat # h # gamma - search Volume #  ks # beta # z0
  essential_cols <- c("species","w_inf","w_mat","h","gamma","ks","beta","sigma", "z0")
  missing_cols <- !(essential_cols %in% colnames(species_params))
  if(any(missing_cols))
  {
    errors <- character()
    for (i in essential_cols[missing_cols])
      errors <- paste(errors, i, sep=" ")
    stop("You are missing these columns from the input dataframe:\n", errors)
  }
  return(TRUE)
}