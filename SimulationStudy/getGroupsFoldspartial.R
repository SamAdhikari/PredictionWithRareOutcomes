getGroupsFoldspartial = function(TT,XX,Outcome,
                          SurgDate_Year){

    ##we will use valves for which we have at least 1% users
    vNames = grep('Valve_G', dimnames(XX)[[2]])
    outcomeByvalve =   sapply(vNames, function(x)sum(XX[, x]))
    vid = vNames[which(outcomeByvalve < 0.01 * dim(XX)[1])]
    if(length(vid > 0)){
        XXsub = XX[, -vid]
    }else(XXsub = XX)
    
    grpid = 0
    ##  GROUP INDICATORS
    ##create group assignment for gglasso
    groupIndicators = rep(NA, dim(XXsub)[[2]])
    
    ## group for race
    raceid = grep('Race', dimnames(XXsub)[[2]])
    if(length(raceid) > 0){
        grpid = grpid + 1
        groupIndicators[raceid] = grpid 
    }
    ##InfEndo
    InfEndoid = grep('InfEndo', dimnames(XXsub)[[2]])
    if(length(InfEndoid) > 0){
        grpid = grpid + 1
        groupIndicators[InfEndoid] = grpid
    }
    ##MedACEI
    #medACEIid = grep('MedACEI',dimnames(XXsub)[[2]])
   # groupIndicators[medACEIid] = 3
    
    ##MedADP5Days
    medADP5Daysid = grep('MedADP5Days', dimnames(XXsub)[[2]])
    if(length(medADP5Daysid) > 0){
        grpid = grpid + 1
        groupIndicators[medADP5Daysid] = grpid
    }
    
    ##MedACoag
    medACoagid = grep('MedACoag', dimnames(XXsub)[[2]])
    if(length(medACoagid) > 0){
        grpid = grpid + 1
        groupIndicators[medACoagid] = grpid
    }
    
    ##group for 'MedASA'
    medASAid = grep('MedASA', dimnames(XXsub)[[2]])
    if(length(medASAid) > 0){
        grpid = grpid + 1
        groupIndicators[medASAid] = grpid
    }
    
    ##group for MedBeta
    medBetaid = grep('MedBeta', dimnames(XXsub)[[2]])
    if(length(medBetaid) > 0){
        grpid = grpid + 1
        groupIndicators[medBetaid] = grpid 
    }
    
    ##group for medCoum
    #medCoumid = grep('MedCoum',dimnames(XXsub)[[2]])
    #groupIndicators[medCoumid] = 8
    
    ##MedGP
    #medGPid = grep('MedGP',dimnames(XXsub)[[2]])
    #groupIndicators[medGPid] = 9
    
    ##MedInotr
    medInotrid = grep('MedInotr', dimnames(XXsub)[[2]])
    if(length(medInotrid) > 0){
        grpid = grpid + 1
        groupIndicators[medInotrid] = grpid
    }
    
    ##MedLipid
    medLipidid = grep('MedLipid', dimnames(XXsub)[[2]])
    if(length(medLipidid)>0){
        grpid = grpid + 1
        groupIndicators[medLipidid] = grpid
    }
    ##MedLipMN
    # medLipMNid = grep('MedLipMN',dimnames(XXsub)[[2]])
    # groupIndicators[medLipMNid] = 9
    # 
    # ##MedNitIV
    # medNitIVid = grep('MedNitIV',dimnames(XXsub)[[2]])
    # groupIndicators[medNitIVid] = 10
    
    ##MedSter
    medSterid = grep('MedSter', dimnames(XXsub)[[2]])
    if(length(medSterid) > 0){
        grpid = grpid + 1
        groupIndicators[medSterid] = grpid
    }
    
    ##classNYH
    classNYHid = c(grep('CHFYes', dimnames(XXsub)[[2]]),
                   grep('ClassNYH', dimnames(XXsub)[[2]]))
    if(length(classNYHid)> 0){
        grpid = grpid + 1
        groupIndicators[classNYHid] = grpid
    }
    
    ##HDEFMeth
    hdefMethid = grep('HDEF', dimnames(XXsub)[[2]])
    if(length(hdefMethid) > 0){
        grpid = grpid + 1    
        groupIndicators[hdefMethid] = grpid
    }
    
    ##EFCateg
    efCategid = grep('EFCateg', dimnames(XXsub)[[2]])
    if(length(efCategid) > 0){
        grpid = grpid + 1
        groupIndicators[efCategid ] = grpid
    }
    
    ##NumDisV
    numDisVid = grep('NumDisV',  dimnames(XXsub)[[2]])
    if(length(numDisVid) > 0){
        grpid = grpid + 1
        groupIndicators[numDisVid] = grpid
    }
   
    statusid = which(dimnames(XXsub)[[2]]%in%c('Elective','Urgent'))
   if(length(statusid)>0){
        grpid = grpid + 1
        groupIndicators[statusid] = grpid
   }
    
    hospid = which(dimnames(XXsub)[[2]%in%c('H05','H07','H13','H17','H19','H31','H43','H47','H61',
                        'H70','H79','H89','H97'))
    if(length(hospid) > 0){
        grpid = grpid + 1
        groupIndicators[hospid] = grpid
    }
    
    
    AVMech = 1:6
    AVBiop = 7:14
    # groupIndicators[match(paste('valve_type',AVMech,sep=''),
    #                       dimnames(XXsub)[[2]])] = 19
    # groupIndicators[match(paste('valve_type',AVBiop,sep=''),
    #                       dimnames(XXsub)[[2]])] = 20
    # 
    grpid = grpid + 1
    groupIndicators[match(paste('Valve_G', AVMech, sep =''),
                          dimnames(XXsub)[[2]])] = grpid 
    grpid = grpid + 1
    groupIndicators[match(paste('Valve_G', AVBiop, sep =''),
                          dimnames(XXsub)[[2]])] = grpid
     
    rem_grp = which(is.na(groupIndicators))
    
    groupIndicators[rem_grp] = seq(grpid + 1,(grpid + length(rem_grp)),by = 1)
    
    sort(unique(groupIndicators))
    
    return(list(groupIndicators = groupIndicators, varNames = dimnames(XXsub)[[2]]))
    
}
