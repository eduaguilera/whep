
remove_id <- function(txml,id){

  aux <- gregexpr(id, txml)
  posid <- aux[[1]][1]
  auxi <- as.numeric(gregexpr("<mxCell",txml)[[1]])

  auxf <- as.numeric(gregexpr("</mxCell>",txml)[[1]])

  t1 <- max(which(auxi<posid))
  t2 <- min(which(auxf>posid))


  auxt <- (substr(txml,auxi[t1],auxf[t2]+str_length("</mxCell>")-1))
  txml <- str_replace(txml,auxt,"")

  # if(id=="XSYGR"){
  # print(auxi[t1])
  # print(auxf[t2])
  # print(posid)
  # print(auxt)
  # akoasop
  # }



  return(txml)
}


remove_change_bubbles <- function(txml,id){

  if(!grepl("ellipse",txml)){ return(txml) }

  aux <- gregexpr("ellipse", txml)
  auxi <- as.numeric(gregexpr("<mxCell",txml)[[1]])
  auxf <- as.numeric(gregexpr("</mxCell>",txml)[[1]])

  posid <- aux[[1]][1]
  # print(posid)
  # print(auxi)
  # print(auxf)
  t1 <- max(which(auxi<posid))
  t2 <- min(which(auxf>posid))
  auxt <- (substr(txml,auxi[t1],auxf[t2]+str_length("</mxCell>")-1))
  txml <- str_replace(txml,auxt,"")

  return(remove_change_bubbles(txml))
}

search_drawio <- function(drive = "C:/") {
  cat("Looking for draw.io.exe en", drive, "...\n")

  archivos <- tryCatch({
    list.files(path = drive, pattern = "draw\\.io\\.exe$", recursive = TRUE, full.names = TRUE)
  }, warning = function(w) {
    message("Advertencia: ", w)
    return(NULL)
  }, error = function(e) {
    message("Error: ", e)
    return(NULL)
  })

  if (length(archivos) == 0) {
    return(NULL)
  } else {
    archivos <- archivos[!grepl("uninstall",str_to_lower(archivos))]
    if(length(archivos)==0){ return(null)}
    return(archivos)
  }
}

change_bubble_size <- function(txml,label,NEW_SIZE,NEW_TEXT_SIZE){


  label <- str_replace_all(label,"<","")
  label <- str_replace_all(label,">","")


  aux <- gregexpr(label, txml)
  posid <- aux[[1]][1]
  auxi <- as.numeric(gregexpr("<mxCell",txml)[[1]])

  auxf <- as.numeric(gregexpr("</mxCell>",txml)[[1]])

  t1 <- max(which(auxi<posid))
  t2 <- min(which(auxf>posid))


  auxt <- (substr(txml,auxi[t1],auxf[t2]+str_length("</mxCell>")-1))

  newauxt <- auxt
  for(i in c(12,11,10,9,8)){
    newauxt <- str_replace(newauxt,paste0("fontSize=",i),paste0("fontSize=",NEW_TEXT_SIZE))
  }
  for(i in c(50.5,50,30)){
    newauxt <- str_replace(newauxt,paste0('width="',i,'"'),paste0('width="',NEW_SIZE,'"'))
    newauxt <- str_replace(newauxt,paste0('height="',i,'"'),paste0('height="',NEW_SIZE,'"'))
  }

  txml <- str_replace(txml,
                      auxt,newauxt )


  return(txml)

}



change_style <- function(txml,id,identifier,value,XML_temp,T_ID_ARROW){

  # so it can accept also id but also labels
  # id <-  str_replace_all(str_replace_all(id,
  #                                        ">","&gt;"),"<","&lt;")
  id <-  str_replace_all(str_replace_all(id,
                                         ">",""),"<","")
  fd <- file(XML_temp, "wb")

  writeChar(txml, fd, eos = NULL)
  close(fd)

  ### Get the ID cell if exists
  data <- xmlParse(XML_temp)
  xd <- xmlToList(data)
  numcell <- NA
  for(i in 1:length(xd$diagram$mxGraphModel$root)){
    act <- xd$diagram$mxGraphModel$root[[i]]
    # if(grepl(paste0('\"',id,'\"'),act[2])){
    if(grepl(paste0('',id,''),act[2])){
      numcell <- i
    }
  }

  if(!is.na(numcell)){
    xml <- xmlTreeParse(XML_temp)
    root <- xmlRoot(xml)

    stact <- xmlAttrs(root[["diagram"]][["mxGraphModel"]][["root"]][[numcell]])["style"]
    aux <- str_split(stact,";")

    stnew <- ""
    idfound <- F
    for(j in 1:length(aux[[1]])){
      act <- aux[[1]][j]
      if(grepl(identifier,act)){
        idfound <- T
        if(identifier=="strokeWidth"){
          # print(value)
          # print(id)
          act <- paste0(identifier,"=",round(max(1,value)))
        }else{
          act <- paste0(identifier,"=",value)
        }
      }
      stnew <- paste0(stnew,act,";")
    }
    if(!idfound){
      stnew <- paste0(stnew,paste0(identifier,"=",value))
    }

    xmlAttrs(root[["diagram"]][["mxGraphModel"]][["root"]][[numcell]])["style"] <- stnew
    cat(saveXML(root), file=XML_temp)

    txml <- readChar(XML_temp, file.info(XML_temp)$size)


    if(value==0 & identifier=="strokeWidth"){
      txml <- remove_id(txml,id)
      aux <- T_ID_ARROW$associated_label_id[T_ID_ARROW$id==id]
      if(!is.na(aux)){
        txml <- remove_id(txml,aux)
      }
    }

  }
  return(txml)
}


change <- function(txml,etiqueta,dato,align="L",T_ID_ARROW,VAL_MAX_WIDTH,DECIMALES_XML,XML_temp,MAX_WIDTH_ARROWS=MAX_WIDTH_ARROWS,PLOT_CHANGE,DATOch,value_change=NA,ARROW_COLOR="#000000",
                   INCREASE_COLOR="#97cde5",
                   DECREASE_COLOR="#a9d77f"){

  if(!grepl(str_replace_all(str_replace_all(etiqueta,">","&amp;gt;"),"<","&amp;lt;"),txml)){
    return(txml)
  }
  # print(DATOch)
  # print(length(DATOch))


  DATOch_flat <- unlist(DATOch)
  # print(DATOch_flat)
  # if(!is.na(DATOch)){
  if (!any(is.na(DATOch_flat))) {
    for(ii in 1:length(DATOch)){
      # print(DATOch[[ii]]$old)
      if(str_to_lower(dato)==str_to_lower(DATOch[[ii]]$old)){ dato <- DATOch[[ii]]$new }
    }
  }

  tdato_xml <- dato
  if(!is.na(as.numeric(dato))){
    tdato_xml <- as.character(round(dato,DECIMALES_XML))
    if(dato<10){
      tdato_xml <- as.character(round(dato,1))
      if(round(dato,1)==0){
        tdato_xml <- as.character(round(dato,2))
      }
    }else{
      tdato_xml <- as.character(round(dato,1))
    }

  }
  if(dato==0 & !grepl("ha",etiqueta)){
    tdato_xml <- ""
  }

  antes <- txml
  txml <- str_replace_all(txml,str_replace_all(str_replace_all(etiqueta,">","&amp;gt;"),"<","&amp;lt;"),
                          str_replace_all(tdato_xml," ",""))
  if(is.na(txml) & !is.na(antes)){
    aquioooooo
  }

  x <- which(T_ID_ARROW$label==etiqueta)
  if(length(x)>0){

    new_thick <-abs(dato)*MAX_WIDTH_ARROWS/VAL_MAX_WIDTH
    txml <- change_style(txml,T_ID_ARROW$id[x],"strokeWidth",new_thick,XML_temp,T_ID_ARROW)

    if(ARROW_COLOR!="#000000" & !is.na(ARROW_COLOR)){
      txml <- change_style(txml,T_ID_ARROW$id[x],"fillColor",ARROW_COLOR,XML_temp,T_ID_ARROW)
      txml <- change_style(txml,T_ID_ARROW$id[x],"strokeColor",ARROW_COLOR,XML_temp,T_ID_ARROW)
    }
    # print(T_ID_ARROW)
    # aux <- T_ID_ARROW$arrowColor[x]
    # if(!is.na(aux)){
    #   txml <- change_style(txml,T_ID_ARROW$id[x],"fillColor",aux,XML_temp,T_ID_ARROW)
    #   txml <- change_style(txml,T_ID_ARROW$id[x],"strokeColor",aux,XML_temp,T_ID_ARROW)
    # }




    if(!is.na(T_ID_ARROW$labelchange[x]) & PLOT_CHANGE){ # & !is.na(value_change)){
      # if(!is.na(T_ID_ARROW$labelchange[x])){ # & !is.na(value_change)){

      lbchange <- T_ID_ARROW$labelchange[x]
      if(dato==0 || is.na(value_change)){ # si antes era 0 o el nuevo es 0. Si el nuevo es 0 implica reduccion de -100 pero no hay que ponerlo
        # print(str_replace_all(str_replace_all(lbchange,">",""),"<",""))
        # if(!SIMPLIFIED_PLOT){
        txml <- remove_id(txml,str_replace_all(str_replace_all(lbchange,">",""),"<",""))
        # }
      }else{

        if(abs(value_change)>10){
          tdato_change_xml <- paste0(round(value_change,0),"%")
        }else{
          tdato_change_xml <- paste0(round(value_change,1),"%")
        }
        if(value_change>0){
          tdato_change_xml <- paste0("+",tdato_change_xml)
        }

        # if(grepl("XDEPGR",T_ID_ARROW$labelchange[x])){
        #   asojioausj
        # }

        # INCREASE_COLOR <- "#FF0000"
        # DECREASE_COLOR <- "#00FF00"
        if(value_change>0){

          txml <- change_style(txml,T_ID_ARROW$labelchange[x],"fillColor",INCREASE_COLOR,XML_temp,T_ID_ARROW)
        }else{
          txml <- change_style(txml,T_ID_ARROW$labelchange[x],"fillColor",DECREASE_COLOR,XML_temp,T_ID_ARROW)
        }

        absval <- abs(value_change)
        mintam <- 30
        maxtam <- 50
        aux <- min(absval,100)
        desp <- (maxtam-mintam)*aux/100
        valfin <- round(mintam+desp)

        tamtexto <- 1
        if(absval>00){ tamtexto <- 10 }
        # if(absval>15){ tamtexto <- 9 }
        if(absval>30){ tamtexto <- 10 }
        if(absval>45){ tamtexto <- 11 }
        if(absval>60){ tamtexto <- 12 }
        if(absval>75){ tamtexto <- 13 }
        if(absval>90){ tamtexto <- 14 }

        txml <- change_bubble_size(txml,T_ID_ARROW$labelchange[x],valfin,tamtexto)

        # print(valfin)
        # print(T_ID_ARROW$labelchange[x])
        # txml <- change_style(txml,T_ID_ARROW$labelchange[x],"fontSize",tamtexto*10,XML_temp,T_ID_ARROW)
        # txml <- change_style(txml,T_ID_ARROW$labelchange[x],"width",paste0('"',valfin*10,'"'),XML_temp,T_ID_ARROW)
        # txml <- change_style(txml,T_ID_ARROW$labelchange[x],"height",paste0('"',valfin*10,'"'),XML_temp,T_ID_ARROW)




        #   newauxt <- str_replace(newauxt,paste0("fontSize=",i),paste0("fontSize=",NEW_TEXT_SIZE))
        # }
        # for(i in c(50.5,50,30)){
        #   newauxt <- str_replace(newauxt,paste0('width="',i,'"'),paste0('width="',NEW_SIZE,'"'))
        #   newauxt <- str_replace(newauxt,paste0('height="',i,'"'),paste0('height="',NEW_SIZE,'"'))
        # }
        #


        txml <- str_replace_all(txml,str_replace_all(str_replace_all(lbchange,">","&amp;gt;"),"<","&amp;lt;"),
                                str_replace_all(tdato_change_xml," ",""))
      }
    }

  }


  return(txml)
}


crea_png <- function(exe_draw_io,xml_in,png_out){

  p <- process$new(exe_draw_io, c("-x",paste0("-o",png_out),xml_in))
  # p <- process$new(paste0(PATH_DRAW_IO_EXE,"draw.io"), c("-x","-oC:/draw/test2.png","C:/draw/GRAFS_spain_1990.xml"))

  p$wait()
  print(paste0(png_out," created!"))
  # p2$get_exit_status()

}

year_info <- function(YEARS){
  label <- YEARS[1]
  for(i in 2:length(YEARS)){
    if(YEARS[i]!=(YEARS[i-1]+1)){
      label <- paste0(label,"-",YEARS[i-1],"_",YEARS[i])
    }else{
      if(i==length(YEARS)){
        label <- paste0(label,"-",YEARS[i])
      }
    }
  }
  for(i in 1:length(unique(YEARS))){
    label <- str_replace_all(label,paste0(YEARS[i],"-",YEARS[i]),paste0(YEARS[i],""))
  }
  return(label)
}

create_GRAFS <- function(XLSX_INPUTS,
                         PATH_OUTPUTS,
                         XML_BASE,
                         TABLA_ID_XML,
                         DRAW_IO_EXE,
                         REGIONS,
                         PERIODS,
                         DECIMALES_XML=0,
                         MAX_WIDTH_ARROWS=25,
                         VAL_MAX_WIDTH=1000,
                         INCREASE_COLOR="#97cde5",
                         DECREASE_COLOR="#a9d77f",
                         OVERWRITE=TRUE,
                         VERBOSE=FALSE,
                         UNITch=NA,
                         LABELch=NA,
                         DATOch=NA){

  T_ID_ARROW <- openxlsx::read.xlsx(TABLA_ID_XML)


  dir.create(PATH_OUTPUTS,showWarnings = F)
  dir.create(paste0(PATH_OUTPUTS,"xml/"),showWarnings = F)
  dir.create(paste0(PATH_OUTPUTS,"png/"),showWarnings = F)

  XML_temp <- str_replace_all(XML_BASE,".xml","_temp.xml")


  d <- openxlsx::read.xlsx(XLSX_INPUTS)
  d <- unique(d)

  if(length(unique(d$year))==1){
    if(unique(d$year)==9999){
      d2 <- d
      d2$year <- 9998
      d <- rbind(d,d2)
    }}

  # antes se ponia en el excel, si lo tiene lo quito
  d <- subset(d,!grepl("WIDTH_MAX",label))


  for(PERIOD in 1:length(PERIODS)){

    tPER <- PERIODS[PERIOD]
    if(grepl("-",tPER)){
      PLOT_CHANGE <- T
      aux <- str_split(tPER,"-")
      aux <- aux[[1]]
      auxt <- paste0("YEARS <- ",aux[1])
      eval(parse(text=auxt))
      auxt <- paste0("YEARS_CHANGE <- ",aux[2])
      eval(parse(text=auxt))
    }else{
      PLOT_CHANGE <- F
      auxt <- paste0("YEARS <- ",tPER)
      eval(parse(text=auxt))
    }

    for(PROV_ACT in REGIONS){

      dact <- subset(d,province==PROV_ACT & is.element(year,YEARS))

      if(PLOT_CHANGE){
        dactch <- subset(d,province==PROV_ACT & is.element(year,YEARS_CHANGE))
      }



      FACT_XML <- paste0(PATH_OUTPUTS,"xml/","GRAFS_",PROV_ACT,"_P",PERIOD,"_MEAN_",year_info(YEARS),".xml")
      if(file.exists(FACT_XML) & !OVERWRITE){
        next
      }


      txml <- readChar(XML_BASE, file.info(XML_BASE)$size)

      if(PLOT_CHANGE){
        txml <- change(txml,"<YEARCHANGE>",paste0("(changes_",YEARS_CHANGE[1],"-",max(YEARS_CHANGE),")"),T_ID_ARROW=T_ID_ARROW,VAL_MAX_WIDTH=VAL_MAX_WIDTH,DECIMALES_XML=DECIMALES_XML,XML_temp=XML_temp,MAX_WIDTH_ARROWS=MAX_WIDTH_ARROWS,PLOT_CHANGE=PLOT_CHANGE,DATOch=DATOch,
                       INCREASE_COLOR=INCREASE_COLOR,DECREASE_COLOR=DECREASE_COLOR)
      }else{
        txml <- change(txml,"<YEARCHANGE>","",T_ID_ARROW=T_ID_ARROW,VAL_MAX_WIDTH=VAL_MAX_WIDTH,DECIMALES_XML=DECIMALES_XML,XML_temp=XML_temp,MAX_WIDTH_ARROWS=MAX_WIDTH_ARROWS,PLOT_CHANGE=PLOT_CHANGE,DATOch=DATOch,
                       INCREASE_COLOR=INCREASE_COLOR,DECREASE_COLOR=DECREASE_COLOR)
      }

      LABELch_flat <- unlist(LABELch)

      if (!any(is.na(LABELch_flat))) {
        # if(!is.na(LABELch)){
        for(ii in 1:length(LABELch)){
          change(txml,LABELch[[ii]]$old,LABELch[[ii]]$new,T_ID_ARROW=T_ID_ARROW,VAL_MAX_WIDTH=VAL_MAX_WIDTH,DECIMALES_XML=DECIMALES_XML,XML_temp=XML_temp,MAX_WIDTH_ARROWS=MAX_WIDTH_ARROWS,PLOT_CHANGE=PLOT_CHANGE,DATOch=DATOch)
        }
      }

      txml <- change(txml,"<WIDTH_MAX>",VAL_MAX_WIDTH,T_ID_ARROW=T_ID_ARROW,VAL_MAX_WIDTH=VAL_MAX_WIDTH,DECIMALES_XML=DECIMALES_XML,XML_temp=XML_temp,MAX_WIDTH_ARROWS=MAX_WIDTH_ARROWS,PLOT_CHANGE=PLOT_CHANGE,DATOch=DATOch)

      # Add the production if it is not stated
      x <- which(dact$label=="<CRPLNDTOTN>")
      if(length(x)==0){
        xx <- subset(dact,is.element(label,c("<PERrN>","<PERiN>","<NPErN>","<NPEiN>","<GREHN>")))
        xx$data <- as.numeric(xx$data)
        xxx <- xx %>% group_by(province,year,align,arrowColor) %>% summarise(label="<CRPLNDTOTN>",data=sum(data))
        # print(xxx)
        xxx <- xxx[,c("province","year","label","data","align","arrowColor")]
        dact <- rbind(dact,xxx)

        if(PLOT_CHANGE){
          # El cambio a?ado NA para que no de error pero no tengo que calcularlo
          xxxch <- xxx
          xxxch$data <- NA
          dactch <- rbind(dactch,xxx)
        }
      }



      UNITch_flat <- unlist(UNITch)

      if (!any(is.na(UNITch_flat))) {
        # Cambios de unidades
        # if(PROV_ACT!="spain"){
        # if(!is.na(UNITch)){
        for(ii in 1:length(UNITch)){
          dact$data[dact$label==UNITch[[ii]]$old] <- as.numeric(dact$data[dact$label==UNITch[[ii]]$old])/UNITch[[ii]]$div
          dact$label[dact$label==UNITch[[ii]]$old] <- UNITch[[ii]]$new
          dactch$label[dactch$label==UNITch[[ii]]$old] <- UNITch[[ii]]$new
        }
      }

      for(lact in unique(dact$label)){

        x <- dact[dact$label==lact,]
        if(PLOT_CHANGE){
          xch <- dactch[dactch$label==lact,]
        }

        if(length(x[,1])!=length(YEARS)){
          something_is_missing_check
        }
        if(PLOT_CHANGE){
          if(length(xch[,1])!=length(YEARS_CHANGE)){
            something_is_missing_check
          }
        }

        # cojo la moda
        # arrowcolor <- as.numeric(names(sort(table(x$arrowColor), decreasing = TRUE)[1]))
        # if(length(arrowcolor)==0){
        #   arrowcolor <- NA
        # }
        arrowcolor <- unique(x$arrowColor)

        value_change <- NA
        if(lact=="<PROVINCE_NAME>"){
          mean_val <- x$data[1]

        }else{
          if(lact=="<YEAR>"){
            mean_val <- paste0("mean_",min(YEARS),"-",max(YEARS))

          }else{

            x$data <- as.numeric(x$data)
            mean_val <- round(mean(x$data),DECIMALES_XML) #max(DECIMALES_TXT,DECIMALES_XML))
            if(mean_val==0){
              mean_val <- round(mean(x$data),DECIMALES_XML+1)
            }

            if(PLOT_CHANGE){
              xch$data <- as.numeric(xch$data)
              value_change <- mean(x$data)*100/mean(xch$data)-100

              if(mean(xch$data)==0){ # el base era 0
                value_change <- NA
              }
            }
          }
        }
        # print(arrowcolor)
        txml <- change(txml,lact,mean_val,x$align[1],value_change,T_ID_ARROW=T_ID_ARROW,VAL_MAX_WIDTH=VAL_MAX_WIDTH,DECIMALES_XML=DECIMALES_XML,XML_temp=XML_temp,MAX_WIDTH_ARROWS=MAX_WIDTH_ARROWS,PLOT_CHANGE=PLOT_CHANGE,DATOch=DATOch,ARROW_COLOR=arrowcolor)
        # print(txml)
        # print(paste0("LACT: ",lact," MEAN: ",mean_val," value change ",value_change))
      }


      # print("asadsd")
      if(!PLOT_CHANGE){
        txml <- remove_change_bubbles(txml)
      }


      fd <- file(FACT_XML, "wb")
      writeChar(txml, fd, eos = NULL)
      close(fd)

      crea_png(DRAW_IO_EXE,FACT_XML,str_replace_all(FACT_XML,"xml","png"))
      print(paste0("GRAFS creado!"))

    }
  }
}
