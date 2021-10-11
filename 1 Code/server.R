server <- function(input, output, session){

  
################################################################################
##                              Section 1                                     ##
################################################################################

  # first reactive dataset
  df_sub_1 = reactiveVal(NULL)

  observe({
    df_sub_1(
      crData(geovar = input$geo1,
             sexvar = input$sex1,
             agevar = input$age1,
             time1 = input$time1[[1]],
             time2 = input$time1[[2]])
    )
  })
  
  # BARPLOTS DATASETS-----------------------------------------------------------
  # update FIRST slider
  observe({
    req(df_sub_1()$Total_first)
    updateSliderInput(
      session = session,
      inputId = "threshold1",
      min = min(df_sub_1()$Total_first, na.rm = T)+1,
      max = max(df_sub_1()$Total_first, na.rm = T),
      value = c(min(df_sub_1()$Total_first, na.rm = T)+0.05*max(df_sub_1()$Total_first, na.rm = T),
                max(df_sub_1()$Total_first, na.rm = T))
    )
  })

  # update FIRST dataset
  df_sub_11 = reactiveVal(NULL)
  
  observe({
    req(df_sub_1())
    df_sub_11(
      upData(data = df_sub_1(),
             trvar = df_sub_1()$Total_first,
             threshold1 = input$threshold1[[1]],
             threshold2 = input$threshold1[[2]])
    )
  })
  
  # update SECOND slider
  observe({
    req(df_sub_1()$Total_def)
    updateSliderInput(
      session = session,
      inputId = "threshold2",
      min = min(df_sub_1()$Total_def, na.rm = T)+1,
      max = max(df_sub_1()$Total_def, na.rm = T),
      value = c(min(df_sub_1()$Total_def, na.rm = T)+0.05*max(df_sub_1()$Total_def, na.rm = T),
                max(df_sub_1()$Total_def, na.rm = T))
    )
  })
  
  # update SECOND dataset
  df_sub_12 = reactiveVal(NULL)
  
  observe({
    req(df_sub_1())
    df_sub_12(
      upData(data = df_sub_1(),
             trvar = df_sub_1()$Total_def,
             threshold1 = input$threshold2[[1]],
             threshold2 = input$threshold2[[2]])
    )
  })
  
  
  
  # FIRST ISTANCE BARPLOT ------------------------------------------------------
  output$bar1 = renderPlotly({
    barDef(data = df_sub_11(),
           decisions = "Total_first",
           pos_decisions = "Total_pos_first",
           name = "first istance",
           genconv = df_sub_11()$Genconv_first,
           humstat = df_sub_11()$Humstat_first,
           subprot = df_sub_11()$Sub_prot_first,
           RR = "RR_f",
           geo = input$geo1,
           time1 = input$time1[[1]],
           time2 = input$time1[[2]],
           sortvar = df_sub_11()$Total_first,
           denom = df_sub_11()$Total_pos_first)
  })
  
  output$rrfirst = renderText({
    req(df_sub_1())
    paste("Overall RR by first istance of", input$geo1, "in", input$time1[[1]],"-",input$time2[[2]], ": ", round(sum(df_sub_1()$Total_pos_first, na.rm = T)/sum(df_sub_1()$Total_first, na.rm = T), digits = 4)*100, "%")
  })
  
  
  # DEFINITIVE DECISIONS BARPLOT -----------------------------------------------

  output$bar2 = renderPlotly({
    barDef(data = df_sub_12(),
           decisions = "Total_def",
           pos_decisions = "Total_pos_def",
           name = "definitive decision",
           genconv = df_sub_12()$Genconv_def,
           humstat = df_sub_12()$Humstat_def,
           subprot = df_sub_12()$Sub_prot_def,
           RR = "RR_d",
           geo = input$geo1,
           time1 = input$time1[[1]],
           time2 = input$time1[[2]],
           sortvar = df_sub_12()$Total_def,
           denom = df_sub_12()$Total_pos_def)
  })
  
  output$rrdef = renderText({
    req(df_sub_1())
    paste("Overall RR by definitive decision of", input$geo1, "in", input$time1[[1]],"-",input$time2[[2]], ": ", round(sum(df_sub_1()$Total_pos_def, na.rm = T)/sum(df_sub_1()$Total_def, na.rm = T), digits = 4)*100, "%")
  })
  
  
################################################################################
##                              Section 2                                     ##
################################################################################

  # second reactive dataset
  df_sub_2 = reactiveVal(NULL)
  
  observe({
    df_sub_2(
      crData(geovar = input$geo2,
             sexvar = input$sex2,
             agevar = input$age2,
             time1 = input$time2[[1]],
             time2 = input$time2[[2]])
    )
  })
  
  # PIECHART DATASETS-----------------------------------------------------------
  # update FIRST slider ####
  observe({
    req(df_sub_2()$Applications)
    updateSliderInput(
      session = session,
      inputId = "threshold3",
      min = min(df_sub_2()$Applications, na.rm = T)+1,
      max = max(df_sub_2()$Applications, na.rm = T),
      value = c(min(df_sub_2()$Applications, na.rm = T)+0.15*max(df_sub_2()$Applications, na.rm = T),
                max(df_sub_2()$Applications, na.rm = T))
    )
  })
  
  # update FIRST dataset
  df_sub_21 = reactiveVal(NULL)
  
  observe({
    req(df_sub_2())
    df_sub_21(
      upDatageo(data = df_sub_2(),
             trvar = df_sub_2()$Applications,
             threshold1 = input$threshold3[[1]],
             threshold2 = input$threshold3[[2]])
    )
  })
  
  # update SECOND slider ####
  observe({
    req(df_sub_2()$Total_pos_first)
    updateSliderInput(
      session = session,
      inputId = "threshold4",
      min = min(df_sub_2()$Total_pos_first, na.rm = T)+1,
      max = max(df_sub_2()$Total_pos_first, na.rm = T),
      value = c(min(df_sub_2()$Total_pos_first, na.rm = T)+0.15*max(df_sub_2()$Total_pos_first, na.rm = T),
                max(df_sub_2()$Total_pos_first, na.rm = T))
    )
  })
  
  # update SECOND dataset
  df_sub_22 = reactiveVal(NULL)
  
  observe({
    req(df_sub_2())
    df_sub_22(
      upDatageo(data = df_sub_2(),
             trvar = df_sub_2()$Total_pos_first,
             threshold1 = input$threshold4[[1]],
             threshold2 = input$threshold4[[2]])
    )
  })
  
  # update THIRD slider ####
  observe({
    req(df_sub_2()$Total_pos_def)
    updateSliderInput(
      session = session,
      inputId = "threshold5",
      min = min(df_sub_2()$Total_pos_def, na.rm = T)+1,
      max = max(df_sub_2()$Total_pos_def, na.rm = T),
      value = c(min(df_sub_2()$Total_pos_def, na.rm = T)+0.15*max(df_sub_2()$Total_pos_def, na.rm = T),
                max(df_sub_2()$Total_pos_def, na.rm = T))
    )
  })
  
  # update THIRD dataset
  df_sub_23 = reactiveVal(NULL)
  
  observe({
    req(df_sub_2())
    df_sub_23(
      upDatageo(data = df_sub_2(),
             trvar = df_sub_2()$Total_pos_def,
             threshold1 = input$threshold5[[1]],
             threshold2 = input$threshold5[[2]])
    )
  })
         
  # GEO PIECHART----------------------------------------------------------------
  
  # Appl pie
  output$applpie = renderPlotly({pieDefgeo(data = df_sub_21(),
                             variable = "Applications")})
  
  output$appltext = renderText({
    paste("Total asylum applications in", input$geo2, "in the period", input$time2[[1]], "-",input$time2[[2]], ": ", sum(df_sub_21()$Applications))
  })
  
  # First pie
  output$firstpie = renderPlotly({pieDefgeo(data = df_sub_22(),
                            variable = "Total_pos_first")})
  
  output$firsttext = renderText({
    req(df_sub_22())
    a = paste("Total asylums granted by", input$geo2, "by first istance in the period", input$time2[[1]], "-",input$time2[[2]], ": ", sum(df_sub_22()$Total_pos_first))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_22()$Genconv_first))
    d = paste("  - Humanitaran status:", sum(df_sub_22()$Humstat_first))
    e = paste("  - Subsidiary protection:", sum(df_sub_22()$Sub_prot_first))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))
    
  })
  
  # Def pie
  output$defpie = renderPlotly({pieDefgeo(data = df_sub_23(),
                            variable = "Total_pos_def")})
  
  output$deftext = renderText({
    req(df_sub_23())
    a = paste("Total asylums granted by", input$geo2, "by definitive decision in the period", input$time2[[1]], "-",input$time2[[2]], ": ", sum(df_sub_23()$Total_pos_def))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_23()$Genconv_def))
    d = paste("  - Humanitaran status:", sum(df_sub_23()$Humstat_def))
    e = paste("  - Subsidiary protection:", sum(df_sub_23()$Sub_prot_def))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))
  })
  
  
  
  
################################################################################
##                              Section 3                                     ##
################################################################################
  
  # third reactive dataset
  df_sub_3 = reactiveVal(NULL)
  
  observe({
    df_sub_3(
      crDatacit(geovar = input$ctz,
                sexvar = input$sex3,
                agevar = input$age3,
                time1 = input$time3[[1]],
                time2 = input$time3[[2]])
    )
  })
  
  # PIECHART DATASETS-----------------------------------------------------------
  # update FIRST slider ####
  observe({
    req(df_sub_3()$Applications)
    updateSliderInput(
      session = session,
      inputId = "threshold6",
      min = min(df_sub_3()$Applications, na.rm = T)+1,
      max = max(df_sub_3()$Applications, na.rm = T),
      value = c(min(df_sub_3()$Applications, na.rm = T)+0.05*max(df_sub_3()$Applications, na.rm = T),
                max(df_sub_3()$Applications, na.rm = T))
    )
  })
  
  # update FIRST dataset
  df_sub_31 = reactiveVal(NULL)
  
  observe({
    req(df_sub_3())
    df_sub_31(
      upDatacit(data = df_sub_3(),
             trvar = df_sub_3()$Applications,
             threshold1 = input$threshold6[[1]],
             threshold2 = input$threshold6[[2]])
    )
  })
  
  # update SECOND slider ####
  observe({
    req(df_sub_3()$Total_pos_first)
    updateSliderInput(
      session = session,
      inputId = "threshold7",
      min = min(df_sub_3()$Total_pos_first, na.rm = T)+1,
      max = max(df_sub_3()$Total_pos_first, na.rm = T),
      value = c(min(df_sub_3()$Total_pos_first, na.rm = T)+0.05*max(df_sub_3()$Total_pos_first, na.rm = T),
                max(df_sub_3()$Total_pos_first, na.rm = T))
    )
  })
  
  # update SECOND dataset
  df_sub_32 = reactiveVal(NULL)
  
  observe({
    req(df_sub_3())
    df_sub_32(
      upDatacit(data = df_sub_3(),
             trvar = df_sub_3()$Total_pos_first,
             threshold1 = input$threshold7[[1]],
             threshold2 = input$threshold7[[2]])
    )
  })
  
  # update THIRD slider ####
  observe({
    req(df_sub_3()$Total_pos_def)
    updateSliderInput(
      session = session,
      inputId = "threshold8",
      min = min(df_sub_3()$Total_pos_def, na.rm = T)+1,
      max = max(df_sub_3()$Total_pos_def, na.rm = T),
      value = c(min(df_sub_3()$Total_pos_def, na.rm = T)+0.05*max(df_sub_3()$Total_pos_def, na.rm = T),
                max(df_sub_3()$Total_pos_def, na.rm = T))
    )
  })
  
  # update THIRD dataset
  df_sub_33 = reactiveVal(NULL)
  
  observe({
    req(df_sub_3())
    df_sub_33(
      upDatacit(data = df_sub_3(),
             trvar = df_sub_3()$Total_pos_def,
             threshold1 = input$threshold8[[1]],
             threshold2 = input$threshold8[[2]])
    )
  })
  
  # CITIZEN PIECHART----------------------------------------------------------------
  
  # Appl pie
  output$applpie_cit = renderPlotly({pieDefcit(data = df_sub_31(),
                                      variable = "Applications")})
  
  output$appltext_cit = renderText({
    paste("Total asylum applications from", input$ctz, "in the period", input$time3[[1]], "-",input$time3[[2]], ": ", sum(df_sub_31()$Applications))
  })
  
  # First pie
  output$firstpie_cit = renderPlotly({pieDefcit(data = df_sub_32(),
                                      variable = "Total_pos_first")})
  
  output$firsttext_cit = renderText({
    req(df_sub_22())
    a = paste("Total asylums granted to", input$ctz, "by first istance in the period", input$time3[[1]], "-",input$time3[[2]], ": ", sum(df_sub_32()$Total_pos_first))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_32()$Genconv_first))
    d = paste("  - Humanitaran status:", sum(df_sub_32()$Humstat_first))
    e = paste("  - Subsidiary protection:", sum(df_sub_32()$Sub_prot_first))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))
    
  })
  
  # Def pie
  output$defpie_cit = renderPlotly({pieDefcit(data = df_sub_33(),
                                    variable = "Total_pos_def")})
  
  output$deftext_cit = renderText({
    req(df_sub_33())
    a = paste("Total asylums granted to", input$geo3, "by definitive decision in the period", input$time3[[1]], "-",input$time3[[2]], ": ", sum(df_sub_33()$Total_pos_def))
    b = "Of which by: "
    c = paste("  - Geneve convention:", sum(df_sub_33()$Genconv_def))
    d = paste("  - Humanitaran status:", sum(df_sub_33()$Humstat_def))
    e = paste("  - Subsidiary protection:", sum(df_sub_33()$Sub_prot_def))
    HTML(paste(a, b, c, d, e, sep = '<br/>'))
  })
  
 
  
  
  
}

