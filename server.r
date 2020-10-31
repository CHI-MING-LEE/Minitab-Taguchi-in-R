library(shiny)
library(shinydashboard)
library(htmlTable)
library(stringi)
library(DoE.base)
library(rhandsontable)
library(zoo)
# library(DT) conflict
library(magrittr)

single_level_table <- read.csv("www/single_level_table.csv")
mixed_level_table <- read.csv("www/mixed_level_table.csv")
L18_21_31 <- read.csv("www/L18_21_31.csv", header = T, stringsAsFactors = F, row.names = NULL)
L36_21_32 <- read.csv("www/L18_21_31.csv", header = T, stringsAsFactors = F, row.names = NULL)
source("function.r")

server <- function(input, output, session) {
  # Taguchi----
# available table-----
  observeEvent(input$show_design,{

      # Single level
      aval_table_single <- as.data.frame(matrix(c(paste0("L",c(4,8,9,12,16,16,25,27,32)),"2-3","2-7","","2-11","2-15",rep("",3),"2-31",
                                                  rep("",2),"2-4",rep("",4),"2-13","",
                                                  rep("",5),"2-5",rep("",3),
                                                  rep("",6),"2-6",rep("",2)),
                                                  ncol=5,nrow=9,byrow = F))
      colnames(aval_table_single) <- c("Design",paste0(2:5,"level"))
      # Mixed 2-3 level
      aval_table_mx23 <- as.data.frame(matrix(c(paste("L",c(18,36,36,54)),
                                      "1","1-11","1-3","1","1-7","2-12","13","3-25"),
                                      ncol=3,nrow=4,byrow = F))
      colnames(aval_table_mx23) <- c("Designs",paste0(2:3,"level"))
      # Mixed 2-4 level
      aval_table_mx24 <- as.data.frame(matrix(c(paste("L",c(8,16,16,16,16,32))
                                      ,"1-4","2-12","1-9","1-6","1-3","1","1","1","2","3","4","2-9"),
                                      ncol=3,nrow=6,byrow = F))
      colnames(aval_table_mx24) <- c("Designs",paste0(c(2,4),"level"))
      # Mixed 2-8 level
      aval_table_mx28 <- as.data.frame(matrix(c(paste("L",c(16)),"1-8","1"),
                         ncol=3,nrow=1,byrow = F))
      colnames(aval_table_mx28) <- c("Designs",paste0(c(2,8),"level"))
      # Mixed 3-6 level
      aval_table_mx36 <- as.data.frame(matrix(c(paste("L",c(18)),"1-6","1"),
                         ncol=3,nrow=1,byrow = F))
      colnames(aval_table_mx36) <- c("Designs",paste0(c(3,6),"level"))
    
      showModal(modalDialog(size = "m",
                            title = "Available Taguchi Designs (with Number of Factors)",
                            div("Single-level designs",style="text-align:center;font-weight:bold;font-size:120%;background-color:#AED6F1"),
                            renderDataTable({aval_table_single}, options = list(searching = F, paging = F)),
                            #renderHtmlTableWidget({htmlTableWidget(aval_table_single, number_of_entries = 20)}),
                            div("Mixed 2-3 level designs",style="text-align:center;font-weight:bold;font-size:120%;background-color:#AED6F1"),
                            renderDataTable({aval_table_mx23}, options = list(searching = F, paging = F)),
                            #renderHtmlTableWidget({htmlTableWidget(aval_table_mx23)}),
                            div("Mixed 2-4 level designs",style="text-align:center;font-weight:bold;font-size:120%;background-color:#AED6F1"),
                            renderDataTable({aval_table_mx24}, options = list(searching = F, paging = F)),
                            #renderHtmlTableWidget({htmlTableWidget(aval_table_mx24)}),
                            div("Mixed 2-8 level designs",style="text-align:center;font-weight:bold;font-size:120%;background-color:#AED6F1"),
                            renderDataTable({aval_table_mx28}, options = list(searching = F, paging = F)),
                            #renderHtmlTableWidget({htmlTableWidget(aval_table_mx28)}),
                            div("Mixed 3-6 level designs",style="text-align:center;font-weight:bold;font-size:120%;background-color:#AED6F1"),
                            renderDataTable({aval_table_mx36}, options = list(searching = F, paging = F))
                            #renderHtmlTableWidget({htmlTableWidget(aval_table_mx36)})
                            ,easyClose = T,footer = modalButton("OK")
      ))
  })
  
  # Number of factors
  output$Num_of_factors<-renderUI({
      switch(input$type_of_design,
             "2-Level Design (2-31 factors)" =  selectInput("single_2", label = "Single-2-Level Design", choices = 2:31),
             "3-Level Design (2-13 factors)" = selectInput("single_3", label = "Single-3-Level Design", choices = 2:13),
             "4-Level Design (2-5 factors)" = selectInput("single_4", label = "Single-4-Level Design", choices = 2:5),
             "5-Level Design (2-6 factors)" = selectInput("single_5", label = "Single-5-Level Design", choices = 2:6),
             "Mixed Level Design (2 to 26 factors)" = selectInput("mixed_factor", label = "Mixed Level Design", choices = 2:26)
      )
  })
# Design button (head)----
  observeEvent(input$designs,{
    # 2 single-level
    if(input$type_of_design=="2-Level Design (2-31 factors)"){
         # extract name list (refer to function.r)
         choice.names <- table_extract(level_table = single_level_table, lv = 2, fct = input$single_2, cate = "single")
         # name runs levels factors
         print("design2")
         output$select_taguchi_table <- renderUI({
            selectInput("single_2_aval_table", HTML("&nbsp&nbsp&nbsp&nbsp Name &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp 
                                                  Runs &nbsp&nbsp Levels &nbsp&nbsp Factors"), choices = choice.names)
         })
    }
    # 3 single-level  
    else if(input$type_of_design=="3-Level Design (2-13 factors)"){
        choice.names <- table_extract(level_table = single_level_table, lv = 3, fct = input$single_3, cate = "single")
        print("design3")
        output$select_taguchi_table <- renderUI({
          selectInput("single_3_aval_table", HTML("&nbsp&nbsp&nbsp&nbsp Name &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp 
                                                    Runs &nbsp&nbsp Levels &nbsp&nbsp Factors"), choices = choice.names)
        })
    }
    # 4 single-level  
    else if(input$type_of_design=="4-Level Design (2-5 factors)"){
        choice.names <- table_extract(level_table = single_level_table, lv = 4, fct = input$single_4, cate = "single")
        print("design4")
        output$select_taguchi_table <- renderUI({
          selectInput("single_4_aval_table", HTML("&nbsp&nbsp&nbsp&nbsp Name &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp 
                                                      Runs &nbsp&nbsp Levels &nbsp&nbsp Factors"), choices = choice.names)
        })
    }
    # 5 single-level  
    else if(input$type_of_design=="5-Level Design (2-6 factors)"){
        choice.names <- table_extract(level_table = single_level_table, lv = 5, fct = input$single_5, cate = "single")
        print("design5")
        output$select_taguchi_table <- renderUI({
          selectInput("single_5_aval_table", HTML("&nbsp&nbsp&nbsp&nbsp Name &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp 
                                                        Runs &nbsp&nbsp Levels &nbsp&nbsp Factors"), choices = choice.names)
        })
    }
    else{
    # Mixed-level  
      # extract the table
      choice_mx.names <- table_extract(level_table = mixed_level_table, fct_sum = input$mixed_factor, cate = "mixed")
      # name runs levels factors
      print("design mixed")
      output$select_taguchi_table <- renderUI({
        selectInput("mixed_aval_table", HTML("&nbsp&nbsp&nbsp&nbsp Name &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp Runs 
                                  &nbsp&nbsp Level_1 &nbsp&nbsp Factors_1 &nbsp&nbsp Level_2 &nbsp&nbsp Factors_2"), choices = choice_mx.names)
      })
    }
# common shiny widgets for type_of_designs----
      # generate_taguchi_table button
      output$generate_taguchi_table <- renderUI({
        actionButton("generate_table","Generate Taguchi Table",style="color: #fff; background-color: #C0392B; border-color:#C0392B;font-weight:bold")
      })
      # signal factor
      if(input$add_signal){
        output$signal_numeric <- renderUI({
          numericInput("signal_level", label = "Levels of signal factor ", value = 2, min = 2)
        })
      }
      # revise Factors
      output$revise_fac <- renderUI({
        actionButton("revise_factor", label = "Factors", style="color: #fff; background-color: #C0392B; border-color:#C0392B;font-weight:bold")
      })
  })
# Design button (tail)----
  
# generate taguchi table (without handsontable changes) (head)----
  tmp_extraction <- reactive({
    # single level
    if(!input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
        if(input$type_of_design=="2-Level Design (2-31 factors)"){
          single_2_para <- get_param(cate = "single", tb_csv = single_level_table, usr_chs = input$single_2_aval_table, lv = 2, fct = input$single_2)
          attach(single_2_para)
          print("tmp2")
          # if nruns is larger than (factors^levels), it must be replicated (refer to function.r)
          # generate taguchi table with or without a signal factor ----
          generate_taguchi(lv = 2, runs = runs, fct = factors, sig_boolean = input$add_signal, sig_lv = input$signal_level, cate = "single")
        }
        else if(input$type_of_design=="3-Level Design (2-13 factors)"){
          single_3_para <- get_param(cate = "single", tb_csv = single_level_table, usr_chs = input$single_3_aval_table, lv = 3, fct = input$single_3)
          attach(single_3_para)
          print("tmp3")
          generate_taguchi(lv = 3, runs = runs, fct = factors, sig_boolean = input$add_signal, sig_lv = input$signal_level, cate = "single")
        }
        else if(input$type_of_design=="4-Level Design (2-5 factors)"){
          single_4_para <- get_param(cate = "single", tb_csv = single_level_table, usr_chs = input$single_4_aval_table, lv = 4, fct = input$single_4)
          attach(single_4_para)
          print("tmp4")
          generate_taguchi(lv = 4, runs = runs, fct = factors, sig_boolean = input$add_signal, sig_lv = input$signal_level, cate = "single")
        }
        else if(input$type_of_design=="5-Level Design (2-6 factors)"){
          single_5_para <- get_param(cate = "single", tb_csv = single_level_table, usr_chs = input$single_5_aval_table, lv = 5, fct = input$single_5)
          attach(single_5_para)
          print("tmp5")
          generate_taguchi(lv = 5, runs = runs, fct = factors, sig_boolean = input$add_signal, sig_lv = input$signal_level, cate = "single")
        }
    }
    # mixed level
    else{
        if(is.null(input$mixed_aval_table)){
            return(NULL)
        }
        else{
            mx_para <- get_param(cate = "mixed", tb_csv = mixed_level_table, usr_chs = input$mixed_aval_table, fct_sum = input$mixed_factor)
            attach(mx_para)
            if(level_a^factors_a*level_b^factors_b < runs){ # 組合數不夠，例外兩個
                if(runs == 18){
                    plan_mx <- L18_21_31
                    # if signal is cheched
                    if(input$add_signal & !is.null(input$signal_level)){
                      plan_mx <- addsig(DA = plan_mx, sig_level = 1:input$signal_level)
                    }
                    return(list(plan_mx=plan_mx,level_a=2,level_b=3,factors_a=1,factors_b=1))
                }
                if(runs == 36){
                    plan_mx <- L36_21_32
                    # if signal is cheched
                    if(input$add_signal & !is.null(input$signal_level)){
                      plan_mx <- addsig(DA = plan_mx, sig_level = 1:input$signal_level)
                    }
                    return(list(plan_mx=plan_mx,level_a=2,level_b=3,factors_a=1,factors_b=2))
                }
            }
            else{
                plan_mx <- oa.design(nruns = runs, nlevels = c(rep(level_a,factors_a),rep(level_b,factors_b)))
                  # if signal is cheched
                  if(input$add_signal & !is.null(input$signal_level)){
                    plan_mx <- addsig(DA = plan_mx, sig_level = 1:input$signal_level)
                  }
                plan_mx <- as.data.frame(plan_mx)
                return(list(plan_mx=plan_mx,level_a=level_a,level_b=level_b,factors_a=factors_a,factors_b=factors_b))
            }
        }
    }
  })
# generate taguchi table (without handsontable changes) (tail)----

#if number of widget changed, reset----
  observeEvent(input$single_2,{
    output$generate_taguchi_table<-renderUI({
      return(NULL)
    })
    output$revise_fac<-renderUI({
      return(NULL)
    })
    output$select_taguchi_table <- renderUI({
      return(NULL)
    })
  })
  observeEvent(input$single_3,{
    output$generate_taguchi_table<-renderUI({
      return(NULL)
    })
    output$revise_fac<-renderUI({
      return(NULL)
    })
    output$select_taguchi_table <- renderUI({
      return(NULL)
    })
  })
  observeEvent(input$single_4,{
    output$generate_taguchi_table<-renderUI({
      return(NULL)
    })
    output$revise_fac<-renderUI({
      return(NULL)
    })
    output$select_taguchi_table <- renderUI({
      return(NULL)
    })
  })
  observeEvent(input$single_5,{
    output$generate_taguchi_table<-renderUI({
      return(NULL)
    })
    output$revise_fac<-renderUI({
      return(NULL)
    })
    output$select_taguchi_table <- renderUI({
      return(NULL)
    })
  })
  observeEvent(input$mixed_factor,{
    output$generate_taguchi_table<-renderUI({
      return(NULL)
    })
    output$revise_fac<-renderUI({
      return(NULL)
    })
    output$select_taguchi_table <- renderUI({
      return(NULL)
    })
  })
  observeEvent(input$type_of_design,{
    output$generate_taguchi_table<-renderUI({
      return(NULL)
    })
    output$revise_fac<-renderUI({
      return(NULL)
    })
    output$select_taguchi_table <- renderUI({
      return(NULL)
    })
  })
  observeEvent(input$add_signal,{
    if(input$add_signal==FALSE){ # unchecked
      output$signal_numeric <- renderUI({
        return(NULL)
      })
    }
    output$generate_taguchi_table<-renderUI({
      return(NULL)
    })
    output$revise_fac<-renderUI({
      return(NULL)
    })
    output$select_taguchi_table <- renderUI({
      return(NULL)
    })
  })
  
# Handsontable (head) ----
  # single level handson
  single_hst <- reactive({
      if(input$type_of_design=="2-Level Design (2-31 factors)"){
        generate_init_single(init_table = tmp_extraction(), fct = input$single_2, lv = 2)
      }
      else if(input$type_of_design=="3-Level Design (2-13 factors)"){
        print(tmp_extraction())
        generate_init_single(init_table = tmp_extraction(), fct = input$single_3, lv = 3)
      }
      else if(input$type_of_design=="4-Level Design (2-5 factors)"){
        generate_init_single(init_table = tmp_extraction(), fct = input$single_4, lv = 4)
      }
      else if(input$type_of_design=="5-Level Design (2-6 factors)"){
        generate_init_single(init_table = tmp_extraction(), fct = input$single_5, lv = 5)
      }
      else{
        return(NULL)
      }
  })
  
  # mixed level_a handson
  mixed_hst_a <- reactive({
      if(input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
        mx <- tmp_extraction()
        attach(mx)
        plan_mx_nosig <- plan_mx[, !colnames(plan_mx) %in% 'signal']
        plan_mx_nosig <- as.matrix(plan_mx_nosig[,1:factors_a])
        plan_mx_nosig <- t(apply(plan_mx_nosig, 2, function(x) levels(as.factor(x))))
        plan_mx_nosig <- cbind(LETTERS[1:factors_a],plan_mx_nosig)
        colnames(plan_mx_nosig) <- c("Factors" ,paste0("Level", 1:level_a))
        row.names(plan_mx_nosig) <- 1:factors_a
        plan_mx_nosig
      }
      else{
        return(NULL)
      }
  })
  
  # mixed level_b handson
  mixed_hst_b <- reactive({
    if(input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
      mx <- tmp_extraction()
      attach(mx)
      plan_mx_nosig <- plan_mx[, !colnames(plan_mx) %in% 'signal']
      plan_mx_nosig <- as.matrix(plan_mx_nosig[,(factors_a+1):(factors_a+factors_b)])
      plan_mx_nosig <- t(apply(plan_mx_nosig, 2, function(x) levels(as.factor(x))))
      # avoid "I" and "i"
      plan_mx_nosig <- cbind(c(LETTERS,letters)[!c(LETTERS,letters)%in%c("I","i")][(factors_a+1):(factors_a+factors_b)],plan_mx_nosig)
      colnames(plan_mx_nosig) <- c("Factors" ,paste0("Level", 1:level_b))
      row.names(plan_mx_nosig) <- 1:factors_b
      plan_mx_nosig
    }
    else{
      return(NULL)
    }
  })
  # signal handson
  signal_hst <- reactive({
    if(input$add_signal){
      # input$single_2 = number of factors
      x <- cbind('signal',matrix(1:input$signal_level,ncol = (input$signal_level)))
      colnames(x) <- c("Factor",paste0("Level", 1:input$signal_level))
      x
    }
  })
  
  observeEvent(input$revise_factor,{
    # if no signal factor
    if(!input$add_signal){
        if(!input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
          showModal(modalDialog(
            div("Assign Factors",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_single")
            ,easyClose = T,size = "m",footer= modalButton("OK")
            
          ))
        }
        else{
          showModal(modalDialog(
            div("Assign First Factors",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_mx_a"),
            div("Assign Second Factors",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_mx_b")
            ,easyClose = T,size = "m",footer= modalButton("OK")
          ))
        }
    }
    else{
        if(!input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
          showModal(modalDialog(
            div("Assign Factors",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_single"),
            div("Signal Factor",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_signal")
            ,easyClose = T,size = "m",footer= modalButton("OK")
          ))
        }
        else{
          showModal(modalDialog(
            div("Assign First Factors",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_mx_a"),
            div("Assign Second Factors",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_mx_b"),
            div("Signal Factor",style="text-align:center;font-weight:bold;font-size:120%;background-color:#FFF9C4"),
            rHandsontableOutput("box_signal")
            ,easyClose = T,size = "m",footer= modalButton("OK")
          ))
        }
    }
  })
  # The output$ can be called as input$, like reactive, it changes and react when called
  # single hst output
  output$box_single<- renderRHandsontable({
    hot_table(rhandsontable(single_hst()))
  })
  # mixed level_a hst output
  output$box_mx_a<- renderRHandsontable({
    hot_table(rhandsontable(mixed_hst_a()))
  })
  # mixed level_b hst output
  output$box_mx_b<- renderRHandsontable({
    hot_table(rhandsontable(mixed_hst_b()))
  })
  # signal hst output
  output$box_signal<- renderRHandsontable({
    hot_table(rhandsontable(signal_hst()))
  })
# Handsontable (tail) ----
  
# eventReactive for levels mapping (with handsontable changes) (head)----
  level_mapping <- eventReactive(input$generate_table,{
    # single level
    if(is.null(tmp_extraction())){
      return(NULL)
    }
    else{
      plan <- tmp_extraction()
      # didn't click 'Factor' button (no mapping occurs)
      if(input$revise_factor %>% as.numeric == 0){
        if(!input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
          return(plan)
        }
        else{
          return(plan$plan_mx)
        }
      }
      else{
        # has clicked the 'Factor' button
        # with signal
        if(input$add_signal){
          # After generating the initial table, hot_to_r is used here to put them into R table
          # mx with signal
          if(input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
              mx_hst_df_a <- hot_to_r(input$box_mx_a)
              mx_hst_df_b <- hot_to_r(input$box_mx_b)
              signal_df <- hot_to_r(input$box_signal)
              if(input$signal_level != (ncol(signal_df)-1)){
                return(plan$plan_mx)
              }
              # mapping func directly returns table
              # levels updates after clicking 'Factor' button (then handsontable updates)
              else{
                if(plan$level_a != (ncol(mx_hst_df_a)-1)){
                  return(plan$plan_mx)
                }
                else if(plan$level_b != (ncol(mx_hst_df_b)-1)){
                  return(plan$plan_mx)
                }
                else{
                  m_list_mx <- list(mx_hst_df_a,mx_hst_df_b,signal_df)
                  mapping(plan_table = plan$plan_mx, map_list = m_list_mx) 
                }
              }
          }
          else{
          # single with signal
              # if input$signal_level != levels, output default table (signal_df levels updates while clicking input$revise_factor)
              single_hst_df <- hot_to_r(input$box_single)
              signal_df <- hot_to_r(input$box_signal)
              if(input$signal_level != (ncol(signal_df)-1)){
                return(plan)
              }
              else{
                m_list <- list(single_hst_df,signal_df)
                mapping(plan_table = plan, map_list = m_list)
              }
          }
        }else{ # no signal
          if(input$type_of_design=="Mixed Level Design (2 to 26 factors)"){
            mx_hst_df_a <- hot_to_r(input$box_mx_a)
            mx_hst_df_b <- hot_to_r(input$box_mx_b)
            m_list_mx <- list(mx_hst_df_a, mx_hst_df_b)
            mapping(plan_table = plan$plan_mx, map_list = m_list_mx)
          }
          else{
            # single level
            single_hst_df <- hot_to_r(input$box_single)
            single_hst_df <- list(single_hst_df)
            mapping(plan_table = plan, map_list = single_hst_df)
          }
        }
      }
    }
  })
# eventReactive for levels mapping and generate table (tail)----

# show tables----
  output$taguchi_table <- renderDataTable({
    if(is.null(level_mapping()) ){
      return(NULL)
    }else{
      level_mapping()
    }
  },options = list(searching = F, pageLength = -1))
  
# download table----
  output$savetable <- downloadHandler(
    filename = function() { paste0("Taguchi Table", Sys.Date(), ".csv") },
    content = function(file) {
      if(is.null(level_mapping())){
        return(NULL)
      }else{
        write.csv(level_mapping(), file)
      }
      
    }
  )

# summary----
  summary_table <- reactive({
    if(input$type_of_design=="2-Level Design (2-31 factors)"){
      # extract name list (refer to function.r)
      design_names <- design_name(level_table = single_level_table, lv = 2, fct = input$single_2, cate = "single")
      return(list(design_names= design_names,usr_ch = as.numeric(input$single_2_aval_table),fct = input$single_2))
    }
    else if(input$type_of_design=="3-Level Design (2-13 factors)"){
      design_names <- design_name(level_table = single_level_table, lv = 3, fct = input$single_3, cate = "single")
      return(list(design_names= design_names,usr_ch = as.numeric(input$single_3_aval_table),fct = input$single_3))
    }
    else if(input$type_of_design=="4-Level Design (2-5 factors)"){
      design_names <- design_name(level_table = single_level_table, lv = 4, fct = input$single_4, cate = "single")
      return(list(design_names= design_names,usr_ch = as.numeric(input$single_4_aval_table),fct = input$single_4))
    }
    else if(input$type_of_design=="5-Level Design (2-6 factors)"){
      design_names <- design_name(level_table = single_level_table, lv = 5, fct = input$single_5, cate = "single")
      return(list(design_names= design_names,usr_ch = as.numeric(input$single_5_aval_table),fct = input$single_5))
    }
    else{
      design_names <- design_name(level_table = mixed_level_table, lv = 2, fct_sum = input$mixed_factor, cate = "mixed")
      return(list(design_names= design_names,usr_ch = as.numeric(input$mixed_aval_table),fct = input$mixed_factor))
    }
  })
  
  
  output$summary <- renderText({
    if(is.null(level_mapping())){
      NULL
    }
    else{
      if(input$add_signal){
        paste0("Taguchi Orthogonal Array Design"
               ,"\n"
               ,"Design:   ", summary_table()$design_names[summary_table()$usr_ch]
               ,"\n"
               ,"Factors:  ", summary_table()$fct
               ,"\n"
               ,"Runs:     ", nrow(level_mapping())
               ,"\n"
               ,"Signal:   ", colnames(level_mapping())[ncol(level_mapping())]
        )
      }
      else{
        paste0("Taguchi Orthogonal Array Design"
               ,"\n"
               ,"Design:   ", summary_table()$design_names[summary_table()$usr_ch]
               ,"\n"
               ,"Factors:  ", summary_table()$fct
               ,"\n"
               ,"Runs:     ", nrow(level_mapping())
        )
      }
    }
  })
  
  
  # Taguchi----
}