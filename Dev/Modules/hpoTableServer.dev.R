# Define a server module for the "Tab Label"
HPOtabServer <- function(id, phenotype_data) {
  moduleServer(id, function(input, output, session) {
    
    #ns <- session$n
    ns <- shiny::NS(id)
    
    # Search by hpo id
    observeEvent(input$hpo_search, {
      new_hpo_term <- input$hpo_term
      if (new_hpo_term != "") {
        updateTextInput(session, "hpo_term", value = "")
      }
      selected_phenotype_data <- phenotype_data[hpo_id==new_hpo_term]
      output$hpo_table <- DT::renderDT({
          DT::datatable(selected_phenotype_data, filter = list(position="top",clear=TRUE), selection = "none", escape = FALSE,options = list(stateSave = FALSE,lengthMenu = list(c(25, 50, -1), c("25", "50", "All")))) 
      })
    })
    
    # Search by gene symbol
    observeEvent(input$gene_search, {
      new_gene_symbol <- input$gene_symbol
      print(new_gene_symbol)
      if (new_gene_symbol != "") {
        updateTextInput(session, "gene_symbol", value = "")
      }
      selected_phenotype_data <- phenotype_data[gene_symbol==new_gene_symbol]
      output$hpo_table <- DT::renderDT({
        DT::datatable(selected_phenotype_data, filter = list(position="top",clear=TRUE), selection = "none", escape = FALSE,options = list(stateSave = FALSE,lengthMenu = list(c(25, 50, -1), c("25", "50", "All")))) 
      })
    })
    


    
  })
}
