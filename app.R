
library(tidyverse)
library(tensorflow)
library(keras)


model_file = "models/IMDB_cnn.h5"
model_dep = "IMDB_cnn_dep.RData"
emb_matrix_file = "IMDB_emb_matrix.RData"

model = load_model_tf(model_file)
load(file = model_dep)
load(file = emb_matrix_file)
##
# data = sent_train$Utterance
# 
# tokenizer = text_tokenizer(num_words = 1000)
# 
# fit_text_tokenizer(tokenizer,data)
# 
 embedding_layer = layer_embedding(input_dim=vocab_len,
                                   output_dim=vec_len,
                                   input_length = MAXLEN,
                                   weights = list(emb_matrix),
                                   trainable = FALSE)



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("
         .model_output {
            border: 1px solid black;
            border-radius: 25px;
            height: 100px;
            text-align:center;
            padding: 25px 0px;
         }   
            
        "))
    ),

    
    tags$body(
        # Application title
        titlePanel("The Sentiment AI"),
        helpText("The CNN model notices certain words you use and guesses your sentiment."),
        helpText("Scores exist between (0,1). Higher scores = positive. Lower scores = negative."),
    
        fluidRow(
            column(4,
                tags$div(class="text_input",
                    textAreaInput(inputId="user_text","Write something here:", value="Folks are usually about as happy as they make their minds up to be."),
                    actionButton("button","Classify")
                )
            ),
            column(4,
                   tags$b("Model Output"),
                   tags$div(class="model_output",
                            uiOutput("model_output")
                            )
                   )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    vector = reactive({
        sentence_to_sequence(dataset_imdb_word_index(), input$user_text) %>% 
            pad_sequences(maxlen = MAXLEN, padding="post")
    })
    
    inference_info = eventReactive(input$button,{
        val = predict(model, vector()) %>% as.numeric()
        
        if(val > 0.5){
            infer = "positive"
        }else{
            infer = "negative"
        }

        df = tibble(val = val, inference = infer)
    })
    
    output$model_output <- renderUI({
        HTML("<p>Score:",inference_info()$val,
              "<br> Prediction:",inference_info()$inference,"</p>")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
