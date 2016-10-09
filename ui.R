library(shiny)
library(DT)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next word predictor"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      textInput("text", label=h4("Your unfinished sentence"),
                value="Hey sunshine, can you follow me and make me the"),
      submitButton("Go"),
      
      hr(),
      
      selectInput("highest", "Select highest order language model to be used:",
                  choices = c("pentagrams", "quadgrams", "trigrams", "bigrams")),
      
      hr(),
      sliderInput("guess",
                  "Selected number of predictions to be shown:",
                  min = 1,  max = 10, value = 5)
    ),
    
    # Show prediction
    mainPanel(
      tabsetPanel(
        tabPanel("Predictor",
                 DT::dataTableOutput('table')
        ),
        tabPanel("Model",
                 h3("Data source, pre-processing and compression"),
                 p("Three large text files were collected from three sources: blogs, news and Twitter. The data is from a corpus called HC Corpora (see link below)."),
                 a("HC Corpora", href="http://corpora.heliohost.org/"),
                 p("The files cleaned by removing numbers, punctuation and whitespace. From there, the text was split into a corpus each of unigrams, bigrams, trigrams, quadgrams and pentagrams."),
                 a("Exploratory Analysis"),
                 p("Due to the long tail nature of n-gram frequency, the resulting n-gram models of order greater than 2 were truncated by only keeping the n-grams with counts higher than one (in such cases, singletons represent over 70% of total number of unique n-grams)"),
                 h3("Probability and Kneser-Ney Smoothing"),
                 p("Kneser–Ney smoothing is a method primarily used to calculate the probability distribution of n-grams in a document based on their histories. It is widely considered the most effective method of smoothing due to its use of absolute discounting by subtracting a fixed value from the probability's lower order terms to omit n-grams with lower frequencies. This approach has been considered equally effective for both higher and lower order n-grams."),
                 p("A common example that illustrates the concept behind this method is the frequency of the bigram [San Francisco]. If it appears several times in a training corpus, the frequency of the unigram [Francisco] will also be high. Relying on only the unigram frequency to predict the frequencies of n-grams leads to skewed results; however, Kneser–Ney smoothing corrects this by considering the frequency of the unigram in relation to possible words preceding it."),
                 a("Language Models Tutorial", href="http://www.statmt.org/book/slides/07-language-models.pdf"),
                 p("The model actually implemented was the \"Interpolated Modified Kneser-Ney\" which is very well described in the tutorial that can be reached by the hyperlink given above."),
                 h3("Data Structures"),
                 p("A dictionary data type structure was assembled for each language model (pentagrams to unigrams) which points to appropriate files representing specific blocks of n-grams, making it possible to workaround memory and processing constraints so as to reach scalability and preserve computing speed even when working with higher order LMs such as pentagrams.")
        )
        
      )
    )
  )
)
)