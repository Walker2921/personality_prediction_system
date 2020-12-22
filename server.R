library(shiny)
library(shinythemes)
library(knitr)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(RCurl)
library(tidyverse)

library(base)
library(datasets)
library(dplyr)
library(forcats)
library(graphics)
library(grDevices)
library(methods)
library(purrr)
library(readr)
library(stats)
library(stringr)
library(tibble)
library(tidyr)
library(utils)


lr_model_opn <- read_rds(file = "lr_model_opn.rds")
lr_model_csn <- read_rds(file = "lr_model_csn.rds")
lr_model_ext <- read_rds(file = "lr_model_ext.rds")
lr_model_agr <- read_rds(file = "lr_model_agr.rds")
lr_model_neu <- read_rds(file = "lr_model_neu.rds")

shinyServer(function(input, output, session) {
    
    output$keepAlive <- renderText({
      req(input$count)
      paste("keep alive", input$count)
    })
    
    normalize <- function(x){
        x <- x/5
        x <- format(x, digits = 2, nsmall = 1)
        return (x)
    }
    
    decode <- function(x){
        for (i in seq(10)){
            if (x[i]=="SD"){
                x[i] = 1
            } else if (x[i]=="D"){
                x[i] = 2
            } else if (x[i]=="NDNA"){
                x[i] = 3
            } else if (x[i]=="A"){
                x[i] = 4
            } else if (x[i]=="SA"){
                x[i] = 5
            } else if (x[i]=="NS"){
                x[i] = 0
            }
        }
        return(x)
    }
    
    Predict_Factor_values <- reactive({
        opn_predict <- c(input$OPN1, input$OPN2, 
                         input$OPN3, input$OPN4, 
                         input$OPN5, input$OPN6, 
                         input$OPN7, input$OPN8, 
                         input$OPN9, input$OPN10)
        csn_predict <- c(input$CSN1, input$CSN2, 
                         input$CSN3, input$CSN4, 
                         input$CSN5, input$CSN6, 
                         input$CSN7, input$CSN8, 
                         input$CSN9, input$CSN10)
        ext_predict <- c(input$EXT1, input$EXT2, 
                         input$EXT3, input$EXT4, 
                         input$EXT5, input$EXT6, 
                         input$EXT7, input$EXT8, 
                         input$EXT9, input$EXT10)
        agr_predict <- c(input$AGR1, input$AGR2, 
                         input$AGR3, input$AGR4, 
                         input$AGR5, input$AGR6, 
                         input$AGR7, input$AGR8, 
                         input$AGR9, input$AGR10)
        neu_predict <- c(input$NEU1, input$NEU2, 
                         input$NEU3, input$NEU4, 
                         input$NEU5, input$NEU6, 
                         input$NEU7, input$NEU8, 
                         input$NEU9, input$NEU10)
        
        opn_predict <- as.data.frame(rbind(as.numeric(decode(opn_predict))))
        csn_predict <- as.data.frame(rbind(as.numeric(decode(csn_predict))))
        ext_predict <- as.data.frame(rbind(as.numeric(decode(ext_predict))))
        agr_predict <- as.data.frame(rbind(as.numeric(decode(agr_predict))))
        neu_predict <- as.data.frame(rbind(as.numeric(decode(neu_predict))))
        
        colnames(opn_predict) <- c("OPN1", "OPN2", "OPN3", "OPN4", "OPN5", "OPN6", "OPN7", "OPN8", "OPN9", "OPN10")
        colnames(csn_predict) <- c("CSN1", "CSN2", "CSN3", "CSN4", "CSN5", "CSN6", "CSN7", "CSN8", "CSN9", "CSN10")
        colnames(ext_predict) <- c("EXT1", "EXT2", "EXT3", "EXT4", "EXT5", "EXT6", "EXT7", "EXT8", "EXT9", "EXT10")
        colnames(agr_predict) <- c("AGR1", "AGR2", "AGR3", "AGR4", "AGR5", "AGR6", "AGR7", "AGR8", "AGR9", "AGR10")
        colnames(neu_predict) <- c("NEU1", "NEU2", "NEU3", "NEU4", "NEU5", "NEU6", "NEU7", "NEU8", "NEU9", "NEU10")
        
        opn <- predict(lr_model_opn, normalize(opn_predict))
        csn <- predict(lr_model_csn, normalize(csn_predict))
        ext <- predict(lr_model_ext, normalize(ext_predict))
        agr <- predict(lr_model_agr, normalize(agr_predict))
        neu <- predict(lr_model_neu, normalize(neu_predict))
        
        k <- c(neu, agr, ext, csn, opn)
        k <- rbind(k)
        k <- as.data.frame(k)
        colnames(k) <- c("Openess", 
                         "Conscintiousness", 
                         "Extraversion", 
                         "Agreeableness", 
                         "Neuroticism")
        return(k)
    })
    
    observeEvent(input$start_test, {
        updateTabsetPanel(session, "inTabset",
                          selected = "Test")
    })
    
    observeEvent(input$submit_test, {
        updateTabsetPanel(session, "inTabset",
                          selected = "Result")
    })
    
    output$contents <- renderPrint({
        if (input$submit_test>0){
            if ("NS" %in% c(input$OPN1, input$OPN2, input$OPN3, input$OPN4, input$OPN5, 
                            input$OPN6, input$OPN7, input$OPN8, input$OPN9, input$OPN10, 
                            input$CSN1, input$CSN2, input$CSN3, input$CSN4, input$CSN5, 
                            input$CSN6, input$CSN7, input$CSN8, input$CSN9, input$CSN10, 
                            input$EXT1, input$EXT2, input$EXT3, input$EXT4, input$EXT5, 
                            input$EXT6, input$EXT7, input$EXT8, input$EXT9, input$EXT10, 
                            input$AGR1, input$AGR2, input$AGR3, input$AGR4, input$AGR5, 
                            input$AGR6, input$AGR7, input$AGR8, input$AGR9, input$AGR10, 
                            input$NEU1, input$NEU2, input$NEU3, input$NEU4, input$NEU5, 
                            input$NEU6, input$NEU7, input$NEU8, input$NEU9, input$NEU10) ) {
                return("Please answer all the questions")
            } else {
                return("Here is your Result.")
            }
        } else {
            return("Please complete the test to see your result.")
        }
    })
    
    
    
    output$distPlot <- renderPlot({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
        } else {
            prediction <- c(0, 0, 0, 0, 0)
        }
        k <- c("NEU", "AGR", "EXT", "CSN", "OPN")
        prediction <- lapply(prediction, as.integer)
        df <- cbind(c(1, 2, 3, 4, 5), prediction)
        df <- as.data.frame(df)
        rownames(df) <- c("NEU", "AGR", "EXT", "CSN", "OPN")
        colnames(df) <- c("factors","score")
        df$score <- as.numeric(df$score)
        df$factors <- as.numeric(df$factors)
        
        ggplot(data = df, aes(x = factors, y = score)) +
            geom_bar(stat="identity",
                     fill = "steelblue",
                     color = "black",
                     width = 0.5) +
            coord_flip() +
            labs(title = "Analysis of your Personality") +
            geom_text(aes(label = score), hjust = -0.3) +
            scale_y_continuous(name = "Scores", limits = c(0, 110)) +
            scale_x_discrete(name = "Personality Factors",
                             limits = c(1, 2, 3, 4, 5),
                             labels = c("Neuroticism",
                                        "Agreeableness",
                                        "Extraversion",
                                        "Conscientiousness",
                                        "Openness")) + 
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 20, face = "bold"),
                  plot.title = element_text(size = 25, face = "bold"))
    })
    
    output$intro_openness <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[5])
            if (prediction >= 50) {
                return("Those who score high on openness to experience are perceived as creative and artistic. They prefer variety and value independence. They are curious about their surroundings and enjoy traveling and learning new things. People who are high in this trait tend to be more adventurous and creative.")
            } else {
                return("Those who score low on openness to experience prefer routine. They are uncomfortable with change and trying new things so they prefer the familiar over the unknown. As they are practical people, they often find it difficult to think creatively or abstractly.")
            }
        } else {
            return("    ")
        }
    })
    
    output$intro_conscientiousness <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[4])
            if (prediction >= 50) {
                return("Those who score high on conscientiousness can be described as organized, disciplined, detail-oriented, thoughtful, and careful. They also have good impulse control, which allows them to complete tasks and achieve goals.")
            } else {
                return("Those who score low on conscientiousness may struggle with impulse control, leading to difficulty in completing tasks and fulfilling goals. They tend to be more disorganized and may dislike too much structure. They may also engage in more impulsive and careless behavior.")
            }
        } else {
            return("    ")
        }
    })
    
    output$intro_extraversion <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[3])
            if (prediction >= 50) {
                return("Those who score high on extraversion are generally assertive, sociable, fun-loving, and outgoing. They thrive in social situations and feel comfortable voicing their opinions. They tend to gain energy and become excited from being around others.")
            } else {
                return("Those who score low in extraversion are often referred to as introverts. These people tend to be more reserved and quieter. They prefer listening to others rather than needing to be heard. Introverts often need periods of solitude in order to regain energy as attending social events can be very tiring for them. Of importance to note is that introverts do not necessarily dislike social events, but instead find them tiring.")
            }
        } else {
            return("    ")
        }
    })
    
    output$intro_agreeableness <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[2])
            if (prediction >= 50) {
                return("Those who score high in agreeableness can be described as soft-hearted, trusting, and well-liked. They are sensitive to the needs of others and are helpful and cooperative. People regard them as trustworthy and altruistic.")
            } else {
                return("Those who score low in agreeableness may be perceived as suspicious, manipulative, and uncooperative. They may be antagonistic when interacting with others, making them less likely to be well-liked and trusted.")
            }
        } else {
            return("    ")
        }
    })
    
    output$intro_neuroticism <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[1])
            if (prediction >= 50) {
                return("Those who score high on neuroticism often feel anxious, insecure and self-pitying. They are often perceived as moody and irritable. They are prone to excessive sadness and low self-esteem.")
            } else {
                return("Those who score low on neuroticism are more likely to calm, secure and self-satisfied. They are less likely to be perceived as anxious or moody. They are more likely to have high self-esteem and remain resilient.")
            }
        } else {
            return("    ")
        }
    })
    
    output$openness <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[5])
            if (prediction >= 50) {
                return(" Facets of openness include : Curious, Imaginative, Creative, Open to trying new things, Unconventional")
            } else {
                return(" Facets of openness include : Predictable, Not very imaginative, Dislikes change, Prefer routine, Traditional")
            }
        } else {
            return("This trait features characteristics such as imagination and insight. People who are high in this trait also tend to have a broad range of interests. They are curious about the world and other people and eager to learn new things and enjoy new experiences. People who are high in this trait tend to be more adventurous and creative. People low in this trait are often much more traditional and may struggle with abstract thinking.")
        }
    })
    
    output$conscientiousness <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[4])
            if (prediction >= 50) {
                return("Facets of conscientiousness include : Competence, Organized, Dutifulness, Achievement striving, Self-disciplined, Deliberation")
            } else {
                return("Facets of conscientiousness include : Incompetent, Disorganized, Careless, Procrastinates, Indiscipline, Impulsive")
            }
        } else {
            return("Standard features of this dimension include high levels of thoughtfulness, good impulse control, and goal-directed behaviors. Highly conscientious people tend to be organized and mindful of details. They plan ahead, think about how their behavior affects others, and are mindful of deadlines.")
        }
    })
    
    output$extraversion <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[3])
            if (prediction >= 50) {
                return("Facets of extraversion include : Sociable, Energized by social interaction, Excitement-seeking, Enjoys being the center of attention, Outgoing")
            } else {
                return("Facets of extraversion include : Prefers solitude, Fatigued by too much social interaction, Reflective, Dislikes being the center of attention, Reserved")
            }
        } else {
            return("Extraversion (or extroversion) is characterized by excitability, sociability, talkativeness, assertiveness, and high amounts of emotional expressiveness. People who are high in extraversion are outgoing and tend to gain energy in social situations. Being around other people helps them feel energized and excited. People who are low in extraversion (or introverted) tend to be more reserved and have less energy to expend in social settings. Social events can feel draining and introverts often require a period of solitude and quiet in order to 'recharge'.")
        }
    })
    
    output$agreeableness <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[2])
            if (prediction >= 50) {
                return("Facets of agreeableness include : Trust (forgiving), Straightforwardness, Altruism (enjoys helping), Compliance, Modesty, Sympathetic, Empathy")
            } else {
                return("Facets of agreeableness include : Sceptical, Demanding, Insults and belittles others, Stubborn, Show-off, Unsympathetic, Doesn't care about how other people feel")
            }
        } else {
            return("This personality dimension includes attributes such as trust, altruism, kindness, affection, and other prosocial behaviors. People who are high in agreeableness tend to be more cooperative while those low in this trait tend to be more competitive and sometimes even manipulative.")
        }
    })
    
    output$neuroticism <- renderText({
        if (input$submit_test>0) {
            prediction <- Predict_Factor_values()
            prediction <- as.integer(prediction[1])
            if (prediction >= 50) {
                return("Facets of neuroticism include : Anxious, Angry hostility (irritable), Experiences a lot of stress, Self-consciousness (shy), Vulnerability, Experiences dramatic shifts in mood")
            } else {
                return("Facets of neuroticism include : Doesn't worry much, Calm, Emotionally stable, Confident, Resilient, Rarely feels sad or depressed")
            }
        } else {
            return("Neuroticism is a trait characterized by sadness, moodiness, and emotional instability. Individuals who are high in this trait tend to experience mood swings, anxiety, irritability, and sadness. Those low in this trait tend to be more stable and emotionally resilient.")
        }
    })
})
