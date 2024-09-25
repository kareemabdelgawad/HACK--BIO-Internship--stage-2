#used library
library(dplyr)
library(ggplot2)
library(plotly)
library(ggsci)

# to Download data
AMR_Products<- read.delim("https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/WHO_AMR_PRODUCTS_DATA.tsv", sep = "\t", header = TRUE)
#preprocessing
AMR_Products[is.na(AMR_Products)]<-'unknown'

write.table(AMR_Products,"AMR_Product.tsv")
#Identify Key Trends:


#1- distribution of Product Type
Product_type_summary<-AMR_Products%>%
  filter(!duplicated(AMR_Products$Product.name))%>%
  group_by(Product.type)%>%
  summarise(count=n())
  
ggplot(Product_type_summary, aes(x = Product.type, y = count, fill =Product.type)) +
  geom_bar(stat = 'identity',width = 0.3,position = 'dodge') +
  scale_fill_manual(values = c("#4F81BD", "#C0504D"))  +
  theme_minimal()+xlab('')+
  theme(text = element_text(size = 9),
        plot.title = element_text(hjust =0.5,face = 'bold' ))

#

#-----------------
#Create the  bar chart..
#..that shows distribution of Product Type with activity status
product_activity <- AMR_Products %>%
  filter(Active.against.priority.pathogens. !='N/A')%>%
  group_by(Product.type, Active.against.priority.pathogens.) %>%
  summarise(Count = n(), .groups = 'drop')

# Create faceted bar plot
ggplot(product_activity, aes(x = Product.type, y = Count, fill = Active.against.priority.pathogens.)) +
  geom_bar(stat = "identity",width = 0.4) +
  facet_wrap(~ Active.against.priority.pathogens.) +
  scale_fill_manual(values = c("Yes" = "#BC0CFF", "No" = "#008080","Possibly"="#8B0000")) +
  labs(
       x = "Product Type",
       y = "Count",
       fill = "Activity Status") +
  xlab('')
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 4),
        axis.title.x  = element_text(size = 2),
        axis.text = element_text(size = 4))

  #....................................
  
  
  #The relation between Rout of addministraition and drug activity
  # Summarize the data
  Route_Activity_relation <- AMR_Products %>%
    filter(Active.against.priority.pathogens. !='N/A')%>%
    filter(Route.of.administration !='N/A')%>%
    group_by(Route.of.administration, Active.against.priority.pathogens.) %>%
    summarise(Count = n(), .groups = 'drop')
  
  # Create the dot plot with ggplot
  ggplot_plot <- ggplot(Route_Activity_relation, aes(x = Route.of.administration, y = Count, fill = Active.against.priority.pathogens.)) +
    geom_point(size = 3) +
    scale_fill_manual(values = c("Yes" = "green", "No" = "red", "Possibly" = "orange")) +
    labs(title = "Relationship Between Route of Administration and Drug Effectiveness",
         x = "Route of Administration",
         y = "Number of Products",
         color = "Effectiveness Against Priority Pathogens") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 8),
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8))
  
  # 
  interactive_plot <- ggplotly(ggplot_plot)
  # Display the interactive plot
  interactive_plot
  #uploud the plot online
  htmlwidgets::saveWidget(interactive_plot,'interactive_polt.html')
  Sys.setenv("plotly_username"="Abdullah988")
  Sys.setenv("plotly_api_key"="r4cNVAaAC8iReKbMxLDS")
  api_create(interactive_plot, filename = "plotly")
#________________________________________________________________________
  # Summarize the data to count occurrences of each combination
data <- AMR_Products %>%
    group_by(Product.name, Antibacterial.class, R.D.phase,Pathogen.name,Active.against.priority.pathogens.) %>%
  filter(Active.against.priority.pathogens.!='N/A')%>%
  filter(Active.against.priority.pathogens.!='Possibly')%>%
  
    summarise(Count = n(), .groups = 'drop')
  
 

# Create the bubble chart
bubble_plot <- plot_ly(
  data = data,
  x = ~R.D.phase,
  y = ~Antibacterial.class,
  size = I(10),  # Constant size for all bubbles
  color = ~Active.against.priority.pathogens.,  # Color by activity against priority pathogens
    # Color by activity against priority pathogens
  colors = c("Yes" = "blue", "No" = "red"),
   text = ~paste("Product Name:", Product.name, 
                "<br>Pathogen Name:", Pathogen.name, 
                "<br>Active Against Priority Pathogens:", Active.against.priority.pathogens.),
  hoverinfo = 'text',
  type = 'scatter',
  mode = 'markers'
) %>%
  layout(
    xaxis = list(title = "R&D Phase"),
    yaxis = list(title = "Antibacterial Class"),
    title = "Bubble Plot of Antibacterial Products",
    showlegend = TRUE
  )

# Display the interactive bubble plot
bubble_plot
#upload the plot online
Sys.setenv("plotly_username"="Abdullah988")
Sys.setenv("plotly_api_key"="r4cNVAaAC8iReKbMxLDS")
api_create(bubble_plot, filename = "bubbplot")
#_____________________________________________________________________
#non traditional data categories
Non.traditional=AMR_Products%>%filter(Product.type=="Non-traditional")
Non.traditional
colnames(Non.traditional)

ggplot(Non.traditional, aes(x = "", y = Non.traditionals.categories, 
                            fill = factor(Non.traditionals.categories))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_discrete(name = "Non.traditionals.categories") +
  labs(title = "Pie Chart of Non.traditionals.categories") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",legend.title.position = "top")
#Which category has effect on which pathigens

# Create a simple dot plot 
ggplot(Non.traditional, aes(x = Non.traditionals.categories, y = Pathogen.name)) +
  geom_point(aes(color = Pathogen.name), size = 3, position = position_jitter(width = 0.2, height = 0.2)) +
  labs(title = "Effect of Products on Pathogens",
       x = "Non.traditionals.categories",
       y = "Pathogen Name",
       color = "Pathogen Name") +
  theme(legend.position = "bottom")





############################################################################
#========================== create Bacteriophages_and_phage_derived_enzymes_data
Bacteriophages_and_phage_derived_enzymes_data <- AMR_Products %>%
  filter(Non.traditionals.categories == "Bacteriophages and phage-derived enzymes")

Bacteriophages_and_phage_derived_enzymes_data

#what is the route of adminstrtion
ggplot(Bacteriophages_and_phage_derived_enzymes_data,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what is R.D phase of this proucts
ggplot(Bacteriophages_and_phage_derived_enzymes_data,mapping = aes(R.D.phase,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))





#######################################################################################################
#========================== create Immunomodulating_agents_activity_data
Immunomodulating_agents_data <- AMR_Products %>% 
  filter(Non.traditionals.categories == "Immunomodulating agents")

# what products used in Immunomodulating_agents_activity
ggplot(Immunomodulating_agents_data,mapping = aes(Antibacterial.class,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))


#what is R.D phase of this proucts
ggplot(Immunomodulating_agents_data,mapping = aes(R.D.phase,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what is the route of adminstrtion
ggplot(Immunomodulating_agents_data,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))


#Application on different pathogens
Immunomodulating_agents <- Non.traditional %>%
  filter(Product.name %in% c("Rhu-pGSN", "AB103"))
Immunomodulating_agents

# Create a dot plot
ggplot(Immunomodulating_agents, aes(x = Product.name, y = Pathogen.name)) +
  geom_point(aes(color = Pathogen.name), size = 3, position = position_jitter(width = 0.2, height = 0.2)) +
  labs(title = "Effect of Rhu-pGSN and AB103 on Pathogens",
       x = "Product Name",
       y = "Pathogen Name",
       color = "Pathogen Name") +
  theme(
    text = element_text(size = 16),  # Set base font size for all text
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title.x = element_text(size = 18),               
    axis.title.y = element_text(size = 18),               
    axis.text.x = element_text(size = 14, angle = 25, hjust = 1),  
    axis.text.y = element_text(size = 14),                 
    legend.text = element_text(size = 14),                 
    legend.title = element_text(size = 16),                
    legend.position = "bottom"                             
  )





#############################################################################
Microbiome_modulating_agents <- AMR_Products %>% filter(Non.traditionals.categories == "Microbiome-modulating agents")
# what is the anti bacterial class that have been used in Microbiome_modulating_agents
ggplot(Microbiome_modulating_agents,mapping = aes(Antibacterial.class,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))


#what is R.D phase of this proucts

#what pathogens had been tried aganist products
ggplot(Microbiome_modulating_agents,mapping = aes(Pathogen.name,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))


#what is the route of adminstrtion
ggplot(Microbiome_modulating_agents,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))



############################################################################
#========================== create Miscellaneous_data
Miscellaneous_data <- AMR_Products %>% 
  filter(Non.traditionals.categories == "Miscellaneous")
# what is the anti bacterial class that have been used in Miscellaneous
ggplot(Miscellaneous_data,mapping = aes(Antibacterial.class,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))+coord_flip()

#what is R.D phase of this proucts
ggplot(Miscellaneous_data,mapping = aes(R.D.phase,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
  )
#what is the route of adminstrtion
ggplot(Miscellaneous_data,mapping = aes(Route.of.administration,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
  )

#--------------------------------------------------------------
#========================== create antibodies_data
antibodies_data <- AMR_Products %>%
  filter(Non.traditionals.categories == "Antibodies")
# what is the anti bacterial class that have been used in Antibodies
ggplot(antibodies_data,mapping = aes(Antibacterial.class,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))+coord_flip()

#what is R.D phase of this products
ggplot(antibodies_data,mapping = aes(R.D.phase,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what pathogens had been tried aganist products
ggplot(antibodies_data,mapping = aes(Pathogen.name,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
  )
#what is the route of adminstrtion
ggplot(antibodies_data,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#products of antibodies aganist pathogens.
#--------------------------------------------------------------
#loading_data
antibodies_activity_data <- AMR_Products %>% 
  filter(Non.traditionals.categories == "Antibodies")
#visualize data
ggplot(antibodies_activity_data,mapping = aes(Active.against.priority.pathogens.,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))
#_________________________________________________________________________
##The code(s) for deep analysis:
# Distribution of Product Type
Product_type_summary<-AMR_Products%>%
  group_by(Product.type)%>%
  filter(!duplicated(AMR_Products$Product.name))%>%
  summarise(count=n())
Product_type_summary

ggplot(Product_type_summary, aes(x = Product.type, y = count, fill =Product.type)) +
  geom_bar(stat = 'identity',width = 0.3,position = 'dodge') +
  scale_fill_manual(values = c("#4F81BD", "#C0504D")) +
  theme_minimal()+xlab('')+
  theme(text = element_text(size = 9),
        plot.title = element_text(hjust =0.5,face = 'bold' ),legend.position = "bottom")

#Test the product activity
product_activity<-AMR_Products%>%
  group_by(Product.type, Active.against.priority.pathogens.)%>%
  summarise(Count=n())
product_activity

# Create the stacked bar chart that shows distribution of Product Type with activity status
ggplot(product_activity, aes(x = Product.type, y = Count, fill = Active.against.priority.pathogens.)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c("Yes" = "#FF5733", "No" = "grey", "Possibly" = "#3357FF")) +
  labs(
    x = "Product Type",
    y = "Count",
    fill = "Activity Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Summarize the data
Route_Activity_relation <- AMR_Products %>%
  group_by(Route.of.administration, Active.against.priority.pathogens.) %>%
  summarise(Count = n(), .groups = 'drop')
Route_Activity_relation

# relation between Route od administration and activity status
ggplot_plot <- ggplot(Route_Activity_relation, aes(x = Route.of.administration, y = Count, color = Active.against.priority.pathogens.)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Yes" = "#FF5733", "No" = "grey", "Possibly" = "#3357FF")) +
  labs(title = "Relationship Between Route of Administration and Drug Effectiveness",
       x = "Route of Administration",
       y = "Number of Products",
       color = "Effectiveness Against Priority Pathogens") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title.position = "bottom")

ggplot_plot

#================================Antibiotics data
Antibiotics <- AMR_Products %>% filter(Product.type == "Antibiotics")

#what is the R.D phase of antibiotic products
ggplot(Antibiotics,mapping = aes(R.D.phase,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(50))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.ticks.y = element_blank())

#A:The vast majority of new antibiotic products present in phase 1

#what is the route of adminstrtion of the products
ggplot(Antibiotics,mapping = aes(Route.of.administration,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(46))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
  )

#what pathogens have been tried aganist Antibiotic products
ggplot(Antibiotics,mapping = aes(Pathogen.name,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(46))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)+coord_flip()
  )

# Activity status of the antibotics aganist pathogens
ggplot(Antibiotics,mapping = aes(Active.against.priority.pathogens.,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(50))

################################################################################
#non traditional data categories
Non.traditional=AMR_Products%>%filter(Product.type=="Non-traditional")
Non.traditional
colnames(Non.traditional)

ggplot(Non.traditional, aes(x = "", y = Non.traditionals.categories, fill = factor(Non.traditionals.categories))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_discrete(name = "Non.traditionals.categories") +
  labs(title = "Pie Chart of Non.traditionals.categories") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom",legend.title.position = "top")

#Which category has effect on which pathigens

# Create a simple dot plot
ggplot(Non.traditional, aes(x = Non.traditionals.categories, y = Pathogen.name)) +
  geom_point(aes(color = Pathogen.name), size = 3, position = position_jitter(width = 0.2, height = 0.2)) +
  labs(title = "Effect of Products on Pathogens",
       x = "Non.traditionals.categories",
       y = "Pathogen Name",
       color = "Pathogen Name") +
  theme(legend.position = "bottom")

############################################################################
#========================== create Bacteriophages_and_phage_derived_enzymes_data
Bacteriophages_and_phage_derived_enzymes_data <- AMR_Products %>%
  filter(Non.traditionals.categories == "Bacteriophages and phage-derived enzymes")

Bacteriophages_and_phage_derived_enzymes_data

#what is the route of adminstrtion
ggplot(Bacteriophages_and_phage_derived_enzymes_data,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what is R.D phase of this proucts
ggplot(Bacteriophages_and_phage_derived_enzymes_data,mapping = aes(R.D.phase,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#######################################################################################################
#========================== create Immunomodulating_agents_activity_data
Immunomodulating_agents_data <- AMR_Products %>% filter(Non.traditionals.categories == "Immunomodulating agents")

# what products used in Immunomodulating_agents_activity
ggplot(Immunomodulating_agents_data,mapping = aes(Antibacterial.class,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what is R.D phase of this proucts
ggplot(Immunomodulating_agents_data,mapping = aes(R.D.phase,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what is the route of adminstrtion
ggplot(Immunomodulating_agents_data,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#Application on different pathogens
Immunomodulating_agents <- Non.traditional %>%
  filter(Product.name %in% c("Rhu-pGSN", "AB103"))
Immunomodulating_agents

# Create a dot plot
ggplot(Immunomodulating_agents, aes(x = Product.name, y = Pathogen.name)) +
  geom_point(aes(color = Pathogen.name), size = 3, position = position_jitter(width = 0.2, height = 0.2)) +
  labs(title = "Effect of Rhu-pGSN and AB103 on Pathogens",
       x = "Product Name",
       y = "Pathogen Name",
       color = "Pathogen Name") +
  theme(
    text = element_text(size = 16), # Set base font size for all text
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14, angle = 25, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.position = "bottom"
  )

#############################################################################
Microbiome_modulating_agents <- AMR_Products %>% filter(Non.traditionals.categories == "Microbiome-modulating agents")
# what is the anti bacterial class that have been used in Microbiome_modulating_agents
ggplot(Microbiome_modulating_agents,mapping = aes(Antibacterial.class,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what is R.D phase of this proucts

#what pathogens had been tried aganist products
ggplot(Microbiome_modulating_agents,mapping = aes(Pathogen.name,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what is the route of adminstrtion
ggplot(Microbiome_modulating_agents,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

############################################################################
#========================== create Miscellaneous_data
Miscellaneous_data <- AMR_Products %>% filter(Non.traditionals.categories == "Miscellaneous")
# what is the anti bacterial class that have been used in Miscellaneous
ggplot(Miscellaneous_data,mapping = aes(Antibacterial.class,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))+coord_flip()

#what is R.D phase of this proucts
ggplot(Miscellaneous_data,mapping = aes(R.D.phase,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
  )
#what is the route of adminstrtion
ggplot(Miscellaneous_data,mapping = aes(Route.of.administration,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
  )

#--------------------------------------------------------------
#========================== create antibodies_data
antibodies_data <- AMR_Products %>% filter(Non.traditionals.categories == "Antibodies")
# what is the anti bacterial class that have been used in Antibodies
ggplot(antibodies_data,mapping = aes(Antibacterial.class,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))+coord_flip()

#what is R.D phase of this products
ggplot(antibodies_data,mapping = aes(R.D.phase,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#what pathogens had been tried aganist products
ggplot(antibodies_data,mapping = aes(Pathogen.name,fill=Product.name))+geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10)
  )
#what is the route of adminstrtion
ggplot(antibodies_data,mapping = aes(Route.of.administration,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))

#products of antibodies aganist pathogens.
#--------------------------------------------------------------
#loading_data
antibodies_activity_data <- AMR_Products %>% filter(Non.traditionals.categories == "Antibodies")
#visualize data
ggplot(antibodies_activity_data,mapping = aes(Active.against.priority.pathogens.,fill=Product.name))+
  geom_bar()+theme_light()+scale_fill_manual(values = pal_igv()(20))


