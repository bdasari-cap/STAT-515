library(ggplot2)
library(readxl)


hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  
  #  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), linewidth=.2), #changed size to linewidth for ggplot2 3.4.0
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.20,"cm"),
  panel.spacing.y = unit(0.20,"cm"),
  
  # axis.ticks.y= element_blank()
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)




# Read the data
insurance_data <- read_excel("C:/Users/Bhargav Dasari/Documents/SPRING-2024/STAT-515/MIDTERM/MID TERM DATA.xlsx")

# Define revenue ranges and reverse their order
# Define revenue ranges
insurance_data$Revenue_Group <- cut(insurance_data$`REVENUE IN BILLION DOLLARS`,
                                    breaks = c(-Inf, 1, 2, 4, Inf), 
                                    labels = c("<1", "1-2", "2-4", ">4"),
                                    right = FALSE,
                                    ordered_result = TRUE)

# Reorder company levels by revenue in ascending order
insurance_data$COMPANY <- factor(insurance_data$COMPANY, levels = insurance_data$COMPANY[order(insurance_data$`REVENUE IN BILLION DOLLARS`)])


# Define your custom fill colors
custom_colors <- c("<1" = "#a6cee3", "1-2" = "#1f78b4", "2-4" = "#b2df8a", ">4" = "#33a02c")

ggplot(insurance_data, aes(x = `REVENUE IN BILLION DOLLARS`, y = COMPANY, fill = Revenue_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(x = "Revenue (Billion Dollars)", y = "Company", 
       title = "Top 25 Business Insurance Firms in the U.S. by Revenue Group",
       fill = "Brokerage Revenue") +  # Change legend title to "Brokerage Revenue"
  scale_fill_manual(values = custom_colors, guide = guide_legend(reverse = TRUE)) +  # Use custom colors
  theme_minimal()  + hw +
  theme(axis.title.x = element_text(margin = margin(t = 20)), 
        legend.position = "right",
        plot.title = element_text(hjust = 0.5))  # Adjust the title to be centered
