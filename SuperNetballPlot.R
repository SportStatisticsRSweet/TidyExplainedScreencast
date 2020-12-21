
# Load packages
library(xlsx)
library(tidyverse)
library(ggdark)
library(ggtext)
library(cowplot)

# Call out super netball colours - that are CVD friendly
SSN_Colours <- c("#FDE725FF", "#73D055FF", "#27AD81FF", "#7E4E90FF", "#CC6A70FF", "#2D708EFF", "#C0C0C0", "#F68F46FF")
names(SSN_Colours) <- c("Lightning", "Fever", "Vixens", "Firebirds", "Thunderbirds", "Swifts", "Magpies", "Giants")

# Load in raw data
RollingSubs <- read.csv("Subs_Time.csv")
Scoring <- read.csv("Scoring_Time.csv")
# Download team lists - thanks to Aaron Fox!
URL <- 'https://raw.githubusercontent.com/aaronsfox/super-netball-analysis/master/Data/SuperNetball2020/squadLists.csv'
# Read in the csv
SquadLists <- read.csv(URL)
# Rename headers
colnames(SquadLists) <- c("Player", "Team")
# Now lets left join
Scoring <- left_join(Scoring, SquadLists, by=c("Player"))

# Allocate shot value
Scoring <- Scoring %>%
  mutate(Score = case_when(Event == "MISS" ~ 0,
                           Event == "GOAL" ~ 1,
                           Event == "2PT GOAL" ~ 2,
                           TRUE ~ 0)) 
# Calculate margins
Scoring$Margin = with(Scoring, cumsum(ifelse(Team == "GIANTS", Score, -Score)))
# Make the timestamp column a numeric - clunky (and maybe funky) but allows for numeric x-axis
Scoring$TimeStamp <-  as.numeric(format(as.POSIXct(Scoring$Time, format = "%M:%OS"), "%M.%S"))
# Add cumulative time
Scoring <- Scoring %>%
  mutate(MatchTime = case_when(Quarter == 2 ~ TimeStamp + 15,
                               Quarter == 3 ~ TimeStamp + 30,
                               Quarter == 4 ~ TimeStamp + 45,
                               TRUE ~ TimeStamp)) 

# Work on rolling subs data
# Firstly, eliminate any changes frpm the bench at start of the quarter
RollingSubs <- RollingSubs %>% 
  filter(From=="S" & Time!="0:00") 
# Make time a numeric column again
RollingSubs$TimeStamp <-  as.numeric(format(as.POSIXct(RollingSubs$Time, format = "%M:%OS"), "%M.%S"))
# Add cumulative time
RollingSubs <- RollingSubs %>%
  mutate(MatchTime = case_when(Quarter == "Q2" ~ TimeStamp + 15,
                               Quarter == "Q3" ~ TimeStamp + 30,
                               Quarter == "Q4" ~ TimeStamp + 45,
                               TRUE ~ TimeStamp)) 

# Create a data.frame for supershot
SuperShot <- data.frame(TimeStart = c(10, 25, 40, 55), 
                        TimeEnd = c(15, 30, 45, 60))

# Basic plot
ggplot() +
  geom_rect(data = SuperShot, aes(xmin = TimeStart, xmax = TimeEnd,
                                  ymin = -8, ymax = 8), fill = "lightpink", alpha = 0.25) + 
  geom_step(data = Scoring, aes(x = MatchTime, y = Margin), 
            colour = "black", size = 0.5) +
  geom_vline(data = RollingSubs, aes(xintercept = MatchTime, colour = Team), size = 0.75) +
  geom_point(data = RollingSubs, aes(x = MatchTime, y = 7.5, colour = Team), size = 6) +
  geom_text(data = RollingSubs, aes(x = MatchTime, y = 7.5, label = To), size = 2, colour = "black") +
  scale_colour_manual(values = SSN_Colours, na.translate=FALSE) 

# Make it fancy!
# First, add a column for the super shot
Scoring <- Scoring %>%
mutate(SuperShot = case_when(TimeStamp >= 10 ~ "Yes",
                             TimeStamp < 600 ~ "No")) 

# Call out super netball colours - that are CVD friendly (with super shot)
SSN_Colours <- c("#FDE725FF", "#73D055FF", "#27AD81FF", "#7E4E90FF", "#CC6A70FF", "#2D708EFF", "#C0C0C0", "#F68F46FF", "#ff0067", 	"#FFFFFF")
names(SSN_Colours) <- c("Lightning", "Fever", "Vixens", "Firebirds", "Thunderbirds", "Swifts", "Magpies", "Giants", "Yes", "No")

# Plot score!
ggplot() +
  geom_step(data = Scoring, aes(x = MatchTime, y = Margin, colour = SuperShot, group = 1), size = 0.75) +
  scale_x_continuous(expand = c(0,0), breaks = c(15, 30, 45, 60), limits = c(0, 62),
                     labels = paste0("Q", c("1", "2", "3", "4"), " end")) +
  scale_y_continuous(expand = c(0, 0), limits = c(-7, 7), breaks = seq(-6, 6, by = 2), 
                     labels =  c(6, 4, 2, 0, 2, 4, 6)) +
  geom_hline(aes(yintercept = 0), alpha = 0.5, colour = "grey") +
  geom_point(data = RollingSubs, aes(x = MatchTime, y = 6, colour = Team), size = 6) +
  geom_text(data = RollingSubs, aes(x = MatchTime, y = 6, label = To), size = 2, colour = "black") +
  scale_colour_manual(values = SSN_Colours, na.translate=FALSE) +
  scale_fill_manual(values = SSN_Colours, na.translate=FALSE) +
  labs(x = "\n Time (mins)", y = "Margin \n",
       title = "Suncorp Super Netball 2020 Rd11 between <span style = 'color:#F68F46FF;'>Giants</span> and 
       <span style = 'color:#C0C0C0;'>Magpies. </span><br> <span style = 'font-size:12pt'>Margin incl. 
       <span style = 'color:#ff0067;'>SuperShot</span><br>",
       caption = "\n \n ") +
  dark_theme_classic() +
  theme(plot.title = ggtext::element_textbox_simple(size = 12, face = "bold",halign=0.5,
                      lineheight = 1.75,padding = margin(5, 5, 0, 5),margin = margin(0, 0, 0, 0)),
        plot.background = element_rect(fill = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.line.x =  element_blank(),
        axis.line.y =  element_line(colour = "grey"),
        axis.text.x = element_text(size = 10, face = "bold", colour = "grey"),
        axis.ticks.y =  element_line(colour = "grey"),
        axis.text.y = element_text(size = 8, colour = "grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold", colour = "grey"),
        legend.title = element_blank(),
        legend.position = "none") 

# Change rolling subs y direction
RollingSubs <- RollingSubs %>%
  mutate(YLocation = case_when(Team == "Giants" ~ 6,
                               Team == "Magpies" ~ -6)) 
# Happier Plot
ScorePlot <- ggplot() +
  geom_step(data = Scoring, aes(x = MatchTime, y = Margin, colour = SuperShot, group = 1), size = 0.75) +
  scale_x_continuous(expand = c(0,0), breaks = c(15, 30, 45, 60), limits = c(0, 62),
                     labels = paste0("Q", c("1", "2", "3", "4"), " end")) +
  scale_y_continuous(expand = c(0, 0), limits = c(-7, 7), breaks = seq(-6, 6, by = 2), 
                     labels =  c(6, 4, 2, 0, 2, 4, 6)) +
  geom_hline(aes(yintercept = 0), alpha = 0.5, colour = "grey") +
  geom_point(data = RollingSubs, aes(x = MatchTime, y = YLocation, colour = Team), size = 6) +
  geom_text(data = RollingSubs, aes(x = MatchTime, y = YLocation, label = To), size = 2, colour = "black") +
  geom_segment(data = RollingSubs, aes(x = MatchTime, y = 0, xend = MatchTime, yend = YLocation, colour = Team), 
               linetype = "dashed") +
  scale_colour_manual(values = SSN_Colours, na.translate=FALSE) +
  scale_fill_manual(values = SSN_Colours, na.translate=FALSE) +
  labs(x = "\n Time (mins)", y = "Margin \n",
       title = "Suncorp Super Netball 2020 Rd11 between <span style = 'color:#F68F46FF;'>Giants</span> and 
       <span style = 'color:#C0C0C0;'>Magpies. </span><br> <span style = 'font-size:12pt'>Margin incl. 
       <span style = 'color:#ff0067;'>SuperShot</span>and rolling substitutions. <br>",
       caption = "\n \n ") +
  dark_theme_classic() +
  theme(plot.title = ggtext::element_textbox_simple(size = 12, face = "bold",halign=0.5,
                                                    lineheight = 1.75,padding = margin(5, 5, 0, 5),margin = margin(0, 0, 0, 0)),
        plot.background = element_rect(fill = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        axis.line.x =  element_blank(),
        axis.line.y =  element_line(colour = "grey"),
        axis.text.x = element_text(size = 10, face = "bold", colour = "grey"),
        axis.ticks.y =  element_line(colour = "grey"),
        axis.text.y = element_text(size = 8, colour = "grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold", colour = "grey"),
        legend.title = element_blank(),
        legend.position = "none") 

# Plot logos
Giants <- "Giants.png"
Magpies <- "Magpies.png"
  
# Plot
ggdraw() +
  draw_plot(ScorePlot) +
  draw_image(Giants, x = -0.46, y = 0.42, scale = .11) +
  draw_image(Magpies, x = -0.46, y = -0.45, scale = .11) 


  



