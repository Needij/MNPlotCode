diffdatalongpols <- read.csv("MN_Figure_4_data.csv")
diffdatalongpols$catpolitical <- factor(diffdatalongpols$catpolitical, levels = c("Liberals", "Moderates", "Conservatives"))
diffdatalongpols$Difficulty_forgraphs <- factor(diffdatalongpols$Difficulty_forgraphs, levels = c("Easy", "Intermediate", "Difficult"))
diffdatalongpols$PConTypeLabeled <- factor(diffdatalongpols$PConTypeLabeled, levels = c("Apolitical Items", "Liberal Confirming Items", "Conservative Confirming Items"))

library(ggplot2)
#Plot probability correct logistic regression lines on problems based on:
#problem type, difficulty condition, a person's political ideology (created categories), and person's numeracy
Figure4 <-ggplot(data = diffdatalongpols, aes(x = Numeracy, y = ProblemCorr, color =PConTypeLabeled))+
  geom_smooth(method = "glm", method.args = list(family= "binomial"), aes(linetype=PConTypeLabeled, fill = PConTypeLabeled), size = 1, alpha = .25)+
  scale_y_continuous(limits = c(0,1),
                     labels= c(seq (0,1, .2)), 
                     breaks= c(seq (0,1, .2)))+
  scale_x_continuous(labels= c(seq (0,8, 1)), 
                     breaks= c(seq (0,8, 1)))+
  facet_grid(Difficulty_forgraphs~catpolitical)+
  scale_colour_manual(values=c("gray66","gray50", "gray25"))+
  scale_fill_manual(values=c("gray66","gray50", "gray25"))+
  scale_linetype_manual(values = c("solid", "dashed", "twodash"))+
  ylab('Probability Correct')+
  ggtitle("Correctness by Political Ideology, Problem Difficulty, and Numeracy")+
  theme_bw()+
  theme (plot.title = element_text(hjust = 0.5))+
  theme (text = element_text(size = 24, family = "serif"), 
         legend.title = element_blank(),
         legend.key.size = unit(2, "lines"))
Figure4

#Figure 4, except lines are colored according to problem type political ideology####
Figure4col <- Figure4 + scale_colour_manual(values=c("green3","blue3", "red3"))
Figure4col
