library(readxl)
dat <- read_excel("总表.xlsx")

group <- c(
rep("Actinobacteria",7),
rep("Bacteroidetes",13),
rep("Firmicutes",35),
rep("Proteobacteria",5),
rep("Veillonella",2),
rep("Verrucomicrobia",2)
)

data_o <- data.frame(
  ID = glue::glue("{dat$ID}({dat$Reference})")
)

data_o$anti <- "PD-1/PD-L1"

data_o$anti[which(is.na(dat$`PD-1/PD-L1`))] <- "CTLA-4"
data_o$group <- group
data_o$res <- dat$`PD-1/PD-L1`

data_o$res[which(is.na(data_o$res))] <- dat$`CTLA-4`[which(is.na(data_o$res))]



library(ggalluvial)
library(dplyr)
library(plyr)

sankeyData <- function(data) {
  data <- data %>% 
    dplyr::select(ID,anti,res,group)
  res <- plyr::ddply(data,.(ID, anti, res, group),nrow) %>%
    to_lodes_form(
      key = "Class",
      axes = 1:2)
  return(res)
}
sankey_data <- sankeyData(data_o)

ggplot(data = sankey_data,
       aes(x = Class, 
           stratum = stratum, alluvium = alluvium)) +
  geom_flow(aes(fill = res), 
            curve_type = "xspline", show.legend = F,aes.flow = "forward",alpha = 0.7) +
  geom_stratum(aes(fill = group), show.legend = F,
               stat = "stratum",width = 1/4,color = NA)

color_group <- c(
  rep("#5c7c3c",7),
  rep("#ea4f0e",13),
  rep("#0b737b",35),
  rep("#7c3d05",5),
  rep("#078cd4",2),
  rep("#786da9",2)
)

names(color_group) <- group

color_anti_o <- sankey_data[which(sankey_data$Class == "anti"),]
color_anti_o$color_anti <- as.character(color_anti_o$Class)
color_anti_o$color_anti[which(color_anti_o$stratum == "PD-1/PD-L1")] <- "#008f43"
color_anti_o$color_anti[which(color_anti_o$stratum == "CTLA-4")] <- "#8fd9f4"

color_anti = color_anti_o$color_anti
names(color_anti) = color_anti_o$stratum

color_res = c("#0083d0","#e11e25")
names(color_res) = c("R","NR")

p <- ggplot(data = sankey_data,
       aes(x = Class, 
           stratum = stratum, alluvium = alluvium)) +
  geom_flow(aes(fill = res),
            curve_type = "xspline",aes.flow = "forward",alpha = 0.7) +
  geom_stratum(aes(fill = ifelse(as.numeric(Class) == 1, group, NA)), 
               stat = "stratum",width = 1/4,color = NA) +
  geom_stratum(aes(fill = ifelse(as.numeric(Class) == 2, as.character(stratum), NA)),
               stat = "stratum",width = 1/4,color = NA)+
  coord_flip() +
  scale_fill_manual(name = "Bacterial phyla involved:", na.translate = F,
                    values = c(color_group,color_anti,color_res), guide = "none",
                    breaks = c(names(color_group),names(color_anti),names(color_res)))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(breaks = c("ID","anti"), labels = c("ID", "Anti"),
                   expand = expansion(mult = c(0.9, 0))) +
  geom_text(aes(label = ifelse(as.numeric(Class) == 1, as.character(stratum), NA)),
            stat = "stratum", size = 3, angle = -90, hjust = 0,nudge_x = -0.125,family = "Times") +
  geom_text(aes(label = ifelse(as.numeric(Class) == 2, as.character(stratum), NA)),
            stat = "stratum", size = 3,family = "Times")+
  theme_minimal()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(linetype = "blank"),
    axis.text.x = element_text(size = 0),
    axis.text.y = element_text(family = "Times"),
    axis.title = element_text(family = "Times")
  ) + 
  labs(x = "", y = "Microbiota")

library(ggplot2)
library(cowplot)
p_l1 <- ggplot(sankey_data,aes(x=group,y=Class,fill=res)) + 
  geom_col() +
  scale_fill_manual(values = color_res,name = "Association with response:",
                    labels = c("Enriched in responders",
                               "Enriched in nonresponders")) + 
  theme(legend.title = element_text(family = "Times", size = 10),
          legend.text = element_text(family = "Times",face = "italic"))
p_l1

legend_res <- get_legend(p_l1)

p_l2 <- ggplot(sankey_data,aes(x=group,y=Class,fill=group)) + 
  geom_col() +
  scale_fill_manual(values = color_group,name = "Bacterial phyla involved:")+
  guides(fill=guide_legend(ncol=2)) + 
  theme(legend.title = element_text(family = "Times", size = 10),
        legend.text = element_text(family = "Times",face = "italic"))
p_l2

legend_group <- get_legend(p_l2)

ggdraw(plot_grid(
  plot_grid(p),
  plot_grid(legend_group, legend_res, nrow =1,
            rel_widths=c(0.5, 0.5)),
  ncol = 1,rel_heights = c(0.8,0.2))
  )

ggsave(filename = "plot.pdf",width = 8, height = 6.2, dpi = 50)
