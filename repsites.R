Reppts_moll<-spTransform(Reppts,"+proj=moll")
xy_rep<-data.frame(coordinates(Reppts_moll),label=Reppts_moll$label)
colnames(xy_rep)<-c("x","y","label")
library(ggrepel)
inset_rep <- ggplot() + 
            geom_tile(data = dplyr::filter(mask_glkd, !is.na(land)), aes(x = x, y = y), fill = "cornsilk2",col="cornsilk") +  
            geom_point(data = xy_rep, aes(x = x, y = y), fill = "gray") + 
            geom_text_repel(data = xy_rep, aes(x = x, y = y, label = label),
                  box.padding = 0.5,     # Distance around text bounding box
                  point.padding = 0.3,   # Distance away from the point
                  segment.color = "grey50") + # Line connecting text to point if moved far
            xlab("") + ylab("") + theme(legend.position = "none", legend.box.background = element_blank(), legend.background = element_blank()) + 
            theme(panel.background = element_rect(fill = 'transparent'), plot.background = element_rect(fill = 'transparent', color = NA),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.background = element_rect(fill='transparent'), 
            legend.box.background = element_rect(fill = 'transparent'), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), 
            axis.text.y = element_blank(), axis.text.x = element_blank(), panel.border = element_rect(colour = "grey80", fill = NA, size = 0.5), 
            plot.title = element_text(size = 8))

