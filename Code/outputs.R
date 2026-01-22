
# old version -------------------------------------------------------------
# this was a 2-pager pdf with lots of plots stuffed neetly into it

#arrange everything on first page
gridplot1 <- arrangeGrob(c1,
                         arrangeGrob(c2, c3, c4, widths = c(1.6, 6.7, 1.7)),
                         arrangeGrob(c5, leg, c6, widths = c(4.5, 1, 4.5)),
                         heights = c(5,2,3),
                         layout_matrix = rbind(c(1), c(2), c(3)))

#arrange everything on second page
gridplot2 <- arrangeGrob(p1, p2, p3, p4, p5,
                         heights = c(4,4,1),
                         layout_matrix = rbind(c(1,2), c(3,4), c(5)))

# save both pages into one pdf for double sided A4 printout
pdf("Output/Teilnehmerstatistik.pdf", width = 29.7/2.54, height = 21/2.54)
lapply(list(gridplot1, gridplot2), grid.arrange)
dev.off()


# new version -------------------------------------------------------------
# this creates png which can be directly inserted as slides into a presentation

gridslide1 <- arrangeGrob(c1,
                          arrangeGrob(ggplot() + mytheme, c3, ggplot() + mytheme,
                                      widths = c(.87 , 6.3, 1)),
                          heights = c(5,2),
                          layout_matrix = rbind(c(1), c(2)))

gridslide2 <- arrangeGrob(arrangeGrob(arrangeGrob(ggplot() + mytheme, c2, ggplot() + mytheme,
                                                  heights = c(1 , 8, 1)), c4,
                                      widths = c(3, 7)) ,
                          p5,
                          heights = c(10,2),
                          layout_matrix = rbind(1, 2))

# including the legend on top of the other charts is a bit cumbersome
gridslide3 <- arrangeGrob(c5, c6,
                          heights = c(1, 1),
                          layout_matrix = rbind(1, 2)) |> 
  ggdraw() +
  theme(plot.background = element_rect(fill="white", color = "white")) +
  draw_grob(leg, x = 0.45)

gridslide4 <- arrangeGrob(p2,
                          heights = 1,
                          layout_matrix = rbind(c(1)))

gridslide5 <- arrangeGrob(p1,
                          heights = 1,
                          layout_matrix = rbind(c(1)))

gridslide6 <- arrangeGrob(p3,
                          heights = 1,
                          layout_matrix = rbind(c(1)))

gridslide7 <- arrangeGrob(p4,
                          heights = 1,
                          layout_matrix = rbind(c(1)))

gridslide8 <- arrangeGrob(jub2,
                          heights = 1,
                          layout_matrix = rbind(c(1)))

gridslide9 <- arrangeGrob(jub1,
                          heights = 1,
                          layout_matrix = rbind(c(1)))


# save slides as png's for presentation
for (i in 1:9) {
  ggsave(paste0("Output/Slides", i, ".png"),
         get(paste0("gridslide", i)),
         device = png(),
         width = 16, height = 9)
}
# this one directly, because arrangeGrob does not work with ggbreak
ggsave(paste0("Output/Slides10.png"),
       jub4,
       device = png(),
       width = 16, height = 9)

