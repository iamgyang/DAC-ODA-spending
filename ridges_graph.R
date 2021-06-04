bob <- dac %>%
  filter(donorname %in% c("Australia", "Canada","France","Germany","Japan","Korea","Netherlands","Norway","United Kingdom","United States")) %>% 
  as.data.table()
bob <- bob[,.(donorname, recipientname, usd_grantequiv, GDP, Population, refugeepop.dest)]
bob[,gdppc:=GDP/Population]
bob[,totcom:=sum(usd_grantequiv,na.rm=TRUE),by=donorname]
bob[,usd_grantequiv:=usd_grantequiv/totcom]
bob <- bob[usd_grantequiv>0,]
bob[,comit:=usd_grantequiv]
invisible(lapply(names(bob),function(.name) set(bob, which(is.infinite(bob[[.name]])), j = .name,value =NA)))
bob <- na.omit(bob)
bob[,temp:=comit*gdppc]
bob[donorname=="United Kingdom", donorname:="UK"]
bob[donorname=="United States", donorname:="US"]
ordered <- as.vector(unlist(bob[,.(sum(temp)), by = donorname][order(V1)][,.(donorname)]))
bob$donorname <- factor(bob$donorname, levels = ordered)

plot1 <- ggplot(bob, aes(gdppc, weight = comit)) + 
  geom_histogram(bins=40, fill = "aquamarine4", color = "white") + 
  # theme_clean() +
  theme(legend.background = element_blank())+
  theme(plot.background = element_rect(color = "white"))+
  labs(
    x = "Recipient Country Per Capita GDP (2019 Current USD)",
    y = "Donor",
    title = "Where does DAC Official Development Assistance go?",
    caption = "Source: OECD Common Reporting Standard (CRS). All figures are grant equivalent spending."
  ) + 
  scale_fill_colorblind()+
  scale_x_continuous(labels = scales::dollar_format(), breaks = seq(0,14000,2000),
                     limits = c(0,14000)) + 
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y=element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line( size=.5, color="grey", linetype = "dotted"),
    axis.title=element_text(size=12,face="bold")
    )+
  facet_grid(donorname~., switch="both")
  

ggsave("ridges_histogram.png", plot1, dpi=1000, width = 8, height = 10)




# setDF(bob)
# bob <- bob[rep(seq_along(10000*bob$comit), 10000*bob$comit), ]
# 
# ggplot(bob[donorname=="United States"], aes(x = gdppc, y = donorname)) + 
#   ggridges::geom_density_ridges(alpha = .8, color = "grey89", weights = comit) + 
#   theme_clean() + 
#   labs(
#     x = "Recipient Country Per Capita GDP (2019 Current USD)",
#     y = "Donor Country",
#     title = "Where does DAC Official Development Assistance go?",
#     caption = "Source: OECD CRS"
#   ) +
#   scale_fill_colorblind()+
#   scale_x_continuous(labels = scales::dollar_format(), breaks = seq(0,15000,2000))+
#   coord_cartesian(clip = "off") #+ 
#   #+
#   # ggridges::theme_ridges(grid = FALSE)