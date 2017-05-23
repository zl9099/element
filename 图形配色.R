install.packages("devtools")
install.packages("curl")
devtools::install_github('hadley/ggplot2')
devtools::install_github("ricardo-bion/ggtech", dependencies=TRUE)


library(ggplot2)
library(ggtech)
d <- qplot(carat, data = diamonds[diamonds$color %in%LETTERS[4:7], ], geom = "histogram", bins=30, fill = color)

d + theme_tech(theme="airbnb") +
scale_fill_tech(theme="airbnb") +
labs(title="Airbnb theme",  subtitle="now with subtitles for ggplot2 >= 2.1.0")

d + theme_airbnb_fancy() +
scale_fill_tech(theme="airbnb")  +
labs(title="Airbnb theme",
subtitle="now with subtitles for ggplot2 >= 2.1.0")


d + theme_tech(theme="etsy") +
scale_fill_tech(theme="etsy") +
labs(title="Etsy theme",
subtitle="now with subtitles for ggplot2 >= 2.1.0")


d + theme_tech(theme="facebook") +
scale_fill_tech(theme="facebook") +
labs(title="Facebook theme",
subtitle="now with subtitles for ggplot2 >= 2.1.0")


d + theme_tech(theme="google") +
scale_fill_tech(theme="google") +
labs(title="Google theme",
subtitle="now with subtitles for ggplot2 >= 2.1.0")


d + theme_tech(theme="twitter") +
scale_fill_tech(theme="twitter") +
labs(title="Twitter theme",
subtitle="now with subtitles for ggplot2 >= 2.1.0")


install.packages("gridExtra")
install.packages("ggsci")
library("ggsci")
library("gridExtra")


#图形准备：
data("diamonds")
p1 = ggplot(subset(diamonds, carat >= 2.2),

aes(x = table, y = price, colour = cut)) +
geom_point(alpha = 0.7) +
geom_smooth(method = "loess", alpha = 0.05, size = 1, span = 1) +
theme_bw()
p2 = ggplot(subset(diamonds, carat > 2.2 & depth > 55 & depth < 70),

aes(x = depth, fill = cut)) +
geom_histogram(colour = "black", binwidth = 1, position = "dodge") +
theme_bw()


#加载配色：
## Lancet
p1_lancet = p1 + scale_color_lancet()
p2_lancet = p2 + scale_fill_lancet()
grid.arrange(p1_lancet, p2_lancet, ncol = 2)
