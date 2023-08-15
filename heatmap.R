library(tidyverse)
library(pxweb)
library(patchwork)

url <- "https://pxdata.stat.fi:443/PxWeb/api/v1/fi/Maahanmuuttajat_ja_kotoutuminen/Maahanmuuttajat_ja_kotoutuminen/maakoto_pxt_11vw.px"

# You need to delete the outer element that is automatically included in the saved JSON file
pxq <- pxweb_query("pop.json")
pxd <- pxweb_get(url, pxq)
pxdf2 <- as.data.frame(pxd, column.name.type = "text", variable.value.type = "text")

dodata <- function(d, sub) {
  thisd <- d %>% 
    select(-Sukupuoli, -`Syntymävaltio`) %>% 
    rename(Lkm = `Väestö 31.12.`) %>% 
    mutate(`Ikä` = str_replace(`Ikä`, "-", "")) %>% 
    mutate_all(as.numeric)
  
  p <- ggplot(data = thisd, 
              mapping = aes(x = Vuosi, y = `Ikä`, fill = Lkm)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +
    theme_minimal() +
    labs(subtitle = sub, x = NULL) 
  
  return(p)
}

p1 <- pxdf2 %>% 
  filter(`Syntymävaltio` == "Suomi") %>% 
  dodata(., "Suomessa syntyneet")

p2 <- pxdf2 %>% 
  filter(`Syntymävaltio` != "Suomi") %>% 
  dodata(., "Muualla kuin Suomessa syntyneet")

p <- p1 + p2 + 
  plot_annotation('Ikäryhmien koko vuosina 1990-2022', 
                  caption = "Tilastokeskus: Väestö syntymävaltion, iän ja sukupuolen mukaan, 1990-2022 | https://github.com/tts/finage",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(ncol = 1, byrow = TRUE)

ggsave("finage.png", width = 20, height = 20, units = "cm")
