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
    scale_y_continuous(sec.axis = dup_axis(),
                       breaks = seq(0, 100, by = 10)) + 
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(subtitle = sub) 
  
  return(p)
}

p1 <- pxdf2 %>% 
  filter(`Syntymävaltio` == "Suomi") %>% 
  dodata(., "Suomessa syntyneet")

p2 <- pxdf2 %>% 
  filter(`Syntymävaltio` != "Suomi") %>% 
  dodata(., "Muualla kuin Suomessa syntyneet")

p3 <- pxdf2 %>% 
  mutate(`Ikä` = str_replace(`Ikä`, "-", "")) %>% 
  group_by(Vuosi, `Ikä`) %>% 
  mutate(Kaikki = sum(`Väestö 31.12.`)) %>% 
  select(-`Väestö 31.12.`) %>% 
  rename(`Väestö 31.12.` = Kaikki) %>% 
  ungroup() %>% 
  dodata(., "Kaikki")

p <- p2 + p1 + p3 +
  plot_annotation(title = 'Ikäryhmien koko vuosina 1990-2022', 
                  subtitle = 'Vaaka-akselilla tarkasteluvuosi, pystyakselilla ikä',
                  caption = "Tilastokeskus: Väestö syntymävaltion, iän ja sukupuolen mukaan, 1990-2022 | https://github.com/tts/finage") +
  plot_layout(ncol = 1, byrow = TRUE)

ggsave("finage.png", width = 20, height = 20, units = "cm")
