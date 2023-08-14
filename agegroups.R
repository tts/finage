library(tidyverse)
library(pxweb)
library(patchwork)

url <- "https://pxdata.stat.fi:443/PxWeb/api/v1/fi/Maahanmuuttajat_ja_kotoutuminen/Maahanmuuttajat_ja_kotoutuminen/maakoto_pxt_11vw.px"

# You need to delete the outer element that is automatically included in the saved JSON file
pxq <- pxweb_query("pop.json")
pxd <- pxweb_get(url, pxq)
pxdf2 <- as.data.frame(pxd, column.name.type = "text", variable.value.type = "text")

calc_p <- function(d, sub) {
  
  data <- d %>% 
    select(-`Syntymävaltio`, -Sukupuoli) %>%
    mutate(`Ikä` = str_replace(`Ikä`, "-", "")) %>%
    rename(Lkm = `Väestö 31.12.`) %>% 
    mutate_all(as.numeric) 
  
  data_g <- data %>%
    group_by(Vuosi) %>%
    summarise(
      `<10` = sum(
        Lkm[`Ikä` >= 0 & `Ikä` < 10]),
      `10-19` = sum(
        Lkm[`Ikä` >= 10 & `Ikä` < 20]),
      `20-29` = sum(
        Lkm[`Ikä` >= 20 & `Ikä` < 30]),
      `30-39` = sum(
        Lkm[`Ikä` >= 30 & `Ikä` < 40]),
      `40-49` = sum(
        Lkm[`Ikä` >= 40 & `Ikä` < 50]),
      `50-59` = sum(
        Lkm[`Ikä` >= 50 & `Ikä` < 60]),
      `60-69` = sum(
        Lkm[`Ikä` >= 60 & `Ikä` < 70]),
      `70-79` = sum(
        Lkm[`Ikä` >= 70 & `Ikä` < 80]),
      `80>` = sum(
        Lkm[`Ikä` >= 80]),
      .groups = "drop")
  
  data_g_long <- data_g %>% 
    pivot_longer(
      cols = `<10`:`80>`,
      names_to = "Ikäryhmä",
      values_to = "Lkm"
    ) %>% 
    mutate(Vuosi = as.numeric(Vuosi))
  
  data_n <- data_g_long %>% 
    group_by(Vuosi, `Ikäryhmä`) %>% 
    summarise(n = sum(Lkm)) %>% 
    mutate(Prosenttia = n / sum(n))
  
  p <- ggplot(data_n, aes(x = Vuosi, y = Prosenttia, fill = `Ikäryhmä`)) +
    geom_area(alpha = 0.8, size = .5, colour = "white") +
    scale_fill_viridis_d(direction = -1) + 
    theme_minimal() +
    labs(subtitle = sub,
         x = NULL, y = NULL)
  
  return(p)
}

# Born in Finland
p1 <- pxdf2 %>% 
  filter(`Syntymävaltio` == "Suomi") %>% 
  calc_p(., "Suomessa syntyneet")

# Born elsewhere
p2 <- pxdf2 %>% 
  filter(`Syntymävaltio` != "Suomi") %>% 
  calc_p(., "Suomen ulkopuolella syntyneet")

# All together
p3 <- pxdf2 %>% 
  calc_p(., "Kaikki syntyneet")

p <- p1 + p2 + p3 + 
  plot_annotation('Eri ikäryhmien suhteellinen osuus', 
                  caption = "Tilastokeskus: Väestö syntymävaltion, iän ja sukupuolen mukaan, 1990-2022 | @ttso",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(ncol = 2, byrow = TRUE, guides = 'collect')
p
