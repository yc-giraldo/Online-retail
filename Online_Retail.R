
# PROYECTO FINAL - ANALÍTICA DE LOS NEGOCIOS
# Comportamiento de compras en una tienda online
# Base de datos: Online Retail - UCI

# 1. Instalar paquetes necesarios
paquetes <- c("readxl", "dplyr", "ggplot2", "lubridate")

faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

# 2. Cargar librerias
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# 3. Importar base de datos
datos <- read_excel("Online Retail.xlsx")

# 4. Revisar la estructura
dim(datos)
names(datos)
str(datos)
summary(datos)

# 5. Revision de calidad de los datos

revision_datos <- datos %>%
  summarise(
    total_registros = n(),
    cantidad_negativa = sum(Quantity < 0, na.rm = TRUE),
    cantidad_cero = sum(Quantity == 0, na.rm = TRUE),
    precio_negativo = sum(UnitPrice < 0, na.rm = TRUE),
    precio_cero = sum(UnitPrice == 0, na.rm = TRUE),
    clientes_sin_id = sum(is.na(CustomerID)),
    productos_sin_descripcion = sum(is.na(Description)),
    facturas_canceladas = sum(
      grepl("^C", InvoiceNo, ignore.case = TRUE),
      na.rm = TRUE
    )
  )

print(revision_datos)

# 6. Preparacion de las transacciones validas

ventas_limpias <- datos %>%
  filter(
    !is.na(InvoiceDate),
    !is.na(InvoiceNo),
    !grepl("^C", InvoiceNo, ignore.case = TRUE),
    !is.na(Quantity),
    Quantity > 0,
    !is.na(UnitPrice),
    UnitPrice > 0
  ) %>%
  mutate(
    fecha = as.Date(InvoiceDate),
    ventas = Quantity * UnitPrice,
    semana = floor_date(fecha, unit = "week",
                        week_start = 1)
  )

# 7. Agrupar ventas por semana

ventas_semanales <- ventas_limpias %>%
  group_by(semana) %>%
  summarise(
    ventas_totales = sum(ventas),
    unidades_vendidas = sum(Quantity),
    transacciones = n_distinct(InvoiceNo),
    productos_diferentes = n_distinct(StockCode),
    .groups = "drop"
  ) %>%
  arrange(semana)

# 8. Revisar resultado

print(ventas_semanales, n = 10)
cat("Numero de semanas:", nrow(ventas_semanales), "\n")

# 9. Excluir semanas incompletas

fecha_inicio <- min(as.Date(datos$InvoiceDate), na.rm = TRUE)
fecha_fin <- max(as.Date(datos$InvoiceDate), na.rm = TRUE)

primera_semana_completa <- lubridate::ceiling_date(
  fecha_inicio,
  unit = "week",
  week_start = 1
)

ultima_semana_completa <- lubridate::floor_date(
  fecha_fin,
  unit = "week",
  week_start = 1
) - lubridate::days(7)

ventas_semanales_completas <- ventas_semanales %>%
  filter(
    semana >= primera_semana_completa,
    semana <= ultima_semana_completa
  )

cat("Semanas originales:", nrow(ventas_semanales), "\n")
cat("Semanas completas:", nrow(ventas_semanales_completas), "\n")

# 10. Guardar grafica final corregida

# Reconstruir la grafica con las semanas completas
grafica_ventas_final <- ggplot(
  ventas_semanales_completas,
  aes(x = semana, y = ventas_totales)
) +
  geom_line(color = "#D94F86", linewidth = 1.1) +
  geom_point(color = "#A82D63", size = 1.8) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      prefix = "£"
    )
  ) +
  labs(
    title = "Evolucion de las ventas semanales",
    subtitle = "Tienda online - Diciembre 2010 a noviembre 2011",
    x = "Semana",
    y = "Ventas brutas (£)",
    caption = "Fuente: Elaboracion propia con datos de Online Retail (UCI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Guardar la grafica con otro nombre
ggsave(
  "resultados/ventas_semanales_final.png",
  plot = grafica_ventas_final,
  device = "png",
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Comprobar las observaciones
cat("Semanas en la grafica:",
    nrow(ventas_semanales_completas), "\n")

# 11. Construccion de variables para el analisis

base_modelo <- ventas_semanales_completas %>%
  arrange(semana) %>%
  mutate(
    numero_semana = row_number(),
    mes = month(semana),
    trimestre = quarter(semana),
    ventas_semana_anterior = lag(ventas_totales)
  )

# Revisar variables creadas
print(
  base_modelo %>%
    select(
      semana,
      ventas_totales,
      numero_semana,
      mes,
      trimestre,
      ventas_semana_anterior
    ),
  n = 10
)

# Revisar estructura
str(base_modelo)

# 12. Construccion de variables comerciales

# Identificar ventas internacionales
ventas_comerciales <- ventas_limpias %>%
  mutate(
    internacional = Country != "United Kingdom"
  )

# Agrupar indicadores comerciales por semana
indicadores_comerciales <- ventas_comerciales %>%
  group_by(semana) %>%
  summarise(
    ventas_internacionales = sum(
      ventas[internacional], na.rm = TRUE
    ),
    facturas_internacionales = n_distinct(
      InvoiceNo[internacional]
    ),
    .groups = "drop"
  )

# Incorporar las variables a la base del modelo
base_modelo <- base_modelo %>%
  left_join(indicadores_comerciales, by = "semana") %>%
  mutate(
    porcentaje_ventas_internacionales =
      100 * ventas_internacionales / ventas_totales,
    porcentaje_facturas_internacionales =
      100 * facturas_internacionales / transacciones
  )

# Revisar resultados
print(
  base_modelo %>%
    select(
      semana,
      ventas_totales,
      porcentaje_ventas_internacionales,
      porcentaje_facturas_internacionales
    ),
  n = 10
)

# Comprobar estructura
str(base_modelo)

# 13. Exploracion de productos para clasificacion

productos_principales <- ventas_limpias %>%
  filter(!is.na(Description)) %>%
  group_by(StockCode, Description) %>%
  summarise(
    ventas_totales = sum(ventas, na.rm = TRUE),
    unidades_vendidas = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas_totales))

# Mostrar los 30 productos con mayores ventas
print(productos_principales, n = 30)

# Contar productos diferentes
cat(
  "Productos diferentes:",
  n_distinct(productos_principales$StockCode),
  "\n"
)

# 14. Revision de productos y cargos administrativos

revision_cargos <- ventas_limpias %>%
  filter(
    StockCode %in% c(
      "DOT", "POST", "M", "BANK CHARGES",
      "C2", "CRUK", "D"
    )
  ) %>%
  group_by(StockCode, Description) %>%
  summarise(
    registros = n(),
    ventas_totales = sum(ventas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas_totales))

print(revision_cargos, n = Inf)

# Participacion de estos codigos en las ventas brutas
participacion_cargos <- 100 *
  sum(revision_cargos$ventas_totales) /
  sum(ventas_limpias$ventas)

cat(
  "Porcentaje de ventas asociado a estos codigos:",
  round(participacion_cargos, 2), "%\n"
)

# 15. Preparacion de productos para clasificacion

# Identificar codigos que no corresponden claramente
# a productos comerciales

codigos_excluir <- c(
  "DOT", "POST", "M", "BANK CHARGES",
  "C2", "CRUK", "D"
)

# Crear base exclusiva para analizar categorias
productos_clasificacion <- ventas_limpias %>%
  filter(semana %in% ventas_semanales_completas$semana,
    !StockCode %in% codigos_excluir,
    !is.na(Description),
    trimws(Description) != ""
  ) %>%
  mutate(
    descripcion_limpia = toupper(trimws(Description))
  )

# Comprobar cantidad de registros y productos
cat(
  "Registros para clasificacion:",
  nrow(productos_clasificacion), "\n"
)

cat(
  "Codigos de productos diferentes:",
  n_distinct(productos_clasificacion$StockCode),
  "\n"
)

# Revisar las 50 descripciones con mayores ventas
top_descripciones <- productos_clasificacion %>%
  group_by(descripcion_limpia) %>%
  summarise(
    ventas_totales = sum(ventas),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas_totales))

print(top_descripciones, n = 50)


# 16. Clasificacion de productos - version revisada

productos_clasificacion <- productos_clasificacion %>%
  mutate(
    categoria = case_when(

      grepl(
        paste(
          "CHRISTMAS|XMAS|PARTY|BUNTING",
          "PAPER CHAIN|EASTER|HALLOWEEN",
          "FESTIVE|GARLAND|BAUBLE",
          sep = "|"
        ),
        descripcion_limpia
      ) ~ "Fiestas y temporada",

      grepl(
        paste(
          "BAG|SHOPPER|PURSE|WALLET",
          "HANDBAG|TOTE|PARASOL|UMBRELLA",
          sep = "|"
        ),
        descripcion_limpia
      ) ~ "Bolsos y accesorios",

      grepl(
        paste(
          "CAKE|MUG|CUP|SAUCER|TEAPOT",
          "JAR|KITCHEN|PANTRY|LUNCH BOX",
          "BOWL|PLATE|PICNIC|RECIPE",
          "JAM MAKING|HOT WATER BOTTLE",
          "BAKING|SNACK BOX|CUTLERY",
          "BREAD BIN|TEA TOWEL|POPCORN",
          "STORAGE|TEA SET",
          sep = "|"
        ),
        descripcion_limpia
      ) ~ "Cocina y hogar",

      grepl(
        paste(
          "LIGHT|LAMP|CANDLE|T-LIGHT",
          "LANTERN|ORNAMENT|FRAME",
          "DOORMAT|WICKER|CLOCK|SIGN",
          "COAT RACK|CABINET|CHEST",
          "HOME BUILDING BLOCK|TRINKET",
          sep = "|"
        ),
        descripcion_limpia
      ) ~ "Decoracion e iluminacion",

      grepl(
        paste(
          "PAPER|NOTEBOOK|PENCIL|PEN ",
          "CARD|CHALKBOARD|CRAFT",
          "STICKER|WRAP|BLACK BOARD",
          "MEMOBOARD|PAINT SET|RIBBON",
          sep = "|"
        ),
        descripcion_limpia
      ) ~ "Papeleria y manualidades",

      TRUE ~ "Otros productos"
    )
  )

# Comprobar registros por categoria
print(
  productos_clasificacion %>%
    count(categoria, sort = TRUE)
)
# Revisar categorias creadas
cat(
  "Numero de categorias:",
  n_distinct(productos_clasificacion$categoria),
  "\n"
)

# 17. Revision de productos clasificados como Otros

otros_principales <- productos_clasificacion %>%
  filter(categoria == "Otros productos") %>%
  group_by(descripcion_limpia) %>%
  summarise(
    ventas_totales = sum(ventas, na.rm = TRUE),
    registros = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(ventas_totales))

# Mostrar los 35 productos de Otros con mayores ventas
print(otros_principales, n = 35)

# Calcular participacion de Otros en el valor de ventas
participacion_otros <- productos_clasificacion %>%
  summarise(
    porcentaje_ventas_otros = 100 *
      sum(ventas[categoria == "Otros productos"]) /
      sum(ventas)
  )

print(participacion_otros)

# 18. Tabla de ventas por categoria

tabla_categorias <- productos_clasificacion %>%
  group_by(categoria) %>%
  summarise(
    ventas_totales = sum(ventas, na.rm = TRUE),
    unidades_vendidas = sum(Quantity, na.rm = TRUE),
    registros = n(),
    .groups = "drop"
  ) %>%
  mutate(
    participacion_ventas = 100 *
      ventas_totales / sum(ventas_totales)
  ) %>%
  arrange(desc(ventas_totales))

print(tabla_categorias, n = Inf)

# Verificar que los porcentajes sumen 100
cat(
  "Total participacion:",
  sum(tabla_categorias$participacion_ventas),
  "%\n"
)

# 19. Grafica de ventas por categoria

grafica_categorias <- ggplot(
  tabla_categorias,
  aes(
    x = reorder(categoria, ventas_totales),
    y = ventas_totales
  )
) +
  geom_col(fill = "#D94F86", width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ",",
      prefix = "£"
    ),
    expand = expansion(mult = c(0, 0.12))
  ) +
  geom_text(
    aes(
      label = paste0(
        round(participacion_ventas, 1), "%"
      )
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  labs(
    title = "Ventas brutas por categoria de productos",
    subtitle = "Tienda online - 51 semanas de analisis",
    x = "Categoria",
    y = "Ventas brutas (£)",
    caption = paste(
      "Fuente: Elaboracion propia con datos de Online Retail (UCI).",
      "Clasificacion aproximada por palabras clave."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(color = "gray50")
  )

# Guardar la grafica
dir.create("resultados", showWarnings = FALSE)

ggsave(
  filename = "resultados/ventas_por_categoria.png",
  plot = grafica_categorias,
  device = "png",
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("Grafica de categorias guardada correctamente.\n")

# 20. Analisis de ventas por pais

# Utilizar las mismas 51 semanas del analisis principal
ventas_paises <- ventas_limpias %>%
  filter(
    semana %in% ventas_semanales_completas$semana
  ) %>%
  group_by(Country) %>%
  summarise(
    ventas_totales = sum(ventas, na.rm = TRUE),
    unidades_vendidas = sum(Quantity, na.rm = TRUE),
    facturas = n_distinct(InvoiceNo),
    .groups = "drop"
  ) %>%
  mutate(
    participacion_ventas = 100 *
      ventas_totales / sum(ventas_totales)
  ) %>%
  arrange(desc(ventas_totales))

# Mostrar todos los paises
print(ventas_paises, n = Inf, width = Inf)

# Verificar cantidad de paises
cat(
  "Numero de paises:",
  nrow(ventas_paises), "\n"
)

# Verificar participacion total
cat(
  "Participacion total:",
  sum(ventas_paises$participacion_ventas),
  "%\n"
)

# 21. Grafica de ventas por principales mercados

# Seleccionar los cinco principales mercados fuera de Reino Unido
principales_internacionales <- ventas_paises %>%
  filter(Country != "United Kingdom") %>%
  slice_max(
    order_by = ventas_totales,
    n = 5,
    with_ties = FALSE
  ) %>%
  pull(Country)

# Agrupar los demas mercados
ventas_mercados_grafica <- ventas_paises %>%
  mutate(
    mercado = case_when(
      Country == "United Kingdom" ~ "Reino Unido",
      Country %in% principales_internacionales ~ Country,
      TRUE ~ "Otros mercados"
    )
  ) %>%
  group_by(mercado) %>%
  summarise(
    ventas_totales = sum(ventas_totales),
    .groups = "drop"
  ) %>%
  mutate(
    participacion = 100 * ventas_totales /
      sum(ventas_totales)
  ) %>%
  arrange(desc(ventas_totales))

print(ventas_mercados_grafica, n = Inf)

# Elaborar grafica
grafica_paises <- ggplot(
  ventas_mercados_grafica,
  aes(
    x = reorder(mercado, ventas_totales),
    y = ventas_totales
  )
) +
  geom_col(fill = "#D94F86", width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "£",
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0, 0.12))
  ) +
  geom_text(
    aes(label = paste0(round(participacion, 1), "%")),
    hjust = -0.15,
    size = 3.5
  ) +
  labs(
    title = "Distribucion de ventas brutas por mercado",
    subtitle = "Tienda online - 51 semanas de analisis",
    x = "Mercado",
    y = "Ventas brutas (£)",
    caption = "Fuente: Elaboracion propia con datos de Online Retail (UCI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "resultados/ventas_por_pais.png",
  plot = grafica_paises,
  device = "png",
  width = 11,
  height = 6,
  dpi = 300,
  bg = "white"
)

# 22. Incorporacion de Google Trends

# Importar datos de busquedas de Home decor
tendencias <- read.csv(
  "multiTimeline.csv",
  skip = 1,
  check.names = FALSE
)

# Organizar fechas y valores
tendencias <- tendencias %>%
  rename(
    fecha_google = Week,
    interes_home_decor = 2
  ) %>%
  mutate(
    fecha_google = as.Date(fecha_google),
    semana = fecha_google + days(1),
    interes_home_decor = as.numeric(interes_home_decor)
  ) %>%
  select(semana, interes_home_decor)

# Incorporar a la base semanal
base_modelo <- base_modelo %>%
  left_join(tendencias, by = "semana")

# Revisar resultados
print(
  base_modelo %>%
    select(
      semana,
      ventas_totales,
      interes_home_decor
    ),
  n = 10
)

# Comprobar datos faltantes
cat(
  "Semanas sin datos de Google Trends:",
  sum(is.na(base_modelo$interes_home_decor)),
  "\n"
)

# 23. Relacion entre ventas e interes de busqueda

# Calcular correlacion de Pearson
correlacion_home_decor <- cor(
  base_modelo$ventas_totales,
  base_modelo$interes_home_decor,
  use = "complete.obs",
  method = "pearson"
)

cat(
  "Correlacion entre ventas y Home decor:",
  round(correlacion_home_decor, 3),
  "\n"
)
# 24. Grafica de ventas e interes por Home decor

grafica_home_decor <- ggplot(
  base_modelo,
  aes(x = interes_home_decor, y = ventas_totales)
) +
  geom_point(color = "#D94F86", size = 2.5) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#A82D63") +
  labs(
    title = "Ventas semanales e interes por Home decor",
    x = "Indice de interes de Google Trends (0-100)",
    y = "Ventas semanales (£)"
  ) +
  theme_minimal()

# Guardar grafica de Home decor

grDevices::png(
  filename = "resultados/ventas_home_decor.png",
  width = 1200,
  height = 720,
  units = "px",
  res = 120,
  type = "windows"
)

par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1), new = FALSE)

print(grafica_home_decor, newpage = TRUE)

grDevices::dev.off()

# 25. Incorporacion de desempleo en Reino Unido

# Importar datos oficiales de ONS
desempleo_original <- read.csv(
  "desempleo_uk.csv",
  skip = 8,
  header = FALSE,
  col.names = c("periodo", "tasa_desempleo"),
  stringsAsFactors = FALSE
)

# Seleccionar los datos mensuales del periodo
meses_ingles <- c(
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
)

desempleo_mensual <- desempleo_original %>%
  filter(
    grepl("^2010 [A-Z]{3}$|^2011 [A-Z]{3}$", periodo)
  ) %>%
  mutate(
    anio = as.integer(substr(periodo, 1, 4)),
    mes = match(substr(periodo, 6, 8), meses_ingles),
    tasa_desempleo = as.numeric(tasa_desempleo)
  ) %>%
  filter(!is.na(mes)) %>%
  select(anio, mes, tasa_desempleo)

# Asociar cada semana con el desempleo de su mes
base_modelo <- base_modelo %>%
  mutate(anio = lubridate::year(semana)) %>%
  left_join(
    desempleo_mensual,
    by = c("anio", "mes")
  )

# Revisar la incorporacion
print(
  base_modelo %>%
    select(semana, ventas_totales, tasa_desempleo),
  n = 12
)

cat(
  "Semanas sin dato de desempleo:",
  sum(is.na(base_modelo$tasa_desempleo)),
  "\n"
)

# 26. Relacion entre desempleo y ventas semanales

correlacion_desempleo <- cor(
  base_modelo$ventas_totales,
  base_modelo$tasa_desempleo,
  use = "complete.obs",
  method = "pearson"
)

cat(
  "Correlacion entre desempleo y ventas:",
  round(correlacion_desempleo, 3),
  "\n"
)

# Revisar valores de desempleo
summary(base_modelo$tasa_desempleo)

# 27. Evolucion de la tasa de desempleo en Reino Unido

grafica_desempleo <- ggplot(
  base_modelo,
  aes(x = semana, y = tasa_desempleo)
) +
  geom_line(
    color = "#D94F86",
    linewidth = 1.1
  ) +
  geom_point(
    color = "#A82D63",
    size = 1.8
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Evolucion de la tasa de desempleo en Reino Unido",
    subtitle = "Diciembre 2010 a noviembre 2011",
    x = "Semana",
    y = "Tasa de desempleo (%)",
    caption = "Fuente: Elaboracion propia con datos de ONS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )


# Guardar grafica de desempleo

grDevices::png(
  filename = "resultados/evolucion_desempleo.png",
  width = 1200,
  height = 720,
  units = "px",
  res = 120,
  type = "windows"
)

# Restablecer el area de dibujo
par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1), new = FALSE)

print(grafica_desempleo, newpage = TRUE)

grDevices::dev.off()

# 28. Analisis de ventas por trimestre

# Agrupar las ventas por anio y trimestre
ventas_trimestrales <- base_modelo %>%
  mutate(
    anio_trimestre = paste0(
      lubridate::year(semana),
      "-T",
      lubridate::quarter(semana)
    )
  ) %>%
  group_by(anio_trimestre) %>%
  summarise(
    semanas = n(),
    ventas_totales_trimestre = sum(ventas_totales),
    promedio_ventas_semanales = mean(ventas_totales),
    .groups = "drop"
  )

# Mostrar tabla
print(ventas_trimestrales)

# 29. Grafica de ventas promedio semanales por trimestre

grafica_trimestres <- ggplot(
  ventas_trimestrales,
  aes(
    x = anio_trimestre,
    y = promedio_ventas_semanales
  )
) +
  geom_col(
    fill = "#D94F86",
    width = 0.65
  ) +
  geom_text(
    aes(
      label = scales::label_number(
        prefix = "£",
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 1
      )(promedio_ventas_semanales)
    ),
    vjust = -0.5,
    size = 3.8
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "£",
      big.mark = ".",
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Ventas promedio semanales por trimestre",
    subtitle = "Tienda online - 51 semanas completas",
    x = "Trimestre",
    y = "Ventas promedio semanales (£)",
    caption = "Fuente: Elaboracion propia con datos de Online Retail (UCI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

# Guardar grafica
grDevices::png(
  filename = "resultados/ventas_por_trimestre.png",
  width = 1200,
  height = 720,
  units = "px",
  res = 120,
  type = "windows"
)

par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1), new = FALSE)

print(grafica_trimestres, newpage = TRUE)

grDevices::dev.off()

# 30. Relacion entre ventas y variedad de productos

# Calcular correlacion de Pearson
correlacion_productos <- cor(
  base_modelo$ventas_totales,
  base_modelo$productos_diferentes,
  use = "complete.obs",
  method = "pearson"
)

cat(
  "Correlacion entre ventas y variedad de productos:",
  round(correlacion_productos, 3),
  "\n"
)

# Revisar estadisticas descriptivas
summary(
  base_modelo %>%
    select(ventas_totales, productos_diferentes)
)
# 31. Grafica de ventas vs variedad de productos

grafica_variedad <- ggplot(
  base_modelo,
  aes(
    x = productos_diferentes,
    y = ventas_totales
  )
) +
  geom_point(
    color = "#D94F86",
    size = 2.5,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#A82D63"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "£",
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Ventas semanales y variedad de productos vendidos",
    subtitle = "Tienda online - 51 semanas completas",
    x = "Numero de productos diferentes vendidos",
    y = "Ventas brutas semanales (£)",
    caption = "Fuente: Elaboracion propia con datos de Online Retail (UCI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Guardar grafica
grDevices::png(
  filename = "resultados/ventas_variedad_productos.png",
  width = 1200,
  height = 720,
  units = "px",
  res = 120,
  type = "windows"
)

par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1), new = FALSE)

print(grafica_variedad, newpage = TRUE)

grDevices::dev.off()

# 32. Tabla de correlaciones con las ventas semanales

variables_correlacion <- base_modelo %>%
  select(
    ventas_totales,
    unidades_vendidas,
    transacciones,
    productos_diferentes,
    ventas_semana_anterior,
    porcentaje_ventas_internacionales,
    interes_home_decor,
    tasa_desempleo
  )

matriz_correlaciones <- cor(
  variables_correlacion,
  use = "pairwise.complete.obs",
  method = "pearson"
)


tabla_correlaciones_ventas <- data.frame(
  variable = rownames(matriz_correlaciones),
  correlacion_con_ventas = matriz_correlaciones[, "ventas_totales"]
) %>%
  filter(variable != "ventas_totales") %>%
  arrange(desc(abs(correlacion_con_ventas)))

tabla_correlaciones_ventas$correlacion_con_ventas <-
  round(tabla_correlaciones_ventas$correlacion_con_ventas, 3)

print(tabla_correlaciones_ventas)
write.csv(
  tabla_correlaciones_ventas,
  "resultados/tabla_correlaciones_ventas.csv",
  row.names = FALSE
)
print(tabla_correlaciones_ventas)

# 33. Mapa de calor de correlaciones

# Convertir matriz de correlaciones a formato largo
datos_mapa <- as.data.frame(
  as.table(matriz_correlaciones)
)

colnames(datos_mapa) <- c(
  "Variable_1",
  "Variable_2",
  "Correlacion"
)

# Crear grafica
grafica_correlaciones <- ggplot(
  datos_mapa,
  aes(
    x = Variable_1,
    y = Variable_2,
    fill = Correlacion
  )
) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    aes(label = sprintf("%.2f", Correlacion)),
    size = 3.5
  ) +
  scale_fill_gradient2(
    low = "#A82D63",
    mid = "white",
    high = "#D94F86",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlacion"
  ) +
  coord_equal() +
  labs(
    title = "Matriz de correlaciones de variables comerciales",
    subtitle = "Tienda online - datos semanales",
    x = NULL,
    y = NULL,
    caption = "Fuente: Elaboracion propia con datos de UCI, Google Trends y ONS"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Guardar grafica
grDevices::png(
  filename = "resultados/mapa_correlaciones.png",
  width = 1500,
  height = 1100,
  units = "px",
  res = 120,
  type = "windows"
)

par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1), new = FALSE)

print(grafica_correlaciones, newpage = TRUE)

grDevices::dev.off()

# 34. Verificacion de observaciones en correlaciones

nrow(base_modelo)

sum(complete.cases(variables_correlacion))

colSums(is.na(variables_correlacion))

# 35. Relacion entre ventas semanales y participacion internacional

# Calcular correlacion de Pearson
correlacion_internacional <- cor(
  base_modelo$ventas_totales,
  base_modelo$porcentaje_ventas_internacionales,
  use = "complete.obs",
  method = "pearson"
)

cat(
  "Correlacion entre ventas y participacion internacional:",
  round(correlacion_internacional, 3),
  "\n"
)

# Revisar las variables utilizadas
summary(
  base_modelo %>%
    select(
      ventas_totales,
      porcentaje_ventas_internacionales
    )
)

# 36. Grafica de ventas vs participacion internacional

grafica_internacional <- ggplot(
  base_modelo,
  aes(
    x = porcentaje_ventas_internacionales,
    y = ventas_totales
  )
) +
  geom_point(
    color = "#D94F86",
    size = 2.5,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#A82D63"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "£",
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Ventas semanales y participacion internacional",
    subtitle = "Tienda online - 51 semanas completas",
    x = "Participacion de ventas internacionales (%)",
    y = "Ventas brutas semanales (£)",
    caption = "Fuente: Elaboracion propia con datos de Online Retail (UCI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Guardar grafica
grDevices::png(
  filename = "resultados/ventas_participacion_internacional.png",
  width = 1200,
  height = 720,
  units = "px",
  res = 120,
  type = "windows"
)

par(mfrow = c(1, 1))
par(fig = c(0, 1, 0, 1), new = FALSE)

print(grafica_internacional, newpage = TRUE)

grDevices::dev.off()

# 37. Tabla descriptiva de participacion internacional

# Calcular estadisticas de las 51 semanas
resumen_internacional <- base_modelo %>%
  summarise(
    semanas = n(),
    promedio = mean(
      porcentaje_ventas_internacionales, na.rm = TRUE
    ),
    mediana = median(
      porcentaje_ventas_internacionales, na.rm = TRUE
    ),
    minimo = min(
      porcentaje_ventas_internacionales, na.rm = TRUE
    ),
    maximo = max(
      porcentaje_ventas_internacionales, na.rm = TRUE
    ),
    desviacion = sd(
      porcentaje_ventas_internacionales, na.rm = TRUE
    )
  )

# Crear tabla con valores en porcentaje
formato_porcentaje <- function(x) {
  paste0(
    formatC(x, format = "f", digits = 2, decimal.mark = ","),
    " %"
  )
}

tabla_internacional <- data.frame(
  indicador = c(
    "Semanas analizadas",
    "Participacion promedio",
    "Participacion mediana",
    "Participacion minima",
    "Participacion maxima",
    "Desviacion estandar"
  ),
  valor = c(
    as.character(resumen_internacional$semanas),
    formato_porcentaje(resumen_internacional$promedio),
    formato_porcentaje(resumen_internacional$mediana),
    formato_porcentaje(resumen_internacional$minimo),
    formato_porcentaje(resumen_internacional$maximo),
    formato_porcentaje(resumen_internacional$desviacion)
  )
)

print(tabla_internacional, row.names = FALSE)

# Guardar tabla en CSV
write.csv(
  tabla_internacional,
  "resultados/tabla_participacion_internacional.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Preparar tabla para exportar como imagen
tabla_internacional$fila <- 6:1

grafica_tabla_internacional <- ggplot(
  tabla_internacional
) +
  geom_tile(
    aes(x = 5, y = fila),
    width = 10,
    height = 0.85,
    fill = "#FCEBF2"
  ) +
  geom_text(
    aes(x = 0.3, y = fila, label = indicador),
    hjust = 0,
    size = 4.5
  ) +
  geom_text(
    aes(x = 9.6, y = fila, label = valor),
    hjust = 1,
    size = 4.5,
    fontface = "bold",
    color = "#A82D63"
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.5, 6.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Resumen de la participacion internacional",
    subtitle = "Tienda online - 51 semanas completas",
    caption = "Fuente: Elaboracion propia con datos de Online Retail (UCI)"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(color = "gray50")
  )

# Guardar tabla como PNG
ggsave(
  filename = "resultados/tabla_participacion_internacional.png",
  plot = grafica_tabla_internacional,
  width = 9,
  height = 4.5,
  dpi = 300,
  bg = "white"
)

# 38. Primera regresion multivariada de ventas semanales

# Seleccionar variables del primer modelo
datos_regresion_1 <- base_modelo %>%
  select(
    semana,
    ventas_totales,
    productos_diferentes,
    interes_home_decor,
    tasa_desempleo,
    numero_semana
  ) %>%
  filter(complete.cases(.))

# Verificar observaciones disponibles
cat(
  "Observaciones del primer modelo:",
  nrow(datos_regresion_1),
  "\n"
)

# Estimar regresion lineal multiple
modelo_1 <- lm(
  ventas_totales ~
    productos_diferentes +
    interes_home_decor +
    tasa_desempleo +
    numero_semana,
  data = datos_regresion_1
)

# Mostrar resultados del modelo
summary(modelo_1)

# 39. Tabla de coeficientes beta del primer modelo

# Extraer coeficientes de la regresion
coeficientes_modelo_1 <- summary(modelo_1)$coefficients

# Organizar resultados en una tabla
tabla_betas_modelo_1 <- data.frame(
  Variable = rownames(coeficientes_modelo_1),
  Beta = coeficientes_modelo_1[, "Estimate"],
  Error_estandar = coeficientes_modelo_1[, "Std. Error"],
  Valor_t = coeficientes_modelo_1[, "t value"],
  Valor_p = coeficientes_modelo_1[, "Pr(>|t|)"],
  row.names = NULL
)

# Mostrar resultados redondeados
tabla_betas_presentacion <- tabla_betas_modelo_1

tabla_betas_presentacion[, 2:4] <-
  round(tabla_betas_presentacion[, 2:4], 2)

tabla_betas_presentacion$Valor_p <-
  format.pval(
    tabla_betas_presentacion$Valor_p,
    digits = 4,
    eps = 0.001
  )

print(tabla_betas_presentacion, row.names = FALSE)

# Guardar tabla con los valores originales
dir.create("resultados", showWarnings = FALSE)

write.csv(
  tabla_betas_modelo_1,
  "resultados/tabla_betas_modelo_1.csv",
  row.names = FALSE
)

# Resumen general del modelo
cat(
  "R cuadrado ajustado:",
  round(summary(modelo_1)$adj.r.squared, 4),
  "\n"
)

cat(
  "Error estandar residual:",
  round(summary(modelo_1)$sigma, 2),
  "\n"
)

# 39.1. Exportar tabla de coeficientes beta como imagen

# Crear etiquetas con nombres mas claros
tabla_betas_imagen <- tabla_betas_presentacion %>%
  mutate(
    Variable = recode(
      Variable,
      "(Intercept)" = "Intercepto",
      "productos_diferentes" = "Productos diferentes",
      "interes_home_decor" = "Interes Home decor",
      "tasa_desempleo" = "Tasa de desempleo",
      "numero_semana" = "Numero de semana"
    ),
    fila = 5:1
  )

# Convertir los valores en texto
tabla_betas_imagen <- tabla_betas_imagen %>%
  mutate(
    Beta = format(
      Beta, big.mark = ".", decimal.mark = ",",
      nsmall = 2, trim = TRUE
    ),
    Error_estandar = format(
      Error_estandar, big.mark = ".", decimal.mark = ",",
      nsmall = 2, trim = TRUE
    ),
    Valor_t = format(
      Valor_t, decimal.mark = ",",
      nsmall = 2, trim = TRUE
    ),
    Valor_p = gsub("\\.", ",", Valor_p)
  )

# Crear imagen de la tabla
grafica_betas_modelo_1 <- ggplot(tabla_betas_imagen) +
  geom_tile(
    aes(x = 5, y = fila),
    width = 10,
    height = 0.85,
    fill = "#FCEBF2"
  ) +
  geom_text(
    aes(x = 0.2, y = fila, label = Variable),
    hjust = 0,
    size = 3.5
  ) +
  geom_text(
    aes(x = 5.0, y = fila, label = Beta),
    size = 3.3
  ) +
  geom_text(
    aes(x = 7.0, y = fila, label = Error_estandar),
    size = 3.3
  ) +
  geom_text(
    aes(x = 8.4, y = fila, label = Valor_t),
    size = 3.3
  ) +
  geom_text(
    aes(x = 9.5, y = fila, label = Valor_p),
    size = 3.3,
    fontface = "bold",
    color = "#A82D63"
  ) +
  annotate(
    "text",
    x = c(0.2, 5, 7, 8.4, 9.5),
    y = 5.95,
    label = c(
      "Variable", "Beta", "Error est.",
      "Valor t", "Valor p"
    ),
    hjust = c(0, 0.5, 0.5, 0.5, 0.5),
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.5, 6.3),
    expand = c(0, 0)
  ) +
  labs(
    title = "Coeficientes de la primera regresion multivariada",
    subtitle = "Modelo 1 - 51 semanas completas",
    caption = "Fuente: Elaboracion propia con datos de UCI, Google Trends y ONS"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(color = "gray50")
  )

# Guardar imagen
ggsave(
  filename = "resultados/tabla_betas_modelo_1.png",
  plot = grafica_betas_modelo_1,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

# 41. Segundo modelo de regresion y comparacion

# Revisar la relacion entre desempleo y tiempo
correlacion_desempleo_tiempo <- cor(
  datos_regresion_1$tasa_desempleo,
  datos_regresion_1$numero_semana,
  use = "complete.obs"
)

cat(
  "Correlacion entre desempleo y numero de semana:",
  round(correlacion_desempleo_tiempo, 3),
  "\n"
)

# Estimar un segundo modelo mas sencillo
# Se excluye la tasa de desempleo para evaluar
# si aporta informacion adicional al primer modelo

modelo_2 <- lm(
  ventas_totales ~
    productos_diferentes +
    interes_home_decor +
    numero_semana,
  data = datos_regresion_1
)

# Revisar resultados del segundo modelo
summary(modelo_2)


# 42. Comparacion entre el modelo 1 y el modelo 2

comparacion_modelos <- data.frame(
  Indicador = c(
    "Observaciones",
    "R cuadrado",
    "R cuadrado ajustado",
    "Error estandar residual"
  ),
  Modelo_1 = c(
    nobs(modelo_1),
    summary(modelo_1)$r.squared,
    summary(modelo_1)$adj.r.squared,
    summary(modelo_1)$sigma
  ),
  Modelo_2 = c(
    nobs(modelo_2),
    summary(modelo_2)$r.squared,
    summary(modelo_2)$adj.r.squared,
    summary(modelo_2)$sigma
  )
)

print(comparacion_modelos)

# Revisar si el desempleo aporta informacion al modelo

# Valor p de la tasa de desempleo en el modelo 1
valor_p_desempleo <- summary(modelo_1)$coefficients[
  "tasa_desempleo", "Pr(>|t|)"
]

# Diferencia en el R cuadrado ajustado
diferencia_r2_ajustado <-
  summary(modelo_1)$adj.r.squared -
  summary(modelo_2)$adj.r.squared

# Mostrar los resultados
cat(
  "Valor p de la tasa de desempleo:",
  round(valor_p_desempleo, 3), "\n"
)

cat(
  "Diferencia de R cuadrado ajustado:",
  round(diferencia_r2_ajustado, 4), "\n"
)


# 43. Exportar comparacion de modelos

dir.create("resultados", showWarnings = FALSE)

# Guardar tabla CSV
write.csv(
  comparacion_modelos,
  "resultados/comparacion_modelos.csv",
  row.names = FALSE
)

# Preparar tabla en formato de imagen
decimales <- c(0, 4, 4, 2)

formato_modelo <- function(valores) {
  mapply(
    function(valor, digitos) {
      formatC(
        valor,
        format = "f",
        digits = digitos,
        big.mark = ".",
        decimal.mark = ","
      )
    },
    valores,
    decimales
  )
}

tabla_comparacion_imagen <- data.frame(
  indicador = comparacion_modelos$Indicador,
  modelo_1 = formato_modelo(comparacion_modelos$Modelo_1),
  modelo_2 = formato_modelo(comparacion_modelos$Modelo_2),
  fila = 4:1
)

grafica_comparacion <- ggplot(
  tabla_comparacion_imagen
) +
  geom_tile(
    aes(x = 5, y = fila),
    width = 10,
    height = 0.85,
    fill = "#FCEBF2"
  ) +
  geom_text(
    aes(x = 0.3, y = fila, label = indicador),
    hjust = 0,
    size = 4
  ) +
  geom_text(
    aes(x = 5.2, y = fila, label = modelo_1),
    size = 4,
    color = "#A82D63"
  ) +
  geom_text(
    aes(x = 9.5, y = fila, label = modelo_2),
    hjust = 1,
    size = 4,
    color = "#A82D63"
  ) +
  annotate(
    "text",
    x = c(0.3, 5.2, 9.5),
    y = 5,
    label = c("Indicador", "Modelo 1", "Modelo 2"),
    hjust = c(0, 0.5, 1),
    fontface = "bold",
    size = 4
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.5, 5.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Comparacion de modelos de regresion",
    subtitle = "Ventas semanales - 51 observaciones",
    caption = "Fuente: Elaboracion propia con datos de UCI, Google Trends y ONS"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "resultados/comparacion_modelos.png",
  plot = grafica_comparacion,
  width = 10,
  height = 4.5,
  dpi = 300,
  bg = "white"
)

# 44. Tabla de coeficientes beta del segundo modelo

# Extraer los coeficientes
coeficientes_modelo_2 <- summary(modelo_2)$coefficients

tabla_betas_modelo_2 <- data.frame(
  Variable = rownames(coeficientes_modelo_2),
  Beta = coeficientes_modelo_2[, "Estimate"],
  Error_estandar = coeficientes_modelo_2[, "Std. Error"],
  Valor_t = coeficientes_modelo_2[, "t value"],
  Valor_p = coeficientes_modelo_2[, "Pr(>|t|)"],
  row.names = NULL
)

# Guardar los valores originales
write.csv(
  tabla_betas_modelo_2,
  "resultados/tabla_betas_modelo_2.csv",
  row.names = FALSE
)

# Preparar valores para la imagen
formato_numero <- function(x) {
  formatC(
    x,
    format = "f",
    digits = 2,
    big.mark = ".",
    decimal.mark = ","
  )
}

formato_p <- function(p) {
  ifelse(
    p < 0.001,
    "< 0,001",
    formatC(p, format = "f", digits = 3,
            decimal.mark = ",")
  )
}

tabla_betas_imagen_2 <- tabla_betas_modelo_2 %>%
  mutate(
    Variable = recode(
      Variable,
      "(Intercept)" = "Intercepto",
      "productos_diferentes" = "Productos diferentes",
      "interes_home_decor" = "Interes Home decor",
      "numero_semana" = "Numero de semana"
    ),
    Beta = formato_numero(Beta),
    Error_estandar = formato_numero(Error_estandar),
    Valor_t = formato_numero(Valor_t),
    Valor_p = formato_p(Valor_p),
    fila = 4:1
  )

# Crear tabla con el estilo del proyecto
grafica_betas_modelo_2 <- ggplot(tabla_betas_imagen_2) +
  geom_tile(
    aes(x = 5, y = fila),
    width = 10,
    height = 0.85,
    fill = "#FCEBF2"
  ) +
  geom_text(
    aes(x = 0.2, y = fila, label = Variable),
    hjust = 0,
    size = 3.5
  ) +
  geom_text(
    aes(x = 4.8, y = fila, label = Beta),
    size = 3.4
  ) +
  geom_text(
    aes(x = 6.8, y = fila, label = Error_estandar),
    size = 3.4
  ) +
  geom_text(
    aes(x = 8.3, y = fila, label = Valor_t),
    size = 3.4
  ) +
  geom_text(
    aes(x = 9.5, y = fila, label = Valor_p),
    size = 3.4,
    color = "#A82D63",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = c(0.2, 4.8, 6.8, 8.3, 9.5),
    y = 5,
    label = c(
      "Variable", "Beta", "Error est.",
      "Valor t", "Valor p"
    ),
    hjust = c(0, 0.5, 0.5, 0.5, 0.5),
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.5, 5.5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Coeficientes del segundo modelo de regresion",
    subtitle = "Modelo 2 - 51 semanas completas",
    caption = paste(
      "Fuente: Elaboracion propia con datos",
      "de UCI y Google Trends"
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(color = "gray50")
  )

# Guardar imagen
ggsave(
  filename = "resultados/tabla_betas_modelo_2.png",
  plot = grafica_betas_modelo_2,
  width = 12,
  height = 4.5,
  dpi = 300,
  bg = "white"
)

print(tabla_betas_modelo_2)

# Verificar que ambos modelos existen
exists("modelo_1")
exists("modelo_2")

# Verificar que son modelos de regresion lineal
class(modelo_1)
class(modelo_2)

# Numero de observaciones utilizadas
nobs(modelo_1)
nobs(modelo_2)
