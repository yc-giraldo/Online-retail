
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
