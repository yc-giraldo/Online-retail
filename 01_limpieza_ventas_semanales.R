
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

# 10. Grafica final sin semanas incompletas

grafica_ventas_final <- grafica_ventas %+%
  ventas_semanales_completas

print(grafica_ventas_final)


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
