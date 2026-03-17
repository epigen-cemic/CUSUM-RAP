# Herramienta RAP CUSUM: Guía de Desarrollo y Administración

Esta guía está dirigida a desarrolladores, administradores de IT y mantenedores de la Herramienta RAP CUSUM. Si eres un usuario final que busca instrucciones sobre cómo utilizar la interfaz, puedes consultar el **Manual de Usuario** ubicado en `www/docs/Documentation.pdf` o haciendo clic en el botón "Help" dentro de la aplicación.

## 1. Estructura del Proyecto
El repositorio está estructurado como una aplicación Shiny estándar con componentes modularizados.

* **Directorio Raíz (`/`)** Contiene los scripts principales de la aplicación (`app.R`, `global.R`), archivos del proyecto (`CUSUM-RAP.Rproj`) y configuraciones de nivel superior (`config.json`).
* **Directorio `R/`**: Contiene la lógica central y los módulos Shiny. Esto incluye `mod_cusum.R` (el módulo principal de UI y Servidor) y los scripts del backend (`functions_cusum.R`, `functions_io.R`, `functions_parameters.R`, `functions_plot.R`).
* **Directorio `www/`**: Contiene los recursos web estáticos expuestos al navegador. Esto incluye `config.css` (estilos de UI), la carpeta `docs/` (documentación para el usuario final) y recursos gráficos (`AnalysisforAction_white.png`, `splash.png`, `default.ico`).

```text
/ (Directorio Raíz)
├── app.R                        # Script principal de la aplicación
├── global.R                     # Script de configuración y variables globales
├── CUSUM-RAP.Rproj              # Archivo del proyecto de RStudio
├── config.json                  # Configuraciones (ej. jerarquías espaciales)
│
├── R/                           # Lógica central y módulos Shiny
│   ├── mod_cusum.R              # Módulo principal (UI y Servidor)
│   ├── functions_cusum.R        # Funciones matemáticas del algoritmo
│   ├── functions_io.R           # Funciones de lectura y guardado
│   ├── functions_parameters.R   # Funciones para el cálculo de variables
│   └── functions_plot.R         # Funciones para generar los gráficos
│
└── www/                         # Recursos web estáticos
    ├── config.css               # Hoja de estilos (colores y diseño)
    ├── AnalysisforAction_white.png # Recurso gráfico
    ├── splash.png               # Recurso gráfico
    ├── default.ico              # Ícono de la pestaña
    │
    └── docs/                    # Documentación para el usuario final
        └── Documentation.pdf    # El manual que se abre al tocar "Help"
```

## 2. Ejecutar la App Localmente (Desarrollo)
Para ejecutar o probar la aplicación en un entorno de desarrollo R:
1. Abrir el archivo `CUSUM-RAP.Rproj` en RStudio.
2. Abrir `app.R`.
3. Hacer clic en **Run App** en RStudio, o ejecutar `shiny::runApp()` en la consola.

## 3. Despliegue en la Web
Para que la herramienta sea accesible a través de un navegador web, se debe alojar la aplicación en un servidor.

**Opción A: shinyapps.io (Hosting en la nube más sencillo)**
1. Puedes crear una cuenta gratuita o paga en [shinyapps.io](https://www.shinyapps.io/).
2. Instalar el paquete de despliegue en R ejecutando `install.packages("rsconnect")`.
3. Autenticar tu sesión de RStudio usando el token seguro provisto en tu panel de shinyapps.io.
4. Desplegar la app haciendo clic en el botón azul **Publish** en la esquina superior derecha de tu editor de RStudio, o ejecutando `rsconnect::deployApp()` en la consola.

**Opción B: Infraestructura Propia**
Si tus datos epidemiológicos requieren un cumplimiento estricto de privacidad que impide el uso de hosting en la nube de terceros, puedes alojar la aplicación en tu propia infraestructura (AWS, DigitalOcean o un servidor interno seguro). Esto requiere instalar **Shiny Server** (open-source) en una máquina Linux o empaquetar la aplicación utilizando contenedores **Docker**.

## 4. Configuración y Personalización de la App
Los administradores pueden personalizar la apariencia y el mapeo geográfico sin alterar la lógica central de R.

* **Modificación del Diseño (`www/config.css`)**: El estilo de la aplicación está codificado en este archivo CSS. Busca y reemplaza los códigos hexadecimales específicos para cambiar el aspecto visual (ej., `#4a4a4a` para fondos gris oscuro, `#616161` para texto estándar).
* **Agregar Países y Regiones (`config.json`)**: La jerarquía espacial se define aquí. Añade un nuevo arreglo definiendo los niveles administrativos de mayor a menor (ej., `"Brazil": { "levels": ["country", "state", "municipality"] }`).
