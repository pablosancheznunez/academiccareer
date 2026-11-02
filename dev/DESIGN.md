# academiccareer — diseño

**Responsible academic career assessment**
Estado: en construcción (0.1.0), autónomo: sin dependencia de narrativecv (decisión del 2026-11-02). Fecha: 2026-11-02.

## 1. Qué es y qué no es

Un paquete para **describir la trayectoria de una persona investigadora a lo largo del tiempo**, a partir de evidencias abiertas y de lo que la propia persona declara, en todas las dimensiones que proponen DORA y CoARA, no solo las publicaciones.

No es:

- otro generador de CV narrativos;
- un sistema de clasificación: **nunca ordena investigadores de mejor a peor**, no calcula una puntuación compuesta y en la versión 0.1 no compara personas entre sí;
- una herramienta de métricas basadas en la revista: no usa factor de impacto, cuartiles ni índice h.

Nombre: `academiccareer` está libre en CRAN. También están libres `careertrace` y `careerpaths`, más cortas. Prefijo: `ac_`.

## 2. Diferencia con tus otras herramientas

| Herramienta | Pregunta que responde |
|---|---|
| narrativecv | ¿Qué evidencia hay hoy para cada sección de un CV narrativo? |
| OpenResearchMetrics | ¿Qué indicadores de ciencia abierta tiene este perfil? |
| **academiccareer** | ¿Cómo ha evolucionado la trayectoria: contribuciones, roles, colaboraciones y transiciones? |

**Reutilización.** narrativecv ya tiene conectores probados y mantenidos para ORCID, OpenAlex, DataCite y OpenAIRE. academiccareer debería **importar narrativecv** en lugar de duplicarlos: así research-tools-ops solo mantiene un código. Inconveniente: academiccareer no podrá ir a CRAN hasta que narrativecv esté aceptado.

## 3. Dimensiones y de dónde sale la evidencia

| Dimensión | Fuentes abiertas | Cobertura |
|---|---|---|
| Contribuciones científicas | ORCID y OpenAlex (trabajos por tipo) | Buena |
| Capacidades metodológicas | Temas de OpenAlex (solo aproximación) | Baja; sobre todo declarada |
| Colaboraciones nacionales e internacionales | Instituciones y países de los coautores (OpenAlex) | Buena |
| Supervisión y mentoría | Tesis en DataCite o repositorios, a veces | Baja; sobre todo declarada |
| Software científico | DataCite (software en Zenodo) | Media |
| Ciencia abierta | Acceso abierto (OpenAlex), datos y preprints (DataCite) | Buena |
| Transferencia de conocimiento | Coautorías con empresas, gobierno u ONG (tipo de institución en OpenAlex); financiación (ORCID) | Media, aproximada |
| Participación en políticas científicas | Servicio y membresías en ORCID | Baja; sobre todo declarada |
| Diversidad de roles | Roles CRediT donde existen; revisión por pares y servicio en ORCID | Baja |

Esta tabla debe aparecer en cada informe: **que algo no se encuentre no significa que no exista**, el mismo principio que narrativecv.

## 4. Modelo de datos

Una tabla de **eventos de carrera**, una fila por evidencia:

| Campo | Contenido |
|---|---|
| `event_id` | Identificador estable |
| `start`, `end` | Fechas (un evento puede ser puntual o un periodo) |
| `dimension` | Una de las dimensiones de la sección 3 |
| `type` | Artículo, dataset, software, empleo, financiación, supervisión… |
| `role` | CRediT donde aplica; supervisor, revisor, investigador principal… |
| `partners` | Instituciones, países y sector |
| `source`, `source_record`, `evidence_url` | Procedencia trazable |
| `declared_by` | `source` (fuente abierta) o `self` (declarado por la persona) |

**Contexto de la carrera** (`ac_context()`): interrupciones (permisos, enfermedad, cuidados), tiempo parcial, área y país. Las interrupciones se muestran como tales en la línea temporal, no como huecos, siguiendo lo que piden R4RI y CoARA.

## 5. Funciones

| Función | Qué hace |
|---|---|
| `ac_collect(orcid, sources)` | Recoge evidencias de ORCID, OpenAlex, DataCite y OpenAIRE a través de narrativecv (antes `import_orcid()`) |
| `ac_add_evidence()` y `ac_read_evidence(csv)` | Añaden lo que las fuentes no recogen: mentoría, docencia, políticas, transferencia |
| `ac_context()` | Declara interrupciones y contexto |
| `ac_timeline(by = "year")` | Línea temporal por dimensión (antes `build_career_timeline()`) |
| `ac_contributions()` | Mezcla de tipos y roles por periodo (antes `analyse_contributions()`) |
| `ac_collaborations()` | Coautores, instituciones, países y sectores a lo largo del tiempo (antes `map_collaborations()`) |
| `ac_transitions()` | Cambios de institución, país, sector, tipo de producción o rol (antes `identify_career_transitions()`) |
| `ac_profile()` | Perfil descriptivo con evidencias enlazadas (antes `generate_career_profile()`) |
| `ac_report()` | Informe HTML autocontenido |

### Transiciones: reglas transparentes, no aprendizaje automático

- Cambio de institución, país o sector, a partir del historial de empleo.
- Cambio en la mezcla de producción entre dos ventanas temporales, por ejemplo de artículos a software o datos, con un umbral documentado.
- Cambio de posición en la autoría, como aproximación a la supervisión. Se puede desactivar en áreas con orden alfabético de autores (matemáticas, economía).

### Perfiles que no encajan en el modelo tradicional

`ac_profile()` describe el reparto de la actividad entre dimensiones y destaca las **contribuciones distintivas**, por ejemplo una parte grande de la producción en software, datos, transferencia o docencia. El texto se genera con plantillas que siempre enlazan la evidencia, y **nunca usa comparativos** ("mejor", "más productivo"). Es la funcionalidad diferencial que propones: hacer visibles las carreras con mucho software, docencia, transferencia, datos o ciencia abierta.

## 6. Ética y privacidad

- Los datos públicos de ORCID u OpenAlex siguen siendo datos personales según el RGPD. El uso previsto es el **propio perfil** o el de alguien que ha dado su **consentimiento**.
- Las funciones procesan **un ORCID cada vez**. No habrá funciones para procesar a muchas personas a la vez ni para compararlas en la versión 0.1, por diseño.
- Cada informe incluye la fecha de consulta, las fuentes consultadas, su cobertura y lo que la persona ha declarado.
- La caché es solo local.

## 7. Dependencias

`Imports`: narrativecv (o curl y jsonlite mientras narrativecv no esté en CRAN). `Suggests`: knitr, rmarkdown y testthat. Gráficos con R base.

## 8. Datos de ejemplo y web

- Cuatro o cinco **trayectorias ficticias**: una tradicional centrada en artículos, una con mucho software, una orientada a la transferencia, una centrada en docencia y una con una interrupción. Sirven de ejemplo y enseñan los perfiles no tradicionales. Todas marcadas como sintéticas.
- Pruebas contra respuestas grabadas de las API, como en narrativecv.
- Web en `pablosancheznunez.com/academiccareer/` con un informe de ejemplo.

## 9. Hoja de ruta

| Versión | Contenido |
|---|---|
| 0.1 | `ac_collect()`, evidencia declarada, contexto, línea temporal, contribuciones, perfil e informe |
| 0.2 | Colaboraciones y transiciones |
| 0.3 | Software en GitHub, sectores con ROR, contexto específico del CVA español |

## 10. Decisiones abiertas

1. Nombre: `academiccareer`, `careertrace` o `careerpaths`.
2. Si se incluyen las citas como contexto, desactivadas por defecto, o se dejan fuera del todo.
3. Si en el futuro se permite el análisis agregado de una unidad (un departamento) con consentimiento, y cómo evitar que se use para ordenar personas.
