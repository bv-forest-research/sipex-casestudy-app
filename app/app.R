library(shiny)
library(leaflet)
library(jsonlite)

# ---------------------------------------------------------------------------
# test - fetch data
# ---------------------------------------------------------------------------
ckan_base_url <- "https://resources.sipexchangebc.com"

fetch_case_study_datasets <- function(base_url) {
  query_url <- paste0(
    base_url, "/api/3/action/package_search",
    "?fq=", utils::URLencode('tags:"Case Study"', reserved = TRUE),
    "&rows=1000"
  )
  resp <- tryCatch(
    jsonlite::fromJSON(query_url, simplifyVector = FALSE),
    error = function(e) { warning("CKAN fetch failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(resp) || !isTRUE(resp$success)) {
    warning("CKAN package_search did not return a successful result.")
    return(list())
  }
  resp$result$results
}

case_study_datasets <- fetch_case_study_datasets(ckan_base_url)

# ---------------------------------------------------------------------------
# test - extract site locations
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

find_site_locations <- function(x, results = list()) {
  if (is.list(x)) {
    keys <- names(x)
    if (!is.null(keys)) {
      lat_key <- keys[grepl("^lat", keys, ignore.case = TRUE)]
      lng_key <- keys[grepl("^lon|^lng", keys, ignore.case = TRUE)]
      if (length(lat_key) >= 1 && length(lng_key) >= 1) {
        lat_val <- suppressWarnings(as.numeric(x[[lat_key[1]]]))
        lng_val <- suppressWarnings(as.numeric(x[[lng_key[1]]]))
        if (!is.na(lat_val) && !is.na(lng_val)) {
          name_key <- keys[grepl("name", keys, ignore.case = TRUE)]
          site_name <- if (length(name_key) >= 1) x[[name_key[1]]] else NA
          results[[length(results) + 1]] <- list(name = site_name, lat = lat_val, lng = lng_val)
          return(results)
        }
      }
    }
    for (el in x) results <- find_site_locations(el, results)
  }
  results
}

build_map_points <- function(datasets) {
  rows <- list()
  for (ds in datasets) {
    for (s in find_site_locations(ds)) {
      rows[[length(rows) + 1]] <- data.frame(
        dataset_title = ds$title %||% ds$name %||% "Untitled",
        dataset_name  = ds$name %||% "",
        site_name     = s$name %||% "",
        lat           = s$lat,
        lng           = s$lng,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(dataset_title = character(), dataset_name = character(),
                      site_name = character(), lat = numeric(), lng = numeric()))
  }
  do.call(rbind, rows)
}

case_study_map_points <- build_map_points(case_study_datasets)

# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

header_html <- r"[
<header class="masthead">
  <div class="header-brand-row">
    <div class="header-flex-container">
      <div class="main-logo">
        <a class="logo" href="https://sipexchangebc.com/">
          <img src="images/sipex-logo.png" alt="SIPex" title="SIPex" />
        </a>
      </div>
      <div class="right-content">
        <div class="partner-logos">
          <a href="https://sip.bvcentre.ca/">
            <img src="images/sip-logo.png" alt="Silviculture Innovation Program" />
          </a>
          <a href="https://bvcentre.ca/">
            <img src="images/bvrc-logo.png" alt="Bulkley Valley Research Centre" />
          </a>
        </div>
      </div>
    </div>
  </div>

  <div class="container">
    <nav class="navbar navbar-expand-lg">
      <button class="navbar-toggler" type="button" onclick="toggleMobileNav()"
              aria-controls="main-navigation-toggle" aria-expanded="false" aria-label="Toggle navigation">
        <span class="fa fa-bars"></span>
      </button>

      <div class="collapse navbar-collapse" id="main-navigation-toggle">
        <ul class="navbar-nav">
          <li class="nav-item">
            <a class="nav-link" href="https://sipexchangebc.com/">Home</a>
          </li>
          <li class="nav-item dropdown">
            <a class="nav-link dropdown-toggle" href="#" onclick="toggleDropdown(event, 'cop-dropdown')">Communities of Practice</a>
            <ul class="dropdown-menu" id="cop-dropdown">
              <li><a class="dropdown-item" href="https://sipexchangebc.com/communities-of-practice/">About</a></li>
              <li><a class="dropdown-item" href="https://sipexchangebc.com/find-a-community-of-practice/">Find a Community of Practice</a></li>
              <li><a class="dropdown-item" href="https://sipexchangebc.com/find-an-expert/">Find an Expert</a></li>
            </ul>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="https://sipexchangebc.com/training-and-education">Training &amp; Education</a>
          </li>
          <li class="nav-item">
            <!-- TODO: point this at your CKAN dataset search / home.index equivalent -->
            <a class="nav-link" href="#">Explore Resources</a>
          </li>
          <li class="nav-item">
            <a class="nav-link" href="https://sipexchangebc.com/featured-topics/">Featured Topics</a>
          </li>
          <li class="nav-item dropdown">
            <a class="nav-link dropdown-toggle" href="#" onclick="toggleDropdown(event, 'help-dropdown')">Help</a>
            <ul class="dropdown-menu" id="help-dropdown">
              <li><a class="dropdown-item" href="https://sipexchangebc.com/help/">Resources</a></li>
              <li><a class="dropdown-item" href="https://sipexchangebc.com/glossary/">Glossary</a></li>
            </ul>
          </li>
          <li class="nav-item cta-nav-item">
            <a class="cta-button" href="https://sipexchangebc.com/need-help-with-a-silviculture-problem/">Need Help With a Problem?</a>
          </li>
        </ul>
      </div>
    </nav>
  </div>
</header>
]"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),
    tags$script(src = "app.js")
  ),
  
  HTML(header_html),
  
  div(
    class = "app-container",
    
    # ---- left sidebar ----
    div(
      id = "left-sidebar", class = "sidebar-wrapper left open",
      div(class = "sidebar-clip",
          div(class = "sidebar-content",
              h4("Filters"),
          )
      ),
      div(class = "toggle-tab left-tab", onclick = "toggleSidebar('left')",
          span(class = "icon", HTML("&#8249;")))
    ),
    
    # ---- map ----
    div(
      id = "map-container",
      leafletOutput("map", height = "100%")
    ),
    
    # ---- right sidebar ----
    div(
      id = "right-sidebar", class = "sidebar-wrapper right open",
      div(class = "toggle-tab right-tab", onclick = "toggleSidebar('right')",
          span(class = "icon", HTML("&#8250;"))),
      div(class = "sidebar-clip",
          div(class = "sidebar-content",
              h4("Details"),
          )
      )
    ),
    
    div(id = "mobile-overlay", class = "mobile-overlay"),
    
    # test: mobile
    div(class = "mobile-toggle-btn mobile-toggle-left", onclick = "toggleSidebar('left')", HTML("&#9776;")),
    div(class = "mobile-toggle-btn mobile-toggle-right", onclick = "toggleSidebar('right')", HTML("&#8942;"))
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    m <- leaflet() |> addTiles()
    
    if (nrow(case_study_map_points) > 0) {
      m <- m |>
        addMarkers(
          data = case_study_map_points,
          lng = ~lng, lat = ~lat,
          popup = ~paste0(
            "<strong>", htmltools::htmlEscape(dataset_title), "</strong><br/>",
            ifelse(nzchar(site_name), paste0(htmltools::htmlEscape(site_name), "<br/>"), ""),
            "<a href='", ckan_base_url, "/dataset/", dataset_name, "' target='_blank'>View dataset</a>"
          )
        ) |>
        fitBounds(
          lng1 = min(case_study_map_points$lng), lat1 = min(case_study_map_points$lat),
          lng2 = max(case_study_map_points$lng), lat2 = max(case_study_map_points$lat)
        )
    } else {
      m <- m |> setView(lng = -128.6, lat = 54.3, zoom = 8)
    }
    m
  })
}

shinyApp(ui, server)