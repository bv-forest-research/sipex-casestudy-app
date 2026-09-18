library(shiny)
library(leaflet)
library(jsonlite)
library(htmltools)
library(shinyWidgets)
library(sf)

BASE_COLOR <- "#1f2933"
HIGHLIGHT_COLOR <- "#e8aa00"

TAG_SEP <- "|"

conditional_filters <- list(
  wildlife_species = list(parent = "value", value = "Wildlife")
)

category_labels <- c(
  bec_zone_acron   = "BEC Zone",
  tree_common = "Tree Species",
  forest_type = "Forest Type",
  value = "Management Objective",
  wildlife_species = "Wildlife Species",
  technique = "Technique Method",
  technique_2      = "Technique Method",
  wildlife_species = "Wildlife Species"
)

wms_url <- "https://maps.skeenasalmon.info/geoserver/ows"

wms_layers <- c(
  "Timber Supply Areas"        = "geonode:tsa_boundaries_py_bc_3005",
  "Tree Farm Licenses"         = "geonode:tree_farm_license_py_bc_3005",
  "Natural Resource Districts" = "geonode:natural_resource_district_py_bc_3005"
)

wms_z <- c(
  "Timber Supply Areas"        = 10,
  "Natural Resource Districts" = 20,
  "Tree Farm Licenses"         = 30
)

tag_labels <- character(0)

get_category_label <- function(cat) {
  lbl <- unname(category_labels[cat])
  if (length(lbl) == 0 || is.na(lbl)) tools::toTitleCase(gsub("_", " ", cat)) else lbl
}

# ---------------------------------------------------------------------------
# fetch CKAN data
# ---------------------------------------------------------------------------
ckan_base_url <- "https://resources.sipexchangebc.com"

query_url <- paste0(
  ckan_base_url, "/api/3/action/package_search",
  "?fq=", utils::URLencode('tags:"Case Study"', reserved = TRUE),
  "&rows=1000"
)

ckan_response <- tryCatch(
  jsonlite::fromJSON(query_url, simplifyVector = FALSE),
  error = function(e) { warning("CKAN fetch failed: ", conditionMessage(e)); NULL }
)

if (is.null(ckan_response) || !isTRUE(ckan_response$success)) {
  warning("CKAN package_search did not return a successful result.")
  case_study_datasets <- list()
} else {
  case_study_datasets <- ckan_response$result$results
}

# ---------------------------------------------------------------------------
# extract site locations
# ---------------------------------------------------------------------------

# fills in a default value when something is NULL, empty, or NA
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

get_publication_year <- function(ds) {
  val <- ds[["publication_yr"]]
  if (is.null(val) && length(ds$extras) > 0) {
    for (e in ds$extras) {
      if (identical(e$key, "publication_yr")) val <- e$value
    }
  }
  trimws(as.character(val %||% ""))
}

# test helper
# using this to FIND lat and long coords win a large json dump
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

# build one row per site, for every dataset
map_point_rows <- list()
for (ds in case_study_datasets) {
  for (s in find_site_locations(ds)) {
    dataset_name <- ds$name %||% ""
    site_name <- s$name %||% ""
    map_point_rows[[length(map_point_rows) + 1]] <- data.frame(
      point_id      = paste(dataset_name, site_name, length(map_point_rows) + 1, sep = "::"),
      dataset_title = ds$title %||% ds$name %||% "Untitled",
      dataset_name  = dataset_name,
      dataset_url   = paste0(ckan_base_url, "/dataset/", dataset_name),
      dataset_notes = ds$notes %||% "",
      site_name     = site_name,
      lat           = s$lat,
      lng           = s$lng,
      stringsAsFactors = FALSE
    )
  }
}

if (length(map_point_rows) == 0) {
  case_study_map_points <- data.frame(
    point_id = character(), dataset_title = character(), dataset_name = character(),
    dataset_url = character(), dataset_notes = character(), site_name = character(),
    lat = numeric(), lng = numeric()
  )
} else {
  case_study_map_points <- do.call(rbind, map_point_rows)
}

# ---------------------------------------------------------------------------
# fetch relevant tag list - gsheet, one column per category
# ---------------------------------------------------------------------------
tag_whitelist_csv_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSFm-nLI6RRMzH-VFEhiPkC91bsEUBgDqNo80dLkzRqeUT8TCFc1lc1H18jynaKYM-I76CrFfFiwMR_/pub?output=csv"

whitelist_raw <- tryCatch(
  read.csv(tag_whitelist_csv_url, stringsAsFactors = FALSE, na.strings = ""),
  error = function(e) { warning("Tag whitelist fetch failed: ", conditionMessage(e)); NULL }
)

# reformat into a list
whitelist_rows <- list()
if (!is.null(whitelist_raw)) {
  for (category in names(whitelist_raw)) {
    tags_in_col <- whitelist_raw[[category]]
    tags_in_col <- trimws(tags_in_col[!is.na(tags_in_col) & tags_in_col != ""])
    if (length(tags_in_col) > 0) {
      whitelist_rows[[length(whitelist_rows) + 1]] <- data.frame(
        tag = tags_in_col,
        category = category,
        stringsAsFactors = FALSE
      )
    }
  }
}

if (length(whitelist_rows) == 0) {
  tag_whitelist <- data.frame(tag = character(), category = character())
} else {
  tag_whitelist <- do.call(rbind, whitelist_rows)
}

# ---------------------------------------------------------------------------------------
# only keep dataset tags that match the whitelist and combine that with the dataset slug
# ---------------------------------------------------------------------------------------
dataset_tags_lookup <- vapply(case_study_datasets, function(ds) {
  if (is.null(ds$tags) || length(ds$tags) == 0) return("")
  tag_names <- vapply(ds$tags, function(t) t$display_name %||% t$name %||% "", character(1))
  paste(tag_names[tag_names %in% tag_whitelist$tag], collapse = TAG_SEP)
}, character(1))
names(dataset_tags_lookup) <- vapply(case_study_datasets, function(ds) ds$name %||% "", character(1))

case_study_map_points$display_tags <- unname(dataset_tags_lookup[case_study_map_points$dataset_name])
case_study_map_points$display_tags[is.na(case_study_map_points$display_tags)] <- ""

dataset_year_lookup <- vapply(case_study_datasets, get_publication_year, character(1))
names(dataset_year_lookup) <- vapply(case_study_datasets, function(ds) ds$name %||% "", character(1))

case_study_map_points$publication_year <- unname(dataset_year_lookup[case_study_map_points$dataset_name])
case_study_map_points$publication_year[is.na(case_study_map_points$publication_year)] <- ""

tags_in_use <- unique(unlist(strsplit(case_study_map_points$display_tags, TAG_SEP, fixed = TRUE)))
tag_whitelist <- tag_whitelist[tag_whitelist$tag %in% tags_in_use, , drop = FALSE]

tsa_wfs_url <- paste0(
  "https://maps.skeenasalmon.info/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature",
  "&typename=geonode:tsa_boundaries_py_bc_3005&outputFormat=json&srsName=EPSG:4326"
)

old_timeout <- options(timeout = 300)
tsa_polygons <- tryCatch({
  tmp <- tempfile(fileext = ".geojson")
  utils::download.file(tsa_wfs_url, tmp, mode = "wb", quiet = TRUE)
  sf::st_make_valid(sf::st_read(tmp, quiet = TRUE))
}, error = function(e) { warning("TSA fetch failed: ", conditionMessage(e)); NULL })
options(old_timeout)

tsa_matched <- NULL
if (!is.null(tsa_polygons) && "TS_NMBR_DN" %in% names(tsa_polygons) && nrow(case_study_map_points) > 0) {
  use_s2 <- sf::sf_use_s2(FALSE)
  tsa_matched <- tryCatch({
    pts_sf <- sf::st_as_sf(
      case_study_map_points[, c("point_id", "lng", "lat")],
      coords = c("lng", "lat"), crs = 4326
    )
    joined <- sf::st_drop_geometry(sf::st_join(pts_sf, tsa_polygons["TS_NMBR_DN"]))
    joined <- joined[!duplicated(joined$point_id), ]
    as.character(joined$TS_NMBR_DN[match(case_study_map_points$point_id, joined$point_id)])
  }, error = function(e) { warning("TSA join failed: ", conditionMessage(e)); NULL },
  finally = sf::sf_use_s2(use_s2))
}

case_study_map_points$tsa_name <- rep("Outside TSAs", nrow(case_study_map_points))
if (!is.null(tsa_matched)) {
  tsa_matched <- trimws(tsa_matched)
  case_study_map_points$tsa_name <- ifelse(is.na(tsa_matched) | !nzchar(tsa_matched), "Outside TSAs", tsa_matched)
}

# ---------------------------------------------------------------------------
# build filter checkbox inputs from the tag whitelist
# ---------------------------------------------------------------------------
filter_categories <- unique(tag_whitelist$category)

filter_inputs <- tagList(lapply(filter_categories, function(cat) {
  tags_in_cat <- tag_whitelist$tag[tag_whitelist$category == cat]
  
  tag_choices <- setNames(
    tags_in_cat,
    ifelse(tags_in_cat %in% names(tag_labels), unname(tag_labels[tags_in_cat]), tags_in_cat)
  )
  
  picker <- pickerInput(
    inputId = paste0("filter_", cat),
    label = get_category_label(cat),
    choices = tag_choices,
    multiple = TRUE,
    options = pickerOptions(
      actionsBox = TRUE,
      container = "body",
      liveSearch = TRUE,
      selectedTextFormat = "count > 2",
      noneSelectedText = "All",
      size = 10
    )
  )
  
  cond <- conditional_filters[[cat]]
  if (is.null(cond)) return(picker)
  
  parent_id <- paste0("filter_", cond$parent)
  trigger   <- gsub("'", "\\\\'", cond$value)
  conditionalPanel(
    condition = sprintf(
      "input['%s'] && input['%s'].indexOf('%s') !== -1",
      parent_id, parent_id, trigger
    ),
    picker
  )
}))

year_ranges <- list(
  "All years"      = c(-Inf, Inf),
  "Before 2000"    = c(-Inf, 1999),
  "2000\u20132009" = c(2000, 2009),
  "2010\u20132019" = c(2010, 2019),
  "2020 and after" = c(2020, Inf)
)

years_num <- suppressWarnings(as.numeric(case_study_map_points$publication_year))
years_num <- years_num[!is.na(years_num)]
year_ranges <- year_ranges[c(
  TRUE,
  vapply(year_ranges[-1], function(r) any(years_num >= r[1] & years_num <= r[2]), logical(1))
)]

if (length(year_ranges) > 1) {
  filter_inputs <- tagList(
    filter_inputs,
    pickerInput(
      inputId = "filter_publication_year",
      label = "Publication Year",
      choices = names(year_ranges),
      selected = "All years",
      multiple = FALSE,
      options = pickerOptions(container = "body", size = 10)
    )
  )
}

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
            <a class="nav-link" href="https://resources.sipexchangebc.com/">Explore Resources</a>
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
    
    div(
      id = "left-sidebar", class = "sidebar-wrapper left open",
      div(class = "sidebar-clip",
          div(class = "sidebar-content",
              h4("Filters"),
              div(id = "filters-body", filter_inputs),
              actionLink("reset_filters", "Clear filters", class = "details-link-button")
          )
      ),
      div(class = "toggle-tab left-tab", onclick = "toggleSidebar('left')",
          span(class = "icon", HTML("&#8249;")))
    ),
    
    div(
      id = "map-container",
      leafletOutput("map", height = "100%")
    ),
    
    div(
      id = "right-sidebar", class = "sidebar-wrapper right open",
      div(class = "toggle-tab right-tab", onclick = "toggleSidebar('right')",
          span(class = "icon", HTML("&#8250;"))),
      div(class = "sidebar-clip",
          div(class = "sidebar-content",
              h4("Details"),
              div(id = "details-body", uiOutput("details_ui"))
          )
      )
    ),
    
    div(id = "mobile-overlay", class = "mobile-overlay"),
    
    div(class = "mobile-toggle-btn mobile-toggle-left", onclick = "toggleSidebar('left')", HTML("&#9776;")),
    div(class = "mobile-toggle-btn mobile-toggle-right", onclick = "toggleSidebar('right')", HTML("&#8942;"))
  )
)

server <- function(input, output, session) {
  
  active_dataset <- reactiveVal(NULL)
  
  filter_is_active <- function(cat) {
    cond <- conditional_filters[[cat]]
    if (is.null(cond)) return(TRUE)
    cond$value %in% input[[paste0("filter_", cond$parent)]]
  }
  
  observeEvent(input$reset_filters, {
    for (cat in filter_categories) {
      updatePickerInput(session, paste0("filter_", cat), selected = character(0))
    }
    if (length(year_ranges) > 1) {
      updatePickerInput(session, "filter_publication_year", selected = "All years")
    }
  })
  
  observe({
    for (cat in names(conditional_filters)) {
      id <- paste0("filter_", cat)
      if (!filter_is_active(cat) && length(input[[id]]) > 0) {
        updatePickerInput(session, id, selected = character(0))
      }
    }
  })
  
  # logic is is "AND across categories, OR within a category")
  visible_points <- reactive({
    pts <- case_study_map_points
    
    year_sel <- input$filter_publication_year
    if (length(year_sel) == 1 && year_sel != "All years") {
      rng <- year_ranges[[year_sel]]
      yr <- suppressWarnings(as.numeric(pts$publication_year))
      pts <- pts[!is.na(yr) & yr >= rng[1] & yr <= rng[2], , drop = FALSE]
    }
    
    active_sel <- list()
    for (cat in filter_categories) {
      if (!filter_is_active(cat)) next
      sel <- input[[paste0("filter_", cat)]]
      if (length(sel) > 0) active_sel[[cat]] <- sel
    }
    
    if (length(active_sel) == 0) return(pts)
    
    pt_tags_list <- strsplit(pts$display_tags, TAG_SEP, fixed = TRUE)
    
    keep <- vapply(pt_tags_list, function(pt_tags) {
      all(vapply(active_sel, function(sel) any(pt_tags %in% sel), logical(1)))
    }, logical(1))
    
    pts[keep, , drop = FALSE]
  })
  
  output$map <- renderLeaflet({
    m <- leaflet() |> addTiles()
    
    for (nm in names(wms_layers)) {
      m <- m |> addWMSTiles(
        baseUrl = wms_url,
        layers = wms_layers[[nm]],
        group = nm,
        options = WMSTileOptions(
          format = "image/png", transparent = TRUE, opacity = 0.6,
          zIndex = wms_z[[nm]]
        )
      )
    }
    
    m <- m |>
      addLayersControl(
        overlayGroups = names(wms_layers),
        options = layersControlOptions(collapsed = TRUE),
        position = "bottomright"
      ) |>
      hideGroup(setdiff(names(wms_layers), "Timber Supply Areas"))
    
    if (nrow(case_study_map_points) > 0) {
      m <- m |>
        fitBounds(
          lng1 = min(case_study_map_points$lng), lat1 = min(case_study_map_points$lat),
          lng2 = max(case_study_map_points$lng), lat2 = max(case_study_map_points$lat)
        )
    } else {
      m <- m |> setView(lng = -128.6, lat = 54.3, zoom = 8)
    }
    m
  })
  
  # redraw markers whenever filters or the active dataset change
  observe({
    pts <- visible_points()
    active <- active_dataset()
    
    proxy <- leafletProxy("map") |> clearMarkers() |> clearMarkerClusters()
    
    if (nrow(pts) == 0) return()
    
    pts$tip <- ifelse(
      nzchar(pts$site_name),
      paste0(pts$dataset_title, " \u2014 ", pts$site_name),
      pts$dataset_title
    )
    
    is_active <- if (is.null(active)) rep(FALSE, nrow(pts)) else pts$dataset_name == active
    clustered <- pts[!is_active, , drop = FALSE]
    highlighted <- pts[is_active, , drop = FALSE]
    
    for (tsa in unique(clustered$tsa_name)) {
      grp <- clustered[clustered$tsa_name == tsa, , drop = FALSE]
      proxy <- proxy |> addCircleMarkers(
        data = grp, lng = ~lng, lat = ~lat, layerId = ~point_id,
        radius = 7, color = BASE_COLOR, fillColor = BASE_COLOR, weight = 1, fillOpacity = 0.9,
        label = ~tip,
        labelOptions = labelOptions(direction = "top", sticky = TRUE),
        clusterOptions = markerClusterOptions(),
        clusterId = paste0("tsa_", tsa)
      )
    }
    
    if (nrow(highlighted) > 0) {
      proxy |> addCircleMarkers(
        data = highlighted, lng = ~lng, lat = ~lat, layerId = ~point_id,
        radius = 9, color = BASE_COLOR, fillColor = HIGHLIGHT_COLOR, weight = 2, fillOpacity = 1,
        label = ~tip,
        labelOptions = labelOptions(direction = "top", sticky = TRUE)
      )
    }
  })
  
  observeEvent(input$map_marker_click, {
    clicked <- input$map_marker_click
    row <- case_study_map_points[case_study_map_points$point_id == clicked$id, ]
    if (nrow(row) == 0) return()
    ds <- row$dataset_name[1]
    
    if (identical(active_dataset(), ds)) {
      active_dataset(NULL)
    } else {
      active_dataset(ds)
      session$sendCustomMessage("openRightSidebar", list())
      
      group <- case_study_map_points[case_study_map_points$dataset_name == ds, , drop = FALSE]
      if (nrow(group) > 1) {
        leafletProxy("map") |> fitBounds(
          lng1 = min(group$lng), lat1 = min(group$lat),
          lng2 = max(group$lng), lat2 = max(group$lat)
        )
      }
    }
  })
  
  output$details_ui <- renderUI({
    ds <- active_dataset()
    if (is.null(ds)) {
      return(p("Click a site marker on the map to see details about that case study."))
    }
    
    group <- case_study_map_points[case_study_map_points$dataset_name == ds, , drop = FALSE]
    if (nrow(group) == 0) return(NULL)
    
    tags_list <- strsplit(group$display_tags[1], TAG_SEP, fixed = TRUE)[[1]]
    tags_list <- tags_list[nzchar(tags_list)]
    
    notes <- group$dataset_notes[1]
    notes_short <- if (nzchar(notes) && nchar(notes) > 300) {
      paste0(substr(notes, 1, 300), "...")
    } else {
      notes
    }
    
    tagList(
      h3(class = "details-dataset-title", group$dataset_title[1]),
      
      if (length(tags_list) > 0) {
        tagList(
          h5(class = "details-subhead", "Relevant Tags"),
          div(class = "details-tags",
              lapply(tags_list, function(t) {
                lbl <- if (t %in% names(tag_labels)) unname(tag_labels[t]) else t
                span(class = "details-tag", lbl)
              }))
        )
      },
      
      if (nzchar(notes_short)) {
        tagList(
          h5(class = "details-subhead", "Description"),
          p(class = "details-dataset-notes", notes_short)
        )
      },
      
      tagList(
        h5(class = "details-subhead", paste0("Sites (", nrow(group), ")")),
        tags$ul(class = "details-site-list",
                lapply(seq_len(nrow(group)), function(i) {
                  tags$li(sprintf("%.5f, %.5f", group$lat[i], group$lng[i]))
                })
        )
      ),
      
      a(class = "details-link-button", href = group$dataset_url[1],
        target = "_blank", rel = "noopener", "View Full Dataset \u2192")
    )
  })
}

shinyApp(ui, server)