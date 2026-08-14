# shiny base img
FROM rocker/shiny:latest

# core dependencies (curl added for healthcheck, libpq-dev for RPostgres)
RUN apt-get update && apt-get install -y \
    libpq-dev \
    curl \
    nano \
    iputils-ping

# geospatial system libs — must be installed BEFORE the R packages that link against them (sf, leaflet.extras)
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# install r packages incl dependencies
RUN R -e "install.packages(c('shiny', 'DBI', 'RPostgres', 'digest', 'DT', 'shinyjs', \
                             'yaml', 'leaflet', 'sf', 'leaflet.extras', 'readxl', 'dplyr', \
                             'htmltools', 'shinyWidgets'), dependencies=TRUE)"

# run app using shiny user
RUN sed -i 's/^# run_as.*$/run_as shiny;/' /etc/shiny-server/shiny-server.conf

# serve /srv/shiny-server as a single app (app_dir) instead of a directory of multiple apps (site_dir)
RUN sed -i 's#site_dir /srv/shiny-server;#app_dir /srv/shiny-server;#' /etc/shiny-server/shiny-server.conf

# add files to container (baked-in copy as a fallback/default; compose volume mount overrides this at runtime)
# uses the default site_dir (/srv/shiny-server) that ships with rocker/shiny — no custom conf needed
COPY app /srv/shiny-server

# set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# redirect shiny server logs to stdout and stderr
RUN ln -sf /dev/stdout /var/log/shiny-server/shiny-server.log && \
    ln -sf /dev/stderr /var/log/shiny-server/shiny-server-error.log

# set app port
EXPOSE 3838

# start server
CMD ["/usr/bin/shiny-server"]