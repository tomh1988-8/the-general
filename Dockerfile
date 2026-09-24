FROM rocker/r-ver:4.4.2

ENV R_PROFILE_USER=/dev/null
ENV RENV_CONFIG_SANDBOX_ENABLED=false
ENV RENV_CONFIG_AUTOLOADER_ENABLED=false

RUN apt-get update && apt-get install -y --no-install-recommends \
    make \
    pandoc \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libzip-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY renv.lock DESCRIPTION NAMESPACE ./

RUN R -q -e "install.packages('renv', repos = 'https://packagemanager.posit.co/cran/latest')"
RUN R -q -e "renv::restore(lockfile = 'renv.lock', library = .libPaths()[1], prompt = FALSE, clean = FALSE)"

COPY . .

RUN R CMD INSTALL .

EXPOSE 3838

CMD ["R", "-q", "-e", "options(shiny.host = '0.0.0.0', shiny.port = 3838, launch.browser = FALSE); shiny::runApp(thegeneral::the_general_app(debug = FALSE), host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]