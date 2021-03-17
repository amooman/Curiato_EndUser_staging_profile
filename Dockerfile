FROM rocker/r-ver:4.0.3
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("RPostgreSQL",upgrade="never", version = "0.6-2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_github("shahreyar-abeer/DT@e91f9ca4e60807ece909bec3bae433fef3d48ab2")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@5fa9a92e2863c443cdbcd6d477ab97c6db717f79")'
RUN Rscript -e 'remotes::install_github("appsilon/shiny.semantic@9dfe9b374e2a4e4b861e2a0c6bc737cfe535fe00")'
RUN Rscript -e 'remotes::install_github("JohnCoene/waiter@d3f7aafaac9a6cf8095c548dfbae2e1e486e3458")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');curiato.enduser.staging.profile::run_app()"
