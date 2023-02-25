FROM willvieira/rocker-cmdstanr:v1-release

# Install renv
ENV RENV_VERSION 0.16.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# upload list of installed packages to the container
COPY renv.lock renv.lock

# set library path
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf

# install dependences
RUN R -e "renv::restore()"
