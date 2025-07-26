# Use Plumber base image
FROM rstudio/plumber

# Install required R packages
RUN R -e "install.packages(c('randomForest', 'dplyr', 'DBI', 'RMySQL', 'ggplot2'), repos='http://cran.rstudio.com/')"

# Set working directory
WORKDIR /app

# Copy app files
COPY . /app

# Expose port
EXPOSE 8000

# Run Plumber with proper host and port binding
CMD ["plumber", "--port", "8000", "--host", "0.0.0.0", "plumber.R"]