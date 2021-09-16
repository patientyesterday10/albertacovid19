FROM debian:unstable-slim as builder
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y cmake libssl-dev libcurl4-openssl-dev python3 python3-pip python3-pandas python3-numpy r-base r-base-dev xvfb chromium chromium-driver default-jre

# Locale / TZ
ENV TZ=America/Edmonton
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Setup working directory
RUN mkdir /app
WORKDIR /app

FROM builder as build1
# Install python requirements
COPY requirements.txt /app/requirements.txt
RUN python3 -m pip install -U -r /app/requirements.txt
COPY app/R/lib/packages.R /app/r_packages.R
RUN /usr/bin/Rscript --vanilla /app/r_packages.R

FROM build1 as appimg
# Copy Application Files
COPY app /app/

# Start XVFB
ENV DISPLAY=:99
RUN Xvfb $DISPLAY -ac -screen 0 $XVFB_WHD -nolisten tcp &

# Run Python App
WORKDIR /app/
CMD ls -al * && python3 -V && (python3 get_ab_data.py || true) && python3 main.py
