FROM nginx:alpine

ENV NGINX_DIR=/etc/nginx

COPY ./nginx/index.html /usr/share/nginx/html/index.html

RUN mkdir -p ${NGINX_DIR}/sites-available
RUN mkdir -p ${NGINX_DIR}/sites-enabled

COPY ./nginx/setup/nginx.conf ${NGINX_DIR}
COPY ./nginx/setup/sites-available/* ${NGINX_DIR}/sites-available

RUN ln -s ${NGINX_DIR}/sites-available/registry.conf ${NGINX_DIR}/sites-enabled/registry.conf

EXPOSE 80
