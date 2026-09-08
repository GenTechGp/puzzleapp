# RELEASE_3_20 = Bioconductor 3.20 = R 4.4.2, matching R-CMD-check.yaml's pinned R version.
FROM bioconductor/bioconductor_docker:RELEASE_3_20 AS builder

COPY puzzleapp_*.tar.gz /tmp/
RUN mkdir /tmp/src \
    && tar -xzf /tmp/puzzleapp_*.tar.gz -C /tmp/src \
    && Rscript -e ' \
         if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager"); \
         if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes"); \
         options(repos = BiocManager::repositories()); \
         remotes::install_deps("/tmp/src/puzzleapp", dependencies = TRUE, upgrade = "never") \
       ' \
    && R CMD INSTALL /tmp/puzzleapp_*.tar.gz \
    && rm -rf /tmp/src /tmp/puzzleapp_*.tar.gz

# Same Ubuntu 24.04 / R 4.4.2 lineage as the builder (bioconductor_docker is
# itself built on rocker/rstudio, which derives from rocker/r-ver), without
# the compiler/TeX/build toolchain baked into the builder image.
FROM rocker/r-ver:4.4.2

# Runtime shared libraries needed by puzzleapp's compiled dependencies
# (Rsamtools, VariantAnnotation, rtracklayer, BiocParallel, ragg, V8, ...).
# List derived by running `ldd` over every .so under the builder's
# site-library and resolving each library to its owning package with
# `dpkg -S`, rather than guessed.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libopenblas0-pthread libbrotli1 libbz2-1.0 libcares2 libcom-err2 \
      libssl3t64 libcurl3t64-gnutls libcurl4t64 libdeflate0 libexpat1 libffi8 \
      libfontconfig1 libfreetype6 libfribidi0 libgcc-s1 libgfortran5 \
      libglib2.0-0t64 libgmp10 libgnutls30t64 libgomp1 libgraphite2-3 \
      libgssapi-krb5-2 libharfbuzz0b libhogweed6t64 libicu74 libidn2-0 \
      libjbig0 libjpeg-turbo8 libk5crypto3 libkeyutils1 libkrb5-3 \
      libkrb5support0 libldap2 liblerc4 liblzma5 libnettle8t64 libnghttp2-14 \
      libnode109 libp11-kit0 libpcre2-8-0 libpng16-16t64 libpsl5t64 librtmp1 \
      libsasl2-2 libsharpyuv0 libssh-4 libstdc++6 libtasn1-6 libtiff6 \
      libunistring5 libuv1t64 libwebp7 libxml2 zlib1g libzstd1 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY docker/entrypoint.R /usr/local/bin/puzzleapp-entrypoint.R

EXPOSE 8888
WORKDIR /data
ENTRYPOINT ["Rscript", "/usr/local/bin/puzzleapp-entrypoint.R"]
