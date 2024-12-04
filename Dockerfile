# =========================================================================
FROM --platform=linux/amd64 ghcr.io/r-lib/rig/ubuntu-22.04-release AS deps
COPY ./DESCRIPTION .
RUN : -- DEPENDENCIES ------------------------------------------------ && \
    R -q -e 'pak::pkg_install("deps::.", lib = .Library); pak::pak_cleanup(force = TRUE)' && \
    apt-get clean && rm -rf /tmp/* && \
    : ---------------------------------------------------------------------

# =========================================================================
FROM deps AS test-deps
RUN : -- DEV DEPENDENCIES -------------------------------------------- && \
    R -q -e 'pak::pkg_install("deps::.", lib = .Library, dependencies = TRUE)' && \
    apt-get clean && rm -rf /tmp/* && \
    : ---------------------------------------------------------------------

# =========================================================================
FROM test-deps AS test
COPY . /app
WORKDIR /app
RUN : -- TESTS ------------------------------------------------------- && \
    if [ -d tests ]; then \
      R -q -e 'testthat::test_local()'; \
      R -q -e 'covr::package_coverage()'; \
    fi && \
    : ---------------------------------------------------------------------

# =========================================================================
FROM deps AS prod
# introduce a dependency on the test stage
COPY --from=test /tmp/dumm[y] /tmp/

# copy everything (could exclude the tests)
COPY . /app

# tools neeed for prod
RUN : -- PROD TOOLS -------------------------------------------------- && \
    apt-get update && \
    apt-get install -y git rsync && \
    apt-get clean && \
    : ---------------------------------------------------------------------

# =========================================================================
FROM test-deps AS dev

RUN R -q -e 'pak::pkg_install(c("devtools", "usethis", "profvis"))'
RUN R -q -e 'pak::pkg_install("languageserver")'

RUN apt-get update && \
    apt-get install -y openssh-server && \
    apt-get clean

RUN echo PermitRootLogin yes >> /etc/ssh/sshd_config && \
    echo PermitEmptyPasswords yes >> /etc/ssh/sshd_config && \
    echo Port 2222 >> /etc/ssh/sshd_config && \
    passwd -d root

RUN git config --global --add safe.directory '/workspaces/*'
