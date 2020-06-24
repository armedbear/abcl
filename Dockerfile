# TODO optimize me for space
FROM openjdk:11

RUN export DEBIAN_FRONTEND='noninteractive' && \
    apt-get update  && \
    apt-get upgrade -y && \
    apt-get install -y \
      libffi-dev ant maven

USER root
RUN useradd -ms /bin/bash abcl
USER abcl

ENV work        /home/abcl/work
RUN mkdir -p 	${work}

WORKDIR ${work}
COPY . ${work}/abcl
USER root
RUN chown -R abcl:abcl ${work}
USER abcl

# Diagnostics for debugging ABCL construction
#RUN ls -lR ${work}/abcl

RUN cd ${work}/abcl && bash ci/create-abcl-properties.bash openjdk11

RUN cd ${work}/abcl && ant clean && ant abcl
ENV abcl_exec_path  "${work}/abcl/abcl"

USER root
RUN ln -s ${abcl_exec_path} /usr/local/bin/abcl

USER abcl
ENTRYPOINT [ "/usr/local/bin/abcl" ]



