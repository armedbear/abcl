# TODO optimize me for space
FROM easye/openjdk8

RUN export DEBIAN_FRONTEND='noninteractive' && \
    apt-get update  && \
    apt-get upgrade -y && \
    apt-get install -y \
      libffi-dev

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
RUN ls -lR ${work}/abcl

ENV java_home /usr/lib/jvm/java-8-openjdk-amd64
RUN echo "java.options=-d64  -XX:+UseG1GC -XshowSettings:vm -Dfile.encoding=UTF-8 -XX:+AggressiveOpts -XX:CompileThreshold=10" > ${work}/abcl/abcl.properties

RUN JAVA_HOME=${java_home} cd ${work}/abcl && ant clean && ant abcl
ENV abcl_exec_path  "${work}/abcl/abcl"

USER root
RUN ln -s ${abcl_exec_path} /usr/local/bin/abcl

USER abcl
CMD [ "/usr/local/bin/abcl" ]



