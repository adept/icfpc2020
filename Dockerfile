FROM icfpcontest2020/ocaml

WORKDIR /solution
COPY --chown=opam . .
RUN chmod +x ./build.sh
RUN chmod +x ./run.sh
RUN ./build.sh
ENTRYPOINT ["./run.sh"]
