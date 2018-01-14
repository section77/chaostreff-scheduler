#
# builder
#
FROM alpine as builder

RUN apk add --no-cache chicken

RUN chicken-install -retrieve matchable > /dev/null
RUN cd matchable && csc -unit matchable -emit-import-library matchable -c matchable.scm -o matchable.o

COPY . /work
WORKDIR /work

RUN csc -uses matchable -I /matchable -c chaostreff-scheduler.scm -o chaostreff-scheduler.o
RUN csc -static chaostreff-scheduler.o /matchable/matchable.o -o chaostreff-scheduler


#
# runtime container
#
FROM alpine

RUN apk add --no-cache git

RUN git config --global user.email "chaostreff-scheduler@j-keck.net"
RUN git config --global user.name  "section77/chaostreff-scheduler"

COPY --from=builder /work/chaostreff-scheduler /chaostreff-scheduler

ENTRYPOINT ["/chaostreff-scheduler"]
