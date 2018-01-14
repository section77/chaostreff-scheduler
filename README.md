# chaostreff-scheduler

**chaostreff-scheduler** schedules Chaostreff and Tech-Events in our calendar.

It clone's the website repository and expects the event templates in the
checkout under 'templates/chaostreff-scheduler/'.

## Usage:

```
   ./chaostreff-scheduler.scm <GIT_URL> [<WORK_DIR>]

   <GIT_URL> is the URL to the 'website' src repository.
     Write access is necessary to push the changes.
     If it's on GitHub, use somethink like:
       https://<TOKEN>:@github.com/section77/website

   <WORK_DIR> is the directory where the website
     checkout lives and the files are generated.
     DEFAULT: '/website'
```

## Run it

The easiest way to run **chaostreff-scheduler** is in a docker container.
But it can also run as script, executable or per nix.

### Docker Container

#### build the container

```docker build -t chaostreff-scheduler .```

#### run it

```docker run --rm -v website:/website chaostreff-scheduler <GIT_URL>```

The volume is optional, but it saves bandwidth because the repository are updated and not cloned at each run.

### Executable / Script

```
chicken-install matchable
./chaostreff-scheduler.scm <GIT_URL> $PWD/website
```

### nix

```nix-build --attr chaostreff-scheduler release.nix && ./result/chaostreff-scheduler <GIT_URL> $PWD/website```
