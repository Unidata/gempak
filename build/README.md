# Unidata GEMPAK Developer Notes

## Building the **gempak-devel** Docker Image

* https://hub.docker.com/r/unidata/gempak-devel/tags/

We create and use the Docker image `unidata/gempak-devel` to build the various RHEL binary distributions of GEMPAK. The directory **$NAWIPS/build/gempak-devel/** contains Dockerfiles and a build script (**docker_build.sh**) to create the GEMPAK development environment images used in the next steps.

* **Dockerfile.centos6** is used to create `unidata/gempak-devel:cento6`
* **Dockerfile.centos7** is used to create `unidata/gempak-devel:cento7`
* **Dockerfile.fedora** is used to create `unidata/gempak-devel:fedora`
* **Dockerfile.ubuntu** is used to create `unidata/gempak-devel:ubuntu`

## Building GEMPAK RPMs for RHEL/Fedora

### Building External Libraries (gempak-extlibs*.rpm)

Packages in **$NAWIPS/extlibs** generally do not change between GEMPAK releases, they take a long time to compile, and some (or most) should probably be off-loaded to OS-managed dependencies.

`$NAWIPS/build/Installer.gempak/extlibs.spec` is invoked by the script `$NAWIPS/build/extlibs/setup_extlibs.sh ${OS_TYPE} ${OS_VERSION}`

`$NAWIPS/build/Installer.gempak/gempak.spec` is invoked by the script `$NAWIPS/build/setup.sh ${OS_TYPE} ${OS_VERSION}` to build the full GEMPAK RPM.

    cd $NAWIPS
    ./build/extlibs/setup_extlibs.sh centos 7
    ./build/extlibs/setup_extlibs.sh centos 6
    ./build/extlibs/setup_extlibs.sh fedora latest

### Building GEMPAK RPMs

    cd $NAWIPS
    ./build/setup.sh centos 7
    ./build/setup.sh centos 6
    ./build/setup.sh fedora latest


## GEMPAK Ubuntu Linux Binary (gempak*.deb)

    cd $NAWIPS
    ./build/setup.sh ubuntu latest

Binary packages are written to the local directory **$NAWIPS/build/dist/**

## GEMPAK Docker Image



## CloudGEMPAK

* https://github.com/Unidata/CloudGEMPAK
* https://hub.docker.com/r/unidata/cloudgempak/

CloudGEMPAK will install the latest published CentOS 7 GEMPAK RPM package from `www:/content/downloads/gempak/latest/`

    git clone https://github.com/Unidata/CloudGEMPAK.git
    cd CloudGEMPAK
    make build
    docker push unidata/cloudgempak

