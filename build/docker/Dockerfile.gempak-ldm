FROM unidata/ldm-docker
MAINTAINER Michael James <mjames@ucar.edu>

ENV HOME /home/ldm

WORKDIR $HOME

#
# GEMPAK dependencies
#

RUN yum install openmotif libX11 libXt libXext libXp libXft libXtst xorg-x11-xbitmaps csh -y

#
# GEMPAK binary
#

RUN rpm -ivh http://www.unidata.ucar.edu/downloads/gempak/latest/gempak-latest.el7.centos.x86_64.rpm
RUN chown -R ldm:ldm /home/gempak/GEMPAK7

#
# Create single pqact file for GEMPAK decoders
#
COPY gempakConfig.sh $HOME/bin/
RUN gempakConfig.sh

#
# Copy GEMPAK decoders to ldm/bin
#

USER root
RUN cd /home/gempak/GEMPAK7/ldm/
RUN cp /home/gempak/GEMPAK7/os/linux64/bin/dc* $HOME/bin/
RUN chown ldm:ldm $HOME/bin/dc*

#
# Full ldmd.conf
#

COPY ldmd.conf $HOME/etc/

#
# GEMPAK version replacement runldm.sh
#
COPY runldm.sh $HOME/bin/

#
# entrypoint
#

COPY ldmentry.sh /
RUN chmod +x /ldmentry.sh
ENTRYPOINT ["/ldmentry.sh"]

#
# Execute script
#

CMD ["runldm.sh"]
