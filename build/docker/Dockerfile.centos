FROM centos:7
MAINTAINER Michael James <mjames@ucar.edu>

# User account
RUN useradd -ms /bin/bash gempak

# Dependencies
RUN yum --enablerepo=extras install epel-release -y
RUN yum clean all
RUN yum install openmotif libX11 libXt libXext libXp libXft libXtst xorg-x11-xbitmaps csh libgfortran which python-pip git -y

# Latest RPM for CentOS 7
RUN rpm -ivh https://www.unidata.ucar.edu/downloads/gempak/latest/gempak-7.5.1-1.el7.centos.x86_64.rpm

# Python AWIPS Data Acess Framework
RUN pip install six shapely numpy
RUN git clone https://github.com/Unidata/python-awips.git
RUN cd python-awips && python setup.py install
RUN cp python-awips/awips/gempak/*.py /home/gempak/GEMPAK7/scripts/python/

# Testing script and Gemenviron
COPY rungempak.sh /home/gempak/
RUN echo ". /home/gempak/GEMPAK7/Gemenviron.profile" >> /home/gempak/.bashrc

RUN yum install bc -y 

USER gempak
WORKDIR /home/gempak
ENTRYPOINT ["/home/gempak/rungempak.sh"]
