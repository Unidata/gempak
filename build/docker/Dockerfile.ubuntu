FROM ubuntu:latest
MAINTAINER Michael James <mjames@ucar.edu>

# User account
RUN useradd -ms /bin/bash gempak

# Dependencies
RUN apt-get update -y 
RUN apt-get install build-essential gfortran git gcc libtool bc g++ libx11-dev libxt-dev libxext-dev libxft-dev libxtst-dev flex byacc libmotif-dev libxml2-dev libxslt-dev libz-dev autoconf -y
RUN apt-get install wget python-pip python-dev build-essential -y
RUN pip install --upgrade pip 

# Latest source code
RUN wget https://www.unidata.ucar.edu/downloads/gempak/latest/gempak-7.5.1.deb
RUN dpkg -i gempak-7.5.1.deb

# Python AWIPS Data Acess Framework
RUN pip install six shapely numpy
RUN git clone https://github.com/Unidata/python-awips.git
RUN cd python-awips && python setup.py install
RUN cp python-awips/awips/gempak/*.py /home/gempak/GEMPAK7/scripts/python/

# Testing script and Gemenviron
COPY rungempak.sh /home/gempak/
RUN echo ". /home/gempak/GEMPAK7/Gemenviron.profile" >> /home/gempak/.bashrc
RUN chown -R gempak:gempak /home/gempak/GEMPAK7
USER gempak

WORKDIR /home/gempak
ENTRYPOINT ["/home/gempak/rungempak.sh"]
