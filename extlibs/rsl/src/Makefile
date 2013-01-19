include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(RSLLIB)

COBJS = \
	$(ALIB)(cappi.o) \
	$(ALIB)(carpi.o) \
	$(ALIB)(cube.o) \
	$(ALIB)(endian.o) \
	$(ALIB)(gzip.o) \
	$(ALIB)(image_gen.o) \
	$(ALIB)(interp.o) \
	$(ALIB)(prune.o) \
	$(ALIB)(radar.o) \
	$(ALIB)(range.o) \
	$(ALIB)(ray_indexes.o) \
	$(ALIB)(sort_rays.o) \
	$(ALIB)(uf_to_radar.o) \
	$(ALIB)(volume.o) \
	$(ALIB)(wsr88d.o) \
	$(ALIB)(wsr88d_get_site.o) \
	$(ALIB)(wsr88d_to_radar.o)

#CFLAGS = $(COPT) -I../include -I../../bzip2
CFLAGS = $(COPT) -I../include

all : $(ALIB)

$(ALIB): $(COBJS)
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

clean:
	$(RM) *.o
