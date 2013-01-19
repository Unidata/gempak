#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netcdf.h>

#include "profiler.h"
#include "proto_func.h"
#include "ulog.h"

extern void *decode_fsl1( int, long ,int * );
extern void *decode_fsl2( int, long, int * );
extern void *decode_rass( int, long, int * );

sta_struct *
decode_ncprof (char *infilnam, long miss, int *iret)
{
  int ier, cdfid;
  nc_type xtype;
  size_t attlen;
  char *atttext;
  sta_struct *head;

  ier = nc_open (infilnam, 0, &cdfid);
  if (ier != 0)
    {
      *iret = -1;
      uerror ("Could not open %s\0", infilnam);
      return (NULL);
    }
  udebug ("decode_ncprof %s miss %ld ier %d cdfid %d\0", infilnam, miss, ier,
	  cdfid);

  if ((ier = nc_inq_att (cdfid, NC_GLOBAL, "title", &xtype, &attlen)) == 0)
    {
      if (xtype == NC_CHAR)
	{
	  atttext = (char *) malloc (attlen + 1);
	  ier = nc_get_att_text (cdfid, NC_GLOBAL, "title", atttext);
	  if (strncmp (atttext, "RASS data", 9) == 0)
	    head = (void *) decode_rass (cdfid, miss, &ier);
	  else if (strncmp (atttext, "WPDN data", 9) == 0)
	    head = (void *) decode_fsl2 (cdfid, miss, &ier);
	  free (atttext);
	}
      else
	uerror ("unknown attribute type for GLOBAL:title %d", xtype);
    }
  else
    {
      udebug ("Old style Unidata/Wisconsin profiler file\0");
      head = (void *) decode_fsl1 (cdfid, miss, &ier);
    }

  ier = nc_close (cdfid);

  *iret = 0;
  return (head);
}
