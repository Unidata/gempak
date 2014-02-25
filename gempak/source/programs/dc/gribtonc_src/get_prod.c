/*
 *   Copyright 1996 University Corporation for Atmospheric Research
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: get_prod.c,v 1.10 1996/06/20 16:26:17 russ Exp $ */

/*
 * get_prod gets a GRIB product from a stream, which may be a pipe and
 * hence not seekable.
 * 
 * A GRIB product begins with 'GRIBnnn', where the 'nnn' bytes encode the
 * length of the product, including the terminating '7777' characters.  So
 * it seems we could just decode the nnn and read that far ahead.  But
 * since bogus 'GRIB' characters could occur in the middle of binary data,
 * this might send us looking 16 Mbytes ahead for a '7777' that won't be
 * found, and we can't seek backwards on the input, since it may be a pipe.
 * Hence to avoid losing products when this (unlikely) event happens, we
 * take a different approach, parsing the input by looking for 'GRIB' and
 * '7777' strings using a finite-state automaton that only reads at most
 * one byte ahead.
 */

#include <sys/types.h>
#include <string.h>		/* memset() ... */
#include <sys/time.h>
#ifdef _AIX
#include <sys/select.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "ulog.h"
#include "gribtypes.h"
#include "gds.h"
#include "gdes.h"
#include "gbytem.h"
#include "grib1.h"		/* for MAX_GRIB_SIZE */
#include "get_prod.h"
#include "emalloc.h"

#ifdef __STDC__
static char* new_prod_id(u_char * buf, u_char * sp);
static void init_char_class(void);
static enum PROD_MARK look_for_mark(FILE * stream, unsigned char** ptr, unsigned char* last, int *start_state);
#endif

#define WMO_HEADER_DEFAULT "header not found"

/*
 * Extract WMO header, if any, from product.
 * If GRIB product not in a WMO envelope, just manufacture a unique string for
 * a header by using a sequence number.
 */
static char*
new_prod_id(buf, sp)
    u_char *buf;
    u_char *sp;
{
    static long seqno = 0;	/* count used in manufactured headers */
    char *ret;

    seqno++;

				/* Fill in id field */
    if (sp > buf + 27 && *(sp-1) == 0xa &&
	*(sp-2) == 0xd && *(sp-3) == 0xd) {
	/* in a WMO envelope, so extract WMO header*/
	u_char *r1 = sp-3; /* after last character of WMO header */
	u_char *r0 = r1-1;
	while (*r0 != 0xa && r0 > buf)	/* find header start */
	    r0--;
	if (r0 > buf) { /* header starts at r0+1 */
	    ret = (char *)emalloc((r1-r0));
	    strncpy(ret, (char *)r0 + 1, (r1-r0)-1);
	    ret[(r1-r0)-1] = '\0';	/* null terminate */
	} else {		/* didn't find start of header */
	    ret = emalloc(strlen(WMO_HEADER_DEFAULT)+1);
	    strcpy(ret, WMO_HEADER_DEFAULT);
	}
    } else {			/* manufacture a product ID for header */
	char tmp[25];
	sprintf(tmp, "%ld", seqno);
	ret = (char *) emalloc(strlen(tmp)+1);
	strcpy(ret, tmp);
    }
    return ret;
}




/* character classes used in WMO message finite state automaton (FSA) */
#define CL_G		0
#define CL_R		1
#define CL_I		2
#define CL_B		3
#define CL_7		4
#define CL_OTHER	5

/* mapping from input bytes to character classes */
static int char_class[256];
static int char_class_initialized = 0;

/* states in WMO message FSA */
#define ST_		0	/* ground state */
#define ST_G		6
#define ST_GR		12
#define ST_GRI		18
#define ST_GRIB		24
#define ST_GRIBn	30
#define ST_GRIBnn	36
#define ST_7		42
#define ST_77		48
#define ST_777		54
#define ST_7777		60      /* found possible end */
#define ST_GRIBnnn	66	/* found possible beginning */

/*
 * FSA state transition table to simultaneously look for GRIB start of message
 * ('GRIB'nnn) and GRIB end of message ('7777').  If FSA is in state i
 * in comment at right of row and the next input character is in the class j
 * in comment at top of column, then the new state is fsa[i+j], which is in
 * the row i and column j.
 */
static int fsa[] = {
  /* 'G'         'R'         'I'         'B'         '7'         OTHER                   */
    ST_G,       ST_,        ST_,        ST_,        ST_7,       ST_,        /* ST_       */
    ST_G,       ST_GR,      ST_,        ST_,        ST_7,       ST_,        /* ST_G      */
    ST_G,       ST_,        ST_GRI,     ST_,        ST_7,       ST_,        /* ST_GR     */
    ST_G,       ST_,        ST_,        ST_GRIB,    ST_7,       ST_,        /* ST_GRI    */
    ST_GRIBn,   ST_GRIBn,   ST_GRIBn,   ST_GRIBn,   ST_GRIBn,   ST_GRIBn,   /* ST_GRIB   */
    ST_GRIBnn,  ST_GRIBnn,  ST_GRIBnn,  ST_GRIBnn,  ST_GRIBnn,  ST_GRIBnn,  /* ST_GRIBn  */
    ST_GRIBnnn, ST_GRIBnnn, ST_GRIBnnn, ST_GRIBnnn, ST_GRIBnnn, ST_GRIBnnn, /* ST_GRIBnn */
    ST_G,    	ST_,   	    ST_,     	ST_,        ST_77,   	ST_, 	    /* ST_7      */
    ST_G,    	ST_,   	    ST_,     	ST_,        ST_777,  	ST_, 	    /* ST_77     */
    ST_G,    	ST_,   	    ST_,     	ST_,        ST_7777, 	ST_, 	    /* ST_777    */
};

/* Initialize character class mapping table */
static void
init_char_class()
{
    int i = 255;
    while (i--)
      char_class[i] = CL_OTHER;
    char_class['G'] = CL_G;
    char_class['R'] = CL_R;
    char_class['I'] = CL_I;
    char_class['B'] = CL_B;    
    char_class['7'] = CL_7;    
}


/*
 * Look for product delimiter on the specified stream, while capturing product
 * in buffer.  Returns
 *
 * 	READ_ERR	if error reading stream
 * 	FOUND_START	if possible start of product found (GRIBnnn)
 * 	FOUND_END	if possible end of product found (7777)
 * 	FOUND_EOF	if EOF encountered searching for product
 * 	NOT_FOUND	if buffer is full but neither start nor end of product
 *			is seen 
 *  stream	open stdio stream from which input is read.
 *  ptr		on input, points to the beginning of the buffer where input
 *		is copied.  On return, points after last character copied.
 *  last	points to the end of the buffer where input is copied
 *
 * If no product delimiter is found before buffer is full, returns NOT_FOUND.
 */
static enum PROD_MARK
look_for_mark(stream, ptr, last, start_state)
     FILE * stream;
     unsigned char **ptr;
     unsigned char *last;
     int *start_state;
{
    int c;
    int state;
    unsigned char *cp = *ptr;

    /* int state = ST_;		start in ground state */
    state = *start_state;
    *start_state = ST_;

    if (!char_class_initialized) {
	init_char_class();
	char_class_initialized = 1;
    }
    
    while ((c = getc(stream)) != EOF) {
	*cp++ = c;
	if (cp > last) {
	    *ptr = cp;
	    return NOT_FOUND;
	}
	state = fsa[state + char_class[c]]; /* transition to new state */
	switch (state) {
	  case ST_GRIBnnn:
	    *ptr = cp;
	    return FOUND_START;
	  case ST_7777:
	    *ptr = cp;
	    return FOUND_END;
	  default:
	    break;
	}
    }
    if (feof(stream)) {
	*ptr = cp;
	if (state == ST_7777) {
	    return FOUND_END;
	}
	return FOUND_EOF;
    }
    /* else */
    clearerr(stream);
    *ptr = cp;
    return READ_ERR;
}


/*
 * Get a GRIB product from stdin.  Exit gracefully if timeout seconds have
 * elapsed without input available on stdin.
 */
int
get_prod (stream, timeout, prodp)
    FILE *stream;
    int timeout;
    prod* prodp;
{
    static unsigned char buf[2*MAX_GRIB_SIZE];
    static unsigned char *start;
    int ifd = fileno(stream) ;	/* input file descriptor */
    int width = (1 << ifd) ;
    int ready ;
    fd_set readfds ;
    struct timeval timeo ;	/* timeout for read */
    static int in_product = 0;	/* true when scanning for end of product */
    unsigned char *cur = buf;	/* current position in buffer */
    unsigned char *bufend = &buf[2*MAX_GRIB_SIZE-1]; /* last place in buffer */

    while(1) {
	FD_ZERO(&readfds) ;
	FD_SET(ifd, &readfds) ;
	timeo.tv_sec = timeout ;
	timeo.tv_usec = 0 ;
#ifndef NO_SELECT
	ready = select(width, &readfds, 0, 0, &timeo) ;
#else
	ready = 1;
#endif	  

	if(ready < 0 ) {
	    if(errno == EINTR) { /* so will resume select, in case Sys V */
		errno = 0 ;
		continue ;
	    }
	    serror("select") ;
	    exit(8) ;
	}
	/* else */
	if(ready > 0) {
	    if(FD_ISSET(ifd, &readfds)) {
		/*
		 * If not in product, find start-of product.
		 * If in product, find end-of product.
		 * If EOF seen, exit gracefully
		 */
		int total_len;	/* length of product according to GRIB message */
                int start_state;
		
		if (!in_product) {
		    cur = buf;
                    start_state = ST_;
		    switch(look_for_mark(stream, &cur, bufend, &start_state)) {
		      case FOUND_END:
				/* '7777' seen, keep looking */
			continue;
		      case FOUND_START:
			start = cur - 7; /* position before "GRIBnnn" */
			in_product = 1;
			prodp->id = new_prod_id(buf, start); /* WMO header */
			total_len = g3i((cur - 3)); /* decode nnn */
			break;
		      case FOUND_EOF:
			break;
		      default:
			uinfo("error reading GRIB product");
			return 0;
		    }
		}
		if (in_product) {
		    int eof_found = 0;
                    start_state = ST_;
		    while (!eof_found) {
			switch(look_for_mark(stream, &cur, bufend, &start_state)) {
			case FOUND_START:
			    /* 'GRIB'nnn inside product assumed to be data */
			    break; /* keep reading */
			case FOUND_END:
			    if (cur - start == total_len) {
				prodp->len = cur - start;
				prodp->bytes = (unsigned char *) start;
				in_product = 0;
				return prodp->len;
			    } else if (cur - start < total_len) {
                                start_state = ST_777;
				break; /* bogus end in data, keep reading */
			    } else { /* past where end should have been */
				uinfo("oversize GRIB product (%d bytes to long) %s",
                                   (cur - start) - total_len,prodp->id);
				in_product = 0;
				if(prodp->id)
				    free(prodp->id);
				return 0;
			    }
			case NOT_FOUND:
			    uinfo("oversize GRIB product 2");
			    in_product = 0;
			    if(prodp->id)
				free(prodp->id);
			    return 0;
			case FOUND_EOF:
			    eof_found = 1;
			    break;
			default:
			    uinfo("error reading GRIB product");
			    if(prodp->id)
				free(prodp->id);
			    in_product = 0;
			    return 0;
			}
		    }
		}
		/* We only get here if EOF read, so exit gracefully  */
		uinfo("EOF on input");
		exit(0);
	    }
	    continue ;
	}
	/* else ready == 0, timeout expired, exit gracefully */
	uinfo("timeout expired, exiting");
	exit(0);
    }
    /*NOTREACHED*/
}
