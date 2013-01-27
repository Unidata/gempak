/*
 * fopen a file or "-" stdin, report system errs
 * concept from K & P via  Mohamed el Lozy (ellozy@harvard)
 */

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>
#ifndef _H_FCNTL
#include <sys/fcntl.h>
#endif

extern char *progname ;

FILE *
efopen(file, mode)
char *file;
register char *mode;
{
    register FILE *iop;
    register fd, rw, oflags;

    if (strcmp(file, "-") == 0)
        return(stdin);

    rw = (mode[1] == '+');

    switch (*mode) {
    case 'a':
    	oflags = O_CREAT | (rw ? O_RDWR : O_WRONLY);
    	break;
    case 'r':
    	oflags = rw ? O_RDWR : O_RDONLY;
    	break;
    case 'w':
    	oflags = O_TRUNC | O_CREAT | (rw ? O_RDWR : O_WRONLY);
    	break;
    default:
	fprintf(stderr, "%s: efopen: invalid mode %s\n", progname, mode);
    	return (NULL);
    }

    fd = open(file, oflags, 0666);
    if (fd < 0)
		goto err ;

	iop = fdopen(fd, mode) ;
	if(iop == NULL)
		goto err ;
	return iop ;
err:
    {
	char errstr[80] ;
	sprintf(errstr,"%s: efopen: %s", progname, file) ;
	perror(errstr) ;
    	return (NULL);
    }

}
