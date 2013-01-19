/*
 * Scan the directory dirname calling filter() to make a list of selected
 * directory entries then sort using qsort and compare routine dcomp.
 * Returns the number of entries and a pointer to a list of pointers to
 * struct dirent (through namelist). Returns -1 if there were any errors.
 */
#include <dirent.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DIR_ENTRY_CHUNKS   512

int scandir ( const char *dirname, struct dirent *(*namelist[]),
		int (*filter)(const struct dirent *), int (*comparefunc)() )
/************************************************************************
 * scandir								*
 *									*
 * This function returns the contents of a directory.			*
 *									*
 * int scandir ( dirname, namelist, filter, comparefunc )		*
 *									*
 * Input parameters:							*
 *	*dirname	const char	Directory name			*
 *	**namelist[]	struct dirent	Directory contents ( OUTPUT )	*
 *	*filter()	int		Filter function			*
 *	*comparefunc()	int		Compare function		*
 *									*
 * Output parameters:							*
 *	scandir		int		Number of elements returned	*
 *									*
 **									*
 ***********************************************************************/
{
	struct dirent *d, *p, **names;
	int nitems;
	long arraysz;
	DIR *dirp;
	int	dirent_size;

	if ((dirp = opendir(dirname)) == NULL)
		return(-1);

/*
 * 	Allocate enough for DIR_ENTRY_CHUNKS dirents.  Will grow if 
 *	necessary.
 */
	arraysz = DIR_ENTRY_CHUNKS;
	names = 
	    (struct dirent **)malloc(arraysz * sizeof(struct dirent *));
	if (names == NULL)
		return(-1);


/*
 *
 *	opendir/readdir/closedir are defined in POSIX, but apparently,
 *	the exact definition of the dirent data structure is vague.
 *
 *	Solaris declares that in order to allocate enough memory for 
 *	a dirent structure returned by readdir() use: 
 *
 *		sizeof(struct dirent) + _POSIX_PATH_MAX 
 *
 *	The Solaris 2 definition for struct dirent only allocates 1 
 *	byte in the d_name[1] member as a place holder and the actual 
 *	amount is dynamically allocated (but there is no mention of
 *      how to do this exactly).
 *   
 *	However.  HP's and SunOS declare the dirent structure fully 
 *	allocated with d_name[_POSIX_PATH_MAX + 1].  
 *
 *      The safe, generic way to allocate this structure would be the
 *	Solaris way - but on machines like the HP and SunOS that would
 *	allocate twice as much memory as required for each entry (ie.
 *	512 bytes vs 255).
 *
 *	The solution implemented here is at runtime, to  perform
 *	a simple check (hopefully not too simple) to see if the
 *	d_name member of dirent is at least as big as the path max
 *	defined in Posix.  If it is, then the assume that the dirent
 *	structure has defined d_name as the maximum that it can be and
 *	the sizeof(struct dirent) is sufficient to hold any dirent 
 *	returned by readdir().  Otherwise, set dirent_size use the
 *	Solaris formula (plus 1):
 *
 *		sizeof(struct dirent) + _POSIX_PATH_MAX + 1
 *
 */

	dirent_size = sizeof( struct dirent ) + _POSIX_PATH_MAX + 1;
	if ( _POSIX_PATH_MAX <= sizeof ( d->d_name) ) 
		dirent_size = sizeof( struct dirent );

	nitems = 0;
	while ((d = readdir(dirp)) != NULL) {

/*
 *		If a filter function was supplied, call it.  If
 *		filter returns doesn't return true, then skip this
 *		entry.
 */
		if (filter != NULL && !(*filter)(d))
			continue;

/*
 * 		Get some memory to create a copy of the data.
 */
		p = (struct dirent *) malloc( dirent_size );

		if (p == NULL)
			return(-1);

/*
 *		Copy all the data bytes from one structure to the
 *		other.
 */
		memcpy ( p, d, sizeof(*d));

/*
 *		Re-copy just the name.  Some OS's just declare the d_name
 *		as a char d_name[1], but allocate enough space to fit
 *		the entire string.  In the worst case, the entire string
 *		will be copied twice.
 */
		strcpy ( p->d_name, d->d_name);

/*
 *		Check to make sure the array has space left and
 *		if not, realloc another chunk.
 */
		if (++nitems >= arraysz) {
			arraysz +=  DIR_ENTRY_CHUNKS;
			names = (struct dirent **) realloc( (char *)names,
					arraysz * sizeof(struct dirent *) );
			if (names == NULL)
				return(-1);
		}
		names[nitems-1] = p;
	}

	closedir(dirp);

/*
 *	If we got something then optionally sort it.
 */
	if (nitems && comparefunc != NULL)
		qsort(names, nitems, sizeof(struct dirent *), comparefunc);

	*namelist = names;
	return(nitems);

}
