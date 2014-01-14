#include "da.h"

void da_getlabel ( int *iflno, char *label, int *iret )
/************************************************************************
 * da_getlabel								*
 *									*
 * This function returns the file label.				*
 *									*
 * da_getlabel ( iflno, label, iret )					*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *									*
 * Output parameters:							*
 *	*label		char	The label string from the file		*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    *iret = 0;
    strcpy ( label, common[*iflno-1].label );
}

/*=====================================================================*/

void da_getvers ( int *iflno, int *version, int *iret )
/************************************************************************
 * da_getvers								*
 *									*
 * This function returns the file version number.			*
 *									*
 * da_getvers ( iflno, version, iret )					*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *									*
 * Output parameters:							*
 *	*version	int	The version number from the file	*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    *iret = 0;
    *version = common[*iflno-1].version;
}

/*=====================================================================*/

void da_getsorc ( int *iflno, int *type, int *source, int *iret )
/************************************************************************
 * da_getsorc								*
 *									*
 * This function returns the file type and source values.		*
 *									*
 * da_getsorc ( iflno, type, source, iret )				*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *									*
 * Output parameters:							*
 *	*type		int	The file type from the file		*
 *	*source		int	The file data source from the file	*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    *iret = 0;
    *type = common[*iflno-1].type;
    *source = common[*iflno-1].source;
}

/*=====================================================================*/

void da_getnums ( int *iflno,
		  int *rows, int *cols, int *prts, int *fhds, int *iret )
/************************************************************************
 * da_getnums								*
 *									*
 * This function returns the number of rows, number of columns, 	*
 * number of parts and number of file headers.				*
 *									*
 * da_getnums ( iflno, rows, cols, prts, fhds, iret )			*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *									*
 * Output parameters:							*
 *	*rows		int	Number of row headers			*
 *	*cols		int	Number of column headers		*
 *	*prts		int	Number of part headers			*
 *	*fhds		int	Number of file headers			*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    *iret = 0;
    *rows = common[*iflno-1].numrows;
    *cols = common[*iflno-1].numcols;
    *prts = common[*iflno-1].numparts;
    *fhds = common[*iflno-1].numfhdrs;
}

/*=====================================================================*/

void da_getname ( int *iflno, char *key, int *nkey1, int *nkey2,
       		  char *str, int *iret )
/************************************************************************
 * da_getname								*
 *									*
 * This function returns the name of the requested key.			*
 *									*
 * da_getname ( key, nkey1, nkey2, str, iret )				*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *	*key		char	Type of key to find in the information	*
 *	*nkey1		int	First key index				*
 *	*nkey2		int	Second key index			*
 *									*
 * Output parameters:							*
 *	*str		char	Name of the requested key		*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    int	ii, jj;
/*---------------------------------------------------------------------*/
    *iret = 0;
    ii = *nkey1 - 1;
    jj = *nkey2 - 1;
    if  ( strcmp(key,"ROW") == 0 )  {
	strcpy ( str, common[*iflno-1].rows[ii] );
    }
    if  ( strcmp(key,"COL") == 0 )  {
	strcpy ( str, common[*iflno-1].cols[ii] );
    }
    if  ( strcmp(key,"PART") == 0 )  {
	strcpy ( str, common[*iflno-1].parts[ii].name );
    }
    if  ( strcmp(key,"PARM") == 0 )  {
	strcpy ( str, common[*iflno-1].parts[ii].parms[jj].name );
    }
    if  ( strcmp(key,"FHDR") == 0 )  {
	strcpy ( str, common[*iflno-1].fhdrs[ii].name );
    }
}

/*=====================================================================*/

void da_getpart ( int *iflno, int *npart,
       		  int *type, int *nparm, int *iret )
/************************************************************************
 * da_getpart								*
 *									*
 * This function returns the part information.				*
 *									*
 * da_getpart ( iflno, npart, type, nparm, iret )			*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *	*npart		int	Part index number			*
 *									*
 * Output parameters:							*
 *	*type		int	Type of data packing for the part	*
 *	*nparm		int	Number of parameters in the part	*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    int	ii;
/*---------------------------------------------------------------------*/
    *iret = 0;
    ii = *npart - 1;
    *type = common[*iflno-1].parts[ii].type;
    *nparm = common[*iflno-1].parts[ii].numparms;
}

/*=====================================================================*/

void da_getparm ( int *iflno, int *npart, int *nparm,
       		  int *scale, int *offset, int *bits, int *iret )
/************************************************************************
 * da_getparm								*
 *									*
 * This function returns the information about the parameters in the	*
 * given part.								* 
 *									*
 * da_getparm ( iflno, npart, nparm, scale, offset, bits, iret )	*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *	*npart		int	Part index number			*
 *	*nparm		int	Parameter index number			*
 *									*
 * Output parameters:							*
 *	*scale		int	Scale factor for the parameter		*
 *	*offset		int	Offset for the parameter		*
 *	*bits		int	Number of bits for the parameter	*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    int	ii, jj;
/*---------------------------------------------------------------------*/
    *iret = 0;
    ii = *npart - 1;
    jj = *nparm - 1;
    *scale  = common[*iflno-1].parts[ii].parms[jj].scale;
    *offset = common[*iflno-1].parts[ii].parms[jj].offset;
    *bits   = common[*iflno-1].parts[ii].parms[jj].bits;
}

/*=====================================================================*/

void da_getfhdr ( int *iflno, int *nfhdr, int *type, int *fhlen, int *iret )
/************************************************************************
 * da_getfhdr								*
 *									*
 * This function returns the file header information.			*
 *									*
 * da_getfhdr ( iflno, nfhdr, type, fhlen, iret )			*
 *									*
 * Input parameters:							*
 * 	*iflno		int	GEMPAK file number			*
 *	*nfhdr		int	File header index number		*
 *									*
 * Output parameters:							*
 *	*type		int	Type of data packing for the header	*
 *	*fhlen		int	Length of the header			*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 6/13	Created					*
 ************************************************************************/
{
    int	ii;
/*---------------------------------------------------------------------*/
    *iret = 0;
    ii = *nfhdr - 1;
    *type  = common[*iflno-1].fhdrs[ii].type;
    *fhlen = common[*iflno-1].fhdrs[ii].length;
}
