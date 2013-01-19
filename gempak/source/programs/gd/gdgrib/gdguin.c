#include "gdgrib.h"

void gdguin ( GDGRIB_input *ui, int *iret )
/************************************************************************
 * gdguin								*
 *									*
 * This subroutine gets the input parameters for GDGRIB.		*
 *									*
 * gdguin ( ui, iret )							*
 *									*
 *      *ui             GDGRIB_input    User input struct               *
 *      *iret           int             Return code                     *
 **									*
 * Log:									*
 * K. Brill/HPC		 8/99						*
 * K. Brill/HPC		 2/99	Added CPYFIL				*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ier[15], len, ii;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get user input.
     */
    ip_str ( "GDFILE",  ui->gdfile, &ier[0],
        strlen("GDFILE"), sizeof(ui->gdfile) );
    ip_str ( "GFUNC",   ui->gfunc,  &ier[1],
        strlen("GFUNC"), sizeof(ui->gfunc) );
    ip_str ( "GDATTIM", ui->gdatim, &ier[2],
        strlen("GDATTIM"), sizeof(ui->gdatim) );
    ip_str ( "GLEVEL",  ui->glevel, &ier[3],
        strlen("GLEVEL"), sizeof(ui->glevel) );
    ip_str ( "GVCORD",  ui->gvcord, &ier[4],
        strlen("GVCORD"), sizeof(ui->gvcord) );
    ip_str ( "GBTBLS",  ui->gbtbls, &ier[5],
        strlen("GBTBLS"), sizeof(ui->gbtbls) );
    ip_str ( "GBFILE",  ui->gbfile, &ier[6],
        strlen("GBFILE"), sizeof(ui->gbfile) );
    ip_str ( "VERCEN",  ui->vercen, &ier[7],
        strlen("VERCEN"), sizeof(ui->vercen) );
    ip_str ( "PDSVAL",  ui->pdsval, &ier[8],
        strlen("PDSVAL"), sizeof(ui->pdsval) );
    ip_str ( "PRECSN",  ui->precsn, &ier[9],
        strlen("PRECSN"), sizeof(ui->precsn) );
    ip_str ( "WMOHDR",  ui->wmohdr, &ier[10],
        strlen("WMOHDR"), sizeof(ui->wmohdr) );
    ip_str ( "CPYFIL",  ui->cpyfil, &ier[11],
        strlen("CPYFIL"), sizeof(ui->cpyfil) );
    ip_str ( "PROJ",    ui->proj,   &ier[12],
        strlen("PROJ"), sizeof(ui->proj) );
    ip_str ( "GRDAREA", ui->gdarea, &ier[13],
        strlen("GRDAREA"), sizeof(ui->gdarea) );
    ip_str ( "KXKY",    ui->kxky,   &ier[14],
        strlen("KXKY"), sizeof(ui->kxky) );

    /*
     * Fortran string to C string.
     */
    st_null ( ui->gdfile, ui->gdfile, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gfunc , ui->gfunc , &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gdatim, ui->gdatim, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->glevel, ui->glevel, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gvcord, ui->gvcord, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gbtbls, ui->gbtbls, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gbfile, ui->gbfile, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->vercen, ui->vercen, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->pdsval, ui->pdsval, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->precsn, ui->precsn, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->wmohdr, ui->wmohdr, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->cpyfil, ui->cpyfil, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->proj  , ui->proj  , &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gdarea, ui->gdarea, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->kxky  , ui->kxky  , &len, iret, LLMXLN, LLMXLN );
    
    /*
     * Check for return code.
     */
    for ( ii = 0; ii < 15; ii++ ) {
	*iret += ier[ii];
    }
    if ( *iret != 0 ) *iret = -2;

    return;
}
