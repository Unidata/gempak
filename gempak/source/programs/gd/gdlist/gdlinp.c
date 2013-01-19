#include "gdlist.h"

void gdlinp ( GDLIST_input *ui, int *iret )
/************************************************************************
 * gdlinp								*
 *									*
 * This subroutine gets the input parameters for GDLIST from the TAE.	*
 *									*
 * gdlinp ( UI, IRET )							*
 *                                                                      *
 * Output parameters:                                                   *
 *      *ui             GDLIST_input    User input struct               *
 *      *iret           int             Return code                     *
 **									*
 * Log:									*
 * M. desJardins/GSFC	 2/85						*
 * M. Goodman/RDS	11/85	Replaced GPARM with GFUNC		*
 * M. Goodman/RDS	11/85	Added SCALE				*
 * M. desJardins/GSFC	 6/88	Replaced area with garea & proj		*
 * R. Tian/SAIC          9/06   Recoded from Fortran                    *
 ************************************************************************/
{
    int ier1, ier2, ier3, ier4, ier5, ier6, ier7, ier8, ier9, len;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get user input.
     */
    ip_str ( "GDFILE",  ui->gdfile, &ier1,
        strlen("GDFILE"), sizeof(ui->gdfile) );
    ip_str ( "GDATTIM", ui->gdatim, &ier2,
        strlen("GDATTIM"), sizeof(ui->gdatim) );
    ip_str ( "GLEVEL",  ui->glevel, &ier3,
        strlen("GLEVEL"), sizeof(ui->glevel) );
    ip_str ( "GVCORD",  ui->gvcord, &ier4,
        strlen("GVCORD"), sizeof(ui->gvcord) );
    ip_str ( "GFUNC",   ui->gfunc,  &ier5,
        strlen("GFUNC"), sizeof(ui->gfunc) );
    ip_str ( "GAREA",   ui->garea,  &ier6,
        strlen("GAREA"), sizeof(ui->garea) );
    ip_str ( "PROJ",    ui->proj,   &ier7,
        strlen("PROJ"), sizeof(ui->proj) );
    ip_str ( "SCALE",   ui->scale,  &ier8,
        strlen("SCALE"), sizeof(ui->scale) );
    ip_str ( "OUTPUT",  ui->output, &ier9,
        strlen("OUTPUT"), sizeof(ui->output) );

    /*
     * Fortran string to C string.
     */
    st_null ( ui->gdfile, ui->gdfile, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gdatim, ui->gdatim, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->glevel, ui->glevel, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gvcord, ui->gvcord, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gfunc , ui->gfunc , &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->garea , ui->garea , &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->proj  , ui->proj  , &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->scale , ui->scale , &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->output, ui->output, &len, iret, LLMXLN, LLMXLN );

    /*
     * Check for return code.
     */
    *iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 + ier9;
    if ( *iret != 0 ) *iret = -2;

    return;
}
