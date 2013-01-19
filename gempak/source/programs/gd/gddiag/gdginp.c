#include "gddiag.h"

void gdginp ( GDDIAG_input *ui, int *iret )
/************************************************************************
 * gdginp								*
 *									*
 * This subroutine gets the input parameters for GDDIAG.		*
 *									*
 * gdginp ( ui, iret )							* 
 **									*
 * Log:									*
 * M. Goodman/RDS	10/85						*
 * M. desJardins/GSFC	 8/88						*
 * M. Li/SAIC		04/04	Added grdhdr				*
 * R. Tian/SAIC		 1/05	Added parameters for creating file	*
 * R. Tian/SAIC		 9/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ier1, ier2, ier3, ier4, ier5, ier6, ier7, ier8, ier9, ier10,
        ier11, ier12, ier13, ier14, ier15, ier16, len;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get user input.
     */
    ip_str  ( "GDFILE",  ui->gdfile, &ier1,
        strlen("GDFILE"), sizeof(ui->gdfile) );
    ip_str  ( "GDOUTF",  ui->gdoutf, &ier2,
        strlen("GDOUTF"), sizeof(ui->gdoutf) );
    ip_str  ( "GDATTIM", ui->gdatim, &ier3,
        strlen("GDATTIM"), sizeof(ui->gdatim) );
    ip_str  ( "GLEVEL",  ui->glevel, &ier4,
        strlen("GLEVEL"), sizeof(ui->glevel) );
    ip_str  ( "GVCORD",  ui->gvcord, &ier5,
        strlen("GVCORD"), sizeof(ui->gvcord) );
    ip_str  ( "GFUNC",   ui->gfunc,  &ier6,
        strlen("GFUNC"), sizeof(ui->gfunc) );
    ip_str  ( "GRDNAM",  ui->grdnam, &ier7,
        strlen("GRDNAM"), sizeof(ui->grdnam) );
    ip_str  ( "GRDTYP",  ui->grdtyp, &ier8,
        strlen("GRDTYP"), sizeof(ui->grdtyp) );
    ip_str  ( "GPACK",   ui->gpack,  &ier9,
        strlen("GPACK"), sizeof(ui->gpack) );
    ip_str  ( "GRDHDR",  ui->grdhdr, &ier10,
        strlen("GRDHDR"), sizeof(ui->grdhdr) );
    ip_str  ( "PROJ",    ui->proj,   &ier11,
        strlen("PROJ"), sizeof(ui->proj) );
    ip_str  ( "GRDAREA", ui->gdarea, &ier12,
        strlen("GRDAREA"), sizeof(ui->gdarea) );
    ip_str  ( "KXKY",    ui->kxky,   &ier13,
        strlen("KXKY"), sizeof(ui->kxky) );
    ip_str  ( "MAXGRD",  ui->maxgrd, &ier14,
        strlen("MAXGRD"), sizeof(ui->maxgrd) );
    ip_str  ( "CPYFIL",  ui->cpyfil, &ier15,
        strlen("CPYFIL"), sizeof(ui->cpyfil) );
    ip_str  ( "ANLYSS",  ui->anlyss, &ier16,
        strlen("ANLYSS"), sizeof(ui->anlyss) );

    /*
     * Fortran string to C string.
     */
    st_null ( ui->gdfile, ui->gdfile, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gdoutf, ui->gdoutf, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gdatim, ui->gdatim, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->glevel, ui->glevel, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gvcord, ui->gvcord, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gfunc,  ui->gfunc,  &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->grdnam, ui->grdnam, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->grdtyp, ui->grdtyp, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gpack,  ui->gpack,  &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->grdhdr, ui->grdhdr, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->proj,   ui->proj,   &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gdarea, ui->gdarea, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->kxky,   ui->kxky,   &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->maxgrd, ui->maxgrd, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->cpyfil, ui->cpyfil, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->anlyss, ui->anlyss, &len, iret, LLMXLN, LLMXLN );

    /*
     * Check for return code.
     */
    *iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8 + 
            ier9 + ier10 + ier11 + ier12 + ier13 + ier14 + ier15 +
            ier16;
    if ( *iret != 0 ) *iret = -2;

    return;
}
