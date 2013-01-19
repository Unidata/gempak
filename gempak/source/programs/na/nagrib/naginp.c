#include "nagrib.h"

void naginp ( NAGRIB_input *ui, int *iret )
/************************************************************************
 * naginp								*
 *									*
 * This subroutine gets the input parameters for NAGRIB.		*
 *									*
 * NAGINP ( UI, IRET )							*
 *									*
 * Output parameters:                                                   *
 *	*ui		NAGIRB_input	User input struct		*
 *      *iret           int             Return code                     *
 **									*
 * Log:									*
 * S. Jacobs/EAI	 7/93						*
 * K. Brill/NMC		 4/95	Added GAREA				*
 * D.W.Plummer/NCEP	 2/96   Added GBTBLS, GBDIAG			*
 * D.W.Plummer/NCEP	 5/01   Added PDSEXT				*
 * m.gamazaychikov/SAIC	09/05	Added OVERWR				*
 * R. Tian/SAIC		 8/06	Recoded from Fortran			*
 ************************************************************************/
{
    int ier1, ier2, ier3, ier4, ier5, ier6, ier7, ier8, ier9, ier10,
        ier11, ier12, ier13, ier14, len;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Get user input.
     */
    ip_str  ( "GBFILE",  ui->gbfile,  &ier1,
              strlen("GBFILE"), sizeof(ui->gbfile) );
    ip_str  ( "INDXFL",  ui->indxfl,  &ier2,
              strlen("INDXFL"), sizeof(ui->indxfl) );
    ip_str  ( "GDOUTF",  ui->gdoutf,  &ier3,
              strlen("GDOUTF"), sizeof(ui->gdoutf) );
    ip_str  ( "PROJ",    ui->proj,    &ier4, 
              strlen("PROJ"), sizeof(ui->proj) );
    ip_str  ( "GRDAREA", ui->grdarea, &ier5, 
              strlen("GRDAREA"), sizeof(ui->grdarea) );
    ip_str  ( "KXKY",    ui->kxky,    &ier6, 
              strlen("KXKY"), sizeof(ui->kxky) );
    ip_str  ( "MAXGRD",  ui->maxgrd,  &ier7, 
              strlen("MAXGRD"), sizeof(ui->maxgrd) );
    ip_str  ( "CPYFIL",  ui->cpyfil,  &ier8, 
              strlen("CPYFIL"), sizeof(ui->cpyfil) );
    ip_str  ( "GAREA",   ui->garea,   &ier9, 
              strlen("GAREA"), sizeof(ui->garea) );
    ip_str  ( "OUTPUT",  ui->output,  &ier10,
              strlen("OUTPUT"), sizeof(ui->output) );
    ip_str  ( "GBTBLS",  ui->gbtbls,  &ier11, 
              strlen("GBTBLS"), sizeof(ui->gbtbls) );
    ip_str  ( "GBDIAG",  ui->gbdiag,  &ier12,
              strlen("GBDIAG"), sizeof(ui->gbdiag) );
    ip_log  ( "PDSEXT",  &(ui->pdsext),  &ier13, strlen("PDSEXT") );
    ip_log  ( "OVERWR",  &(ui->overwr),  &ier14, strlen("OVERWR") );

    /*
     * Fortran string to C string.
     */
    st_null ( ui->gbfile, ui->gbfile, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->indxfl, ui->indxfl, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gdoutf, ui->gdoutf, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->proj,   ui->proj,   &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->grdarea, ui->grdarea, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->kxky,   ui->kxky,   &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->maxgrd, ui->maxgrd, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->cpyfil, ui->cpyfil, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->garea,  ui->garea,  &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->output, ui->output, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gbtbls, ui->gbtbls, &len, iret, LLMXLN, LLMXLN );
    st_null ( ui->gbdiag, ui->gbdiag, &len, iret, LLMXLN, LLMXLN );

    /*
     * Check for return code.
     */
    *iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 +
            ier8 + ier9 + ier10 + ier11 + ier12 + ier13 + ier14;
    if ( *iret != 0 ) *iret = -2;

    return;
}    
