#include "dg.h"

void dg_driv ( const int *itype, int *iret )
/************************************************************************
 * dg_driv								*
 *									*
 * This subroutine serves as the driver to compute a grid diagnostic	*
 * quantity using the parsed table stored in common.			*
 *									*
 * dg_driv ( itype, iret )						*
 *									*
 * Input parameters:							*
 *	*itype		const int	Output type			*
 *					  1 = scalar			*
 *					  2 = vector			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -3 = GFUNC is blank		*
 *					 -4 = output grid not scalar	*
 *					 -5 = output grid not vector	*
 *					 -6 = wrong number of operands	*
 *					 -7 = grid cannot be found	*
 *					 -8 = grid is the wrong size	*
 *					 -9 = calling sequence error	*
 *					-10 = internal grid list is full*
 *					-11 = must be a vector		*
 *					-12 = must be a scalar		*
 *					-13 = operand must be in file	*
 *					-14 = DG_INIT not called	*
 *					-15 = polar center not valid	*
 *					-16 = map projection invalid	*
 *					-17 = LEVEL must be a layer	*
 *					-18 = TIME must be a range	*
 *					-19 = operator not recognized	*
 *					-20 = stack is full		*
 *					-21 = stack is empty		*
 *					-22 = TIME is invalid		*
 *					-23 = LEVEL is invalid		*
 *					-24 = IVCORD is invalid		*
 *					-26 = layer of layers invalid	*
 *					-27 = time range layer invalid	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * M. desJardins/GSFC	 7/88	Added icpntr, DG_GETS,...		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * G. Huffman/GSC	 4/89	Added OLDF				*
 * M. desJardins/GSFC	 4/89	Changed north-relative vector names	*
 * K. Brill/GSC		 7/89	Added TANG, NORM, TANGV, and NORMV	*
 * K. Brill/GSC		 9/89	Reordered functions in case sttmnt	*
 * M. desJardins/GSFC	 4/90	Removed OLDF				*
 * K. Brill/NMC		 9/90	Added CIRC				*
 * K. Brill/NMC		10/90	Added SAVG, VAVG, SAVS, VAVS		*
 * J. Whistler/SSAI	 3/91	Added SQUO				*
 * J. Whistler/SSAI	 6/91	Added RAD, TNG, MRAD, MTNG		*
 * S. Jacobs/SSAI	 9/91	Added MSFC				*
 * K. Brill/NMC		10/92	Added CROS and KCRS			*
 * L. Sager/NMC		06/93	Added DVDX, DVDY, dropped DVOBS		*
 * S. Jacobs/EAI	 7/93	Added HIGH, LOWS			*
 * M. desJardins/NMC	 8/93	Eliminate duplicate names		*
 * S. Jacobs/EAI	10/93	Added POIS				*
 * S. Jacobs/NMC	 4/94	Added QVEC and QVCL			*
 * L. Williams/EAI	 8/94	Added call to DF_TMWK and DF_SM9S	*
 * D. McCann/NSSFC	11/94	Added THES, EPV, BVSQ, and WSHR		*
 * K. Brill/NMC		 2/95	Added call to DF_GWFS and DV_GWFV	*
 * J. Whistler/NSSFC	 7/95	Added TMST				*
 * D. McCann/NSSFC	 8/95	Added WNDX				*
 * T. Lee/GSC		 4/96	Removed DV_EPV				*
 * S. Maxwell/GSC	 8/97	Added < and > functions			*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * T. Lee/GSC		11/97	Added THWC				*
 * D.W.Plummer/NCEP	 7/98	Added DF_MASK and DV_VMSK		*
 * D. McCann/AWC	 4/01	Added DV_DIVT				*
 * A. Hardy/SAIC	10/01   Added DF_BOOL				*
 * K. Brill/HPC		12/01	Call DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		10/02	Added DF_NCDF				*
 * S. Chiswell/Unidata	 6/02	Added DF_MISS				*
 * K. Brill/HPC		11/02	Remove DF_SAVG & DV_VAVG calls		*
 * K. Brill/HPC		 1/03	Added DF_PAUB, DF_SMAX, DF_SMIN		*
 * K. Brill/HPC		 2/03	Added DV_DSUB				*
 * T. Lee/SAIC		 6/03	Added DF_FOSB				*
 * A. Hardy/NCEP	 8/03	Added DF_INT and DF_NINT		*
 * A. Hardy/NCEP	 5/04	Added DF_PLCL and DF_TLCL		*
 * T. Lee/SAIC		12/04	Pass up internal grids			*
 * m.gamazaychikov/SAIC	09/05	Added DF_LT, DF_LE, DF_GT, DF_GE	*
 *				DF_GTLT, DF_GELT, DF_GTLE, DF_GELE	*
 * m.gamazaychikov/SAIC	09/05	Added DF_AND, DF_OR, DF_EOR		*
 * m.gamazaychikov/SAIC	09/05	Added DF_EQ, DF_NE, DF_NOT		*
 * T. Lee/SAIC		10/05	Added DF_SGMX, DF_SGMN			*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 * S. Chiswell/Unidata   9/06   Added DF_PRCL                           *
 * K. Brill/HPC          4/08   Added DF_INCD, DF_BNCDF, DF_IBNCD	*
 * G. McFadden/SAIC      3/09   Added DV_GCIR				*
 * G. McFadden/SAIC      9/09   Added DV_GCWV				*
 * K. Brill/HPC		12/10	Added DV_VESD				*
 * C. Melick/SPC         8/11   Added DV_PVR3				*
 * K. Brill/HPC         05/12	Added DF_RDFS and DV_RDFV		*
 * C. Melick/SPC	 6/12	Added DF_NMAX, DF_NMIN			*
 ************************************************************************/
{
    char func[17];
    int ignum, ignumu, ignumv, nargs, num, numu, numv, zero, ii, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;
    _dgstck.itop = -1;

    dg_ssub ( iret );

    /*
     * Check first for a single parameter to be found.
     */
    if ( _dgtabl.ltabl < 0 ) {
	*iret = -3;
    } else if ( _dgtabl.ltabl == 0 && _dgtabl.ctabl[0][0] == '*' ) {
	*iret = -6;
	strcpy ( _dgerr.errst, &_dgtabl.ctabl[0][1] );
    } else if ( _dgtabl.ltabl == 0 ) {
	if ( *itype == 1 ) {
	    _dgstck.itop = 0;
	    strcpy ( _dgstck.stack[_dgstck.itop], _dgtabl.ctabl[0] );
	    _dgstck.icpntr[_dgstck.itop] = 0;
	    dg_gets ( &ignum, iret );
	    if ( *iret == 0 ) dg_puts ( &ignum, iret );
	} else {
	    _dgstck.itop = 0;
	    strcpy ( _dgstck.stack[_dgstck.itop], _dgtabl.ctabl[0] );
	    _dgstck.icpntr[_dgstck.itop] = 0; 
	    dg_getv ( &ignumu, &ignumv, iret );
	    if ( *iret == 0 ) dg_putv ( &ignumu, &ignumv, iret );
	}
    } else {
	ii = _dgtabl.ltabl;
	while ( ( ii >= 0 ) && ( *iret == 0 ) ) {
	    strcpy ( func, _dgtabl.ctabl[ii] );
	    nargs = _dgtabl.nfargs[ii];

	    /*
	     * If this is an operand, add it to the stack.
	     */
	    if ( func[0] != '*' ) {
		_dgstck.itop++;
		strcpy ( _dgstck.stack[_dgstck.itop], func );
		_dgstck.icpntr[_dgstck.itop] = ii;

	    /*
	     * If this is an operator, call the appropriate subroutine.
	     */
	    } else {
		memmove ( func, &func[1], strlen(&func[1])+1 );
		_dgerr.errst[0] = '\0';

		/*
		 * DF functions.
		 */
		if ( strcmp ( func, "MIXR" ) == 0 ) {
		    df_mixr ( iret );
		} else if ( strcmp ( func, "THTA" ) == 0 ) {
		    df_thta ( iret );
		} else if ( strcmp ( func, "THTE" ) == 0 ) {
		    df_thte ( iret );
		} else if ( strcmp ( func, "SM5S" ) == 0 ) {
		    df_sm5s ( iret );
		} else if  ( strcmp ( func, "GWFS" ) == 0 ) {
		    df_gwfs ( iret );
		} else if  ( strcmp ( func, "RDFS" ) == 0 ) {
		    df_rdfs ( iret );
		} else if  ( strcmp ( func, "SM9S" ) == 0 ) {
		    df_sm9s ( iret );
		} else if ( strcmp ( func, "SAVS" ) == 0 ) {
		    df_savs ( iret );
		} else if  ( strcmp ( func, "RELH" ) == 0 ) {
		    df_relh ( iret );
		} else if  ( strcmp ( func, "DDEN" ) == 0 ) {
		    df_dden ( iret );
		} else if  ( strcmp ( func, "STAB" ) == 0 ) {
		    df_stab  ( iret );
		} else if  ( strcmp ( func, "DDT" ) == 0 ) {
		    df_ddt  ( iret );
		} else if  ( strcmp ( func, "AVG" ) == 0 )  {
		    df_avg  ( iret );
		} else if  ( strcmp ( func, "DDX" ) == 0 ) {
		    df_ddx ( iret );
		} else if  ( strcmp ( func, "DDY" ) == 0 ) {
		    df_ddy  ( iret );
		} else if  ( strcmp ( func, "LAP" ) == 0 ) {
		    df_lap  ( iret );
		} else if  ( strcmp ( func, "TDF" ) == 0 )  {
		    df_tdf  ( iret );
		} else if  ( strcmp ( func, "TAV" ) == 0 )  {
		    df_tav  ( iret );
		} else if  ( strcmp ( func, "LDF" ) == 0 )  {
		    df_ldf  ( iret );
		} else if  ( strcmp ( func, "LAV" ) == 0 )  {
		    df_lav  ( iret );
		} else if  ( strcmp ( func, "SUB" ) == 0 )  {
		    df_sub ( iret );
		} else if  ( strcmp ( func, "ADD" ) == 0 )  {
		    df_add  ( iret );
		} else if  ( strcmp ( func, "MUL" ) == 0 ) {
		    df_mul  ( iret );
		} else if  ( strcmp ( func, "QUO" ) == 0 ) {
		    df_quo  ( iret );
		} else if  ( strcmp ( func, "EXP" ) == 0 ) {
		    df_exp  ( iret );
		} else if  ( strcmp ( func, "EXPI" ) == 0 ) {
		    df_expi ( iret );
		} else if  ( strcmp ( func, "SQRT" ) == 0 ) {
		    df_sqrt  ( iret );
		} else if  ( strcmp ( func, "LN" ) == 0 )   {
		    df_ln  ( iret );
		} else if  ( strcmp ( func, "LOG" ) == 0 )  {
		    df_log  ( iret );
		} else if  ( strcmp ( func, "SIN" ) == 0 )  {
		    df_sin ( iret );
		} else if  ( strcmp ( func, "COS" ) == 0 )  {
		    df_cos ( iret );
		} else if  ( strcmp ( func, "TAN" ) == 0 )  {
		    df_tan ( iret );
		} else if  ( strcmp ( func, "ASIN" ) == 0 ) {
		    df_asin ( iret );
		} else if  ( strcmp ( func, "ACOS" ) == 0 ) {
		    df_acos ( iret );
		} else if  ( strcmp ( func, "ATAN" ) == 0 ) {
		    df_atan ( iret );
		} else if  ( strcmp ( func, "ATN2" ) == 0 ) {
		    df_atn2 ( iret );
		} else if  ( strcmp ( func, "KNTS" ) == 0 ) {
		    df_knts  ( iret );
		} else if  ( strcmp ( func, "ABS" ) == 0 ) {
		    df_abs   ( iret );
		} else if  ( strcmp ( func, "XAV" ) == 0 ) {
		    df_xav   ( iret );
		} else if  ( strcmp ( func, "YAV" ) == 0 ) {
		    df_yav   ( iret );
		} else if  ( strcmp ( func, "XSUM" ) == 0 ) {
		    df_xsum   ( iret );
		} else if  ( strcmp ( func, "YSUM" ) == 0 ) {
		    df_ysum   ( iret );
		} else if  ( strcmp ( func, "THAN" ) == 0 ) {
		    df_than   ( iret );
		} else if  ( strcmp ( func, "JCBN" ) == 0 ) {
		    df_jcbn   ( iret );
		} else if  ( strcmp ( func, "HIGH" ) == 0 ) {
		    df_high   ( iret );
		} else if  ( strcmp ( func, "LOWS" ) == 0 ) {
		    df_lows   ( iret );
		} else if  ( strcmp ( func, "POIS" ) == 0 ) {
		    df_pois   ( iret );
		} else if  ( strcmp ( func, "THWC" ) == 0 ) {
		    df_thwc   ( iret );
		} else if  ( strcmp ( func, "TMWK" ) == 0 ) {
		    df_tmwk   ( iret );
		} else if  ( strcmp ( func, "THES" ) == 0 ) {
		    df_thes   ( iret );
		} else if  ( strcmp ( func, "TMST" ) == 0 ) {
		    df_tmst   ( iret );
		} else if  ( strcmp ( func, "BVSQ" ) == 0 ) {
		    df_bvsq   ( iret );
		} else if  ( strcmp ( func, "WNDX" ) == 0 ) {
		    df_wndx   ( iret );
		} else if  ( strcmp ( func, "SLT" ) == 0 ) {
		    df_slt   ( iret );
		} else if  ( strcmp ( func, "SLE" ) == 0 ) {
		    df_sle   ( iret );
		} else if  ( strcmp ( func, "SGT" ) == 0 ) {
		    df_sgt   ( iret );
		} else if  ( strcmp ( func, "SGE" ) == 0 ) {
		    df_sge   ( iret );
		} else if  ( strcmp ( func, "SBTW" ) == 0 ) {
		    df_sbtw   ( iret );
		} else if  ( strcmp ( func, "MASK" ) == 0 ) {
		    df_mask   ( iret );
		} else if  ( strcmp ( func, "BOOL" ) == 0 ) {
		    df_bool   ( iret );
		} else if  ( strcmp ( func, "NCDF" ) == 0 ) {
		    df_ncdf   ( iret );
		} else if  ( strcmp ( func, "INCD" ) == 0 ) {
		    df_incd   ( iret );
		} else if  ( strcmp ( func, "BNCDF" ) == 0 ) {
		    df_bncdf   ( iret );
		} else if  ( strcmp ( func, "IBNCD" ) == 0 ) {
		    df_ibncd   ( iret );
		} else if  ( strcmp ( func, "SMAX" ) == 0 ) {
		    df_smax   ( iret );
		} else if  ( strcmp ( func, "SMIN" ) == 0 ) {
		    df_smin   ( iret );
		} else if  ( strcmp ( func, "PAUB" ) == 0 ) {
		    df_paub   ( iret );
		} else if  ( strcmp ( func, "MISS" ) == 0 ) {
		    df_miss   ( iret );
		} else if  ( strcmp ( func, "FOSB" ) == 0 ) {
		    df_fosb   ( iret );
		} else if  ( strcmp ( func, "INT" ) == 0 ) {
		    df_int   ( iret );
		} else if  ( strcmp ( func, "NINT" ) == 0 ) {
		    df_nint   ( iret );
		} else if  ( strcmp ( func, "PLCL" ) == 0 ) {
		    df_plcl   ( iret );
		} else if  ( strcmp ( func, "TLCL" ) == 0 ) {
		    df_tlcl   ( iret );
		} else if  ( strcmp ( func, "LT" ) == 0 ) {
		    df_lt   ( iret );
		} else if  ( strcmp ( func, "LE" ) == 0 ) {
		    df_le   ( iret );
		} else if  ( strcmp ( func, "GT" ) == 0 ) {
		    df_gt   ( iret );
		} else if  ( strcmp ( func, "GE" ) == 0 ) {
		    df_ge   ( iret );
		} else if  ( strcmp ( func, "GTLT" ) == 0 ) {
		    df_gtlt   ( iret );
		} else if  ( strcmp ( func, "GELT" ) == 0 ) {
		    df_gelt   ( iret );
		} else if  ( strcmp ( func, "GTLE" ) == 0 ) {
		    df_gtle   ( iret );
		} else if  ( strcmp ( func, "GELE" ) == 0 ) {
		    df_gele   ( iret );
		} else if  ( strcmp ( func, "AND" ) == 0 ) {
		    df_and    ( &nargs, iret );
		} else if  ( strcmp ( func, "OR" ) == 0 ) {
		    df_or    ( &nargs, iret );
		} else if  ( strcmp ( func, "EOR" ) == 0 ) {
		    df_eor    ( &nargs, iret );
		} else if  ( strcmp ( func, "EQ" ) == 0 ) {
		    df_eq    ( iret );
		} else if  ( strcmp ( func, "NE" ) == 0 ) {
		    df_ne    ( iret );
		} else if  ( strcmp ( func, "NOT" ) == 0 ) {
		    df_not   ( iret );
		} else if  ( strcmp ( func, "SGMX" ) == 0 )  {
		    df_sgmx  ( iret );
		} else if  ( strcmp ( func, "SGMN" ) == 0 )  {
		    df_sgmn  ( iret );
		} else if  ( strcmp ( func, "NMAX" ) == 0 )  {
		    df_nmax  ( iret );
		} else if  ( strcmp ( func, "NMIN" ) == 0 )  {
		    df_nmin  ( iret );
		} else if  ( strcmp ( func, "CAPE" ) == 0 )  {
		    df_prcl  ( func, iret );
		} else if  ( strcmp ( func, "CINS" ) == 0 )  {
		    df_prcl  ( func, iret );
		} else if  ( strcmp ( func, "RELI" ) == 0 ) {
		    df_reli ( iret );

		/*
		 * DV functions.
		 */
		} else if ( strcmp ( func, "DIV" ) == 0 ) {
		    dv_div ( iret );
		} else if ( strcmp ( func, "DIVT" ) == 0 ) {
		    dv_divt ( iret );
		} else if ( strcmp ( func, "VOR" ) == 0 ) {
		    dv_vor ( iret );
		} else if ( strcmp ( func, "AVOR" ) == 0 ) {
		    dv_avor ( iret );
		} else if ( strcmp ( func, "PVOR" ) == 0 ) {
		    dv_pvor ( iret );
		} else if ( strcmp ( func, "PVR3" ) == 0 ) {
		    dv_pvr3 ( iret );
		} else if ( strcmp ( func, "GEO" ) == 0 ) {
		    dv_geo ( iret );
		} else if ( strcmp ( func, "AGE" ) == 0 ) {
		    dv_age ( iret );
		} else if  ( strcmp ( func, "SM5V" ) == 0 ) {
		    dv_sm5v ( iret );
		} else if  ( strcmp ( func, "GWFV" ) == 0 ) {
		    dv_gwfv ( iret );
		} else if  ( strcmp ( func, "RDFV" ) == 0 ) {
		    dv_rdfv ( iret );
		} else if ( strcmp ( func, "VAVS" ) == 0 ) {
		    dv_vavs ( iret );
		} else if  ( strcmp ( func, "GRAD" ) == 0 ) {
		    dv_grad ( iret );
		} else if  ( strcmp ( func, "ADV" ) == 0 ) {
		    dv_adv ( iret );
		} else if  ( strcmp ( func, "NORM" ) == 0 ) {
		    dv_norm   ( iret );
		} else if  ( strcmp ( func, "RAD" ) == 0 ) {
		    dv_rad   ( iret );
		} else if  ( strcmp ( func, "MRAD" ) == 0 ) {
		    dv_mrad   ( iret );
		} else if  ( strcmp ( func, "TNG" ) == 0 ) {
		    dv_tng   ( iret );
		} else if  ( strcmp ( func, "MTNG" ) == 0 ) {
		    dv_mtng   ( iret );
		} else if  ( strcmp ( func, "TANG" ) == 0 ) {
		    dv_tang   ( iret );
		} else if  ( strcmp ( func, "NORMV" ) == 0 ) {
		    dv_nrmv   ( iret );
		} else if  ( strcmp ( func, "TANGV" ) == 0 ) {
		    dv_tngv   ( iret );
		} else if ( strcmp ( func, "CIRC" ) == 0 )  {
		    dv_circ ( iret );
		} else if  ( strcmp ( func, "ISAL" ) == 0 ) {
		    dv_isal ( iret );
		} else if  ( strcmp ( func, "THRM" ) == 0 ) {
		    dv_thrm ( iret );
		} else if  ( strcmp ( func, "SDIV" ) == 0 ) {
		    dv_sdiv ( iret );
		} else if  ( strcmp ( func, "MDIV" ) == 0 ) {
		    dv_mdiv  ( iret );
		} else if  ( strcmp ( func, "MSDV" ) == 0 ) {
		    dv_msdv  ( iret );
		} else if  ( strcmp ( func, "SHR" ) == 0 ) {
		    dv_shr  ( iret );
		} else if  ( strcmp ( func, "STR" ) == 0 ) {
		    dv_str  ( iret );
		} else if  ( strcmp ( func, "DEF" ) == 0 ) {
		    dv_def  ( iret );
		} else if  ( strcmp ( func, "INAD" ) == 0 ) {
		    dv_inad  ( iret );
		} else if  ( strcmp ( func, "ROSS" ) == 0 ) {
		    dv_ross  ( iret );
		} else if  ( strcmp ( func, "RICH" ) == 0 ) {
		    dv_rich  ( iret );
		} else if  ( strcmp ( func, "FRNT" ) == 0 ) {
		    dv_frnt  ( iret );
		} else if  ( strcmp ( func, "VLAV" ) == 0 ) {
		    dv_vlav  ( iret );
		} else if  ( strcmp ( func, "VLDF" ) == 0 ) {
		    dv_vldf  ( iret );
		} else if  ( strcmp ( func, "LTRN" ) == 0 ) {
		    dv_ltrn  ( iret );
		} else if  ( strcmp ( func, "UN" ) == 0 )   {
		    dv_un ( iret );
		} else if  ( strcmp ( func, "VN" ) == 0 )   {
		    dv_vn ( iret );
		} else if  ( strcmp ( func, "UR" ) == 0 )   {
		    dv_ur ( iret );
		} else if  ( strcmp ( func, "VR" ) == 0 )   {
		    dv_vr ( iret );
		} else if  ( strcmp ( func, "DOT" ) == 0 )  {
		    dv_dot  ( iret );
		} else if  ( strcmp ( func, "MAG" ) == 0 )  {
		    dv_mag  ( iret );
		} else if  ( strcmp ( func, "DIRN" ) == 0 ) {
		    dv_dirn  ( iret );
		} else if  ( strcmp ( func, "DIRR" ) == 0 ) {
		    dv_dirr  ( iret );
		} else if  ( strcmp ( func, "VECN" ) == 0 ) {
		    dv_vecn  ( iret );
		} else if  ( strcmp ( func, "VECR" ) == 0 ) {
		    dv_vecr  ( iret );
		} else if  ( strcmp ( func, "VESD" ) == 0 ) {
		    dv_vesd  ( iret );
		} else if  ( strcmp ( func, "VSUB" ) == 0 ) {
		    dv_vsub ( iret );
		} else if  ( strcmp ( func, "VADD" ) == 0 ) {
		    dv_vadd  ( iret );
		} else if  ( strcmp ( func, "VMUL" ) == 0 ) {
		    dv_vmul  ( iret );
		} else if  ( strcmp ( func, "SMUL" ) == 0 ) {
		    dv_smul  ( iret );
		} else if  ( strcmp ( func, "VQUO" ) == 0 ) {
		    dv_vquo  ( iret );
		} else if  ( strcmp ( func, "SQUO" ) == 0 ) {
		    dv_squo  ( iret );
		} else if  ( strcmp ( func, "ROT" ) == 0 )  {
		    dv_rot  ( iret );
		} else if  ( strcmp ( func, "KNTV" ) == 0 ) {
		    dv_kntv  ( iret );
		} else if  ( strcmp ( func, "WNAN" ) == 0 ) {
		    dv_wnan   ( iret );
		} else if  ( strcmp ( func, "MSFC" ) == 0 ) {
		    dv_msfc   ( iret );
		} else if  ( strcmp ( func, "CROS" ) == 0 ) {
		    dv_cros   ( iret );
		} else if  ( strcmp ( func, "KCRS" ) == 0 ) {
		    dv_kcrs   ( iret );
		} else if  ( strcmp ( func, "DVDX" ) == 0 ) {
		    dv_dvdx   ( iret );
		} else if  ( strcmp ( func, "DVDY" ) == 0 ) {
		    dv_dvdy   ( iret );
		} else if  ( strcmp ( func, "VASV" ) == 0 ) {
		    dv_vasv   ( iret );
		} else if  ( strcmp ( func, "QVEC" ) == 0 ) {
		    dv_qvec   ( iret );
		} else if  ( strcmp ( func, "QVCL" ) == 0 ) {
		    dv_qvcl   ( iret );
		} else if  ( strcmp ( func, "WSHR" ) == 0 ) {
		    dv_wshr   ( iret );
		} else if  ( strcmp ( func, "VLT" ) == 0 ) {
		    dv_vlt   ( iret );
		} else if  ( strcmp ( func, "VLE" ) == 0 ) {
		    dv_vle   ( iret );
		} else if  ( strcmp ( func, "VGT" ) == 0 ) {
		    dv_vgt   ( iret );
		} else if  ( strcmp ( func, "VGE" ) == 0 ) {
		    dv_vge   ( iret );
		} else if  ( strcmp ( func, "VBTW" ) == 0 ) {
		    dv_vbtw   ( iret );
		} else if  ( strcmp ( func, "VMSK" ) == 0 ) {
		    dv_vmsk   ( iret );
		} else if  ( strcmp ( func, "DSUB" ) == 0 ) {
		    dv_dsub   ( iret );
		} else if  ( strcmp ( func, "GCIR" ) == 0 ) {
		    dv_gcir   ( iret );
		} else if  ( strcmp ( func, "GCWV" ) == 0 ) {
		    dv_gcwv   ( iret );
		} else {
		    *iret = -19;
		    strcpy ( _dgerr.errst, func );
		}
		if ( *iret != 0 && strcmp ( _dgerr.errst, func ) != 0 ) {
		    strcat ( _dgerr.errst, " in " );
		    strcat ( _dgerr.errst, func );
		}
	    }
	    ii--;
	}

	/*
	 * Check to see that one answer is returned.
	 */ 
	if ( ( *iret == 0 ) && 
	     ( ( _dgstck.itop != 0 ) || ( _dgstck.stack[0][0] != '\0' ) ) ) {
	    *iret = -6;
    	}
    }

    num = _dgstck.istack[0];
    if ( *itype == 1 ) {
	dg_esub ( &num, &zero, &zero, &zero, &ier );
    } else {
	numu = num / 100;
	numv = num - numu * 100;
	dg_esub ( &numu, &numv, &zero, &zero, &ier );
    }
    if ( ier != 0 ) *iret = ier;

    return;
}
