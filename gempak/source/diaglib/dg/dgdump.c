#include "dg.h"

static void dump_dggrid ( void );
static void dump_nfile  ( void );
static void dump_dgfile ( void );
static void dump_dgarea ( void );
static void dump_dgsubg ( void );
static void dump_dginpt ( void );
static void dump_dgtabl ( void );
static void dump_dgstck ( void );

static FILE *logfp;

void dg_dump ( const char *func, int *iret )
{
    *iret = 0;

    if ( ( logfp = fopen ( "dgdump.log", "a" ) ) == NULL ) {
        printf ( "Log file open failed.\n" );
	exit ( -1 );
    }

    fprintf ( logfp, "\n\n Dump Global Variables in Function - %s\n", func );
    dump_dggrid ( );
    dump_nfile  ( );
    dump_dgfile ( );
    dump_dgarea ( );
    dump_dgsubg ( );
    dump_dginpt ( );
    dump_dgtabl ( );
    dump_dgstck ( );

    fclose ( logfp );

    return;
}

static void dump_dggrid ( void )
{
    int i;
/*----------------------------------------------------------------------*/
    fprintf ( logfp, "\nDump _dggrid:\n\n" );

    fprintf ( logfp, "idglst: %d\n", _dggrid.idglst );
    for ( i = 0; i < _dggrid.idglst; i++ ) {
	fprintf ( logfp, "i = %d\n", i );
        fprintf ( logfp, "dttimd1: %s\n", _dggrid.dttimd1[i] );
	fprintf ( logfp, "dttimd2: %s\n", _dggrid.dttimd2[i] );
	fprintf ( logfp, "leveld1: %d\n", _dggrid.leveld1[i] );
	fprintf ( logfp, "leveld2: %d\n", _dggrid.leveld2[i] );
	fprintf ( logfp, "ivcrdd:  %d\n", _dggrid.ivcrdd[i] );
	fprintf ( logfp, "gparmd:  %s\n", _dggrid.gparmd[i] );
	fprintf ( logfp, "iusesv:  %d\n", _dggrid.iusesv[i] );
	fprintf ( logfp, "savflg:  %d\n", _dggrid.savflg[i] );
	fprintf ( logfp, "isubid:  %d\n", _dggrid.isubid );
	fprintf ( logfp, "\n" );
    }

    return;
}

static void dump_nfile ( void )
{
    int i;
/*----------------------------------------------------------------------*/
    fprintf ( logfp, "\nDump _nfile:\n\n" );

    for ( i = 0; i < NGDFLS; i++ ) {
        fprintf ( logfp, "i =  %d\n", i );
	fprintf ( logfp, "ntmplt:  %s\n", _nfile.ntmplt[i] );
	fprintf ( logfp, "gflpth:  %s\n", _nfile.gflpth[i] );
	fprintf ( logfp, "crtfnm:  %s\n", _nfile.crtfnm[i] );
	fprintf ( logfp, "aftrbr:  %s\n", _nfile.aftrbr[i] );
	fprintf ( logfp, "crtgdt1: %s\n", _nfile.crtgdt1[i] );
	fprintf ( logfp, "crtgdt2: %s\n", _nfile.crtgdt2[i] );
	fprintf ( logfp, "outflg:  %d\n", _nfile.outflg[i] );
	fprintf ( logfp, "mbrnum:  %d\n", _nfile.mbrnum[i] );
	fprintf ( logfp, "\n" );
    }

    return;
}

static void dump_dgfile ( void )
{
    int i;
/*----------------------------------------------------------------------*/
    fprintf ( logfp, "\nDump _dgfile:\n\n" );

    fprintf ( logfp, "dgset:  %d\n", _dgfile.dgset );
    fprintf ( logfp, "idlun:  %d\n", _dgfile.idlun );
    fprintf ( logfp, "kxd:    %d\n", _dgfile.kxd );
    fprintf ( logfp, "kyd:    %d\n", _dgfile.kyd );
    fprintf ( logfp, "kxyd:   %d\n", _dgfile.kxyd );
    fprintf ( logfp, "idglat: %d\n", _dgfile.idglat );
    fprintf ( logfp, "idglon: %d\n", _dgfile.idglon );
    fprintf ( logfp, "cprj:   %s\n", _dgfile.cprj );
    fprintf ( logfp, "anglr1: %f\n", _dgfile.anglr1 );
    fprintf ( logfp, "anglr2: %f\n", _dgfile.anglr2 );
    fprintf ( logfp, "angrl3: %f\n", _dgfile.anglr3 );
    fprintf ( logfp, "concon: %f\n", _dgfile.concon );
    fprintf ( logfp, "addcol: %d\n", _dgfile.addcol );
    for ( i = 0; i < MMFILE; i++ ) {
        fprintf ( logfp, "i = %d\n", i );
        fprintf ( logfp, "idflnm: %d\n", _dgfile.idflnm[i] );
	fprintf ( logfp, "gdcur : %s\n", _dgfile.gdcur[i] );
	fprintf ( logfp, "tmpflg: %d\n", _dgfile.tmpflg[i] );
	fprintf ( logfp, "templt: %s\n", _dgfile.templt[i] );
	fprintf ( logfp, "tdattm: %s\n", _dgfile.tdattm[i] );
	fprintf ( logfp, "tfirst: %s\n", _dgfile.tfirst[i] );
	fprintf ( logfp, "tlast : %s\n", _dgfile.tlast[i] );
        fprintf ( logfp, "\n" );
    }

    return;
}

static void dump_dgarea ( void )
{
/*----------------------------------------------------------------------*/

    fprintf ( logfp, "\nDump _dgarea:\n\n" );

    fprintf ( logfp, "kgxmin: %d\n", _dgarea.kgxmin );
    fprintf ( logfp, "kgxmax: %d\n", _dgarea.kgxmax );
    fprintf ( logfp, "kgymin: %d\n", _dgarea.kgymin );
    fprintf ( logfp, "kgymax: %d\n", _dgarea.kgymax );
    fprintf ( logfp, "kextnd: %d\n", _dgarea.kextnd );
    fprintf ( logfp, "jgxmin: %d\n", _dgarea.jgxmin );
    fprintf ( logfp, "jgxmax: %d\n", _dgarea.jgxmax );
    fprintf ( logfp, "jgymin: %d\n", _dgarea.jgymin );
    fprintf ( logfp, "jgymax: %d\n", _dgarea.jgymax );
    fprintf ( logfp, "ksub1:  %d\n", _dgarea.ksub1);
    fprintf ( logfp, "ksub2:  %d\n", _dgarea.ksub2);

    return;
}

static void dump_dgsubg ( void )
{
/*----------------------------------------------------------------------*/
    fprintf ( logfp, "\nDump _dgsubg:\n\n" );

    fprintf ( logfp, "dgsubg: %d\n", _dgsubg.dgsubg );
    fprintf ( logfp, "gwrapg: %d\n", _dgsubg.gwrapg );
    fprintf ( logfp, "ishift: %d\n", _dgsubg.ishift );
    fprintf ( logfp, "jsgxmn: %d\n", _dgsubg.jsgxmn );
    fprintf ( logfp, "jsgxmx: %d\n", _dgsubg.jsgxmx );
    fprintf ( logfp, "jsgymn: %d\n", _dgsubg.jsgymn );
    fprintf ( logfp, "jsgymx: %d\n", _dgsubg.jsgymx );
    fprintf ( logfp, "jsgxsk: %d\n", _dgsubg.jsgxsk );
    fprintf ( logfp, "jsgysk: %d\n", _dgsubg.jsgysk );

    return;
}

static void dump_dginpt ( void )
{
/*----------------------------------------------------------------------*/
    fprintf ( logfp, "\nDump _dginpt:\n\n" );

    fprintf ( logfp, "ddttim1: %s\n", _dginpt.ddttim1 );
    fprintf ( logfp, "ddttim2: %s\n", _dginpt.ddttim2 );
    fprintf ( logfp, "ldlevl1: %d\n", _dginpt.ldlevl1 );
    fprintf ( logfp, "ldlevl2: %d\n", _dginpt.ldlevl2 );
    fprintf ( logfp, "lvcord:  %d\n", _dginpt.lvcord );
    fprintf ( logfp, "ingdtm:  %s\n", _dginpt.ingdtm );
    fprintf ( logfp, "inglev:  %s\n", _dginpt.inglev );
    fprintf ( logfp, "invcrd:  %s\n", _dginpt.invcrd );

    return;
}

static void dump_dgtabl ( void )
{
    int i;
/*----------------------------------------------------------------------*/
    fprintf ( logfp, "\nDump _dgtabl:\n\n" );

    fprintf ( logfp, "ltabl:  %d\n", _dgtabl.ltabl );
    for ( i = 0; i <= _dgtabl.ltabl; i++ ) {
        fprintf ( logfp, "i = %d\n", i );
        fprintf ( logfp, "ctabl:  %s\n", _dgtabl.ctabl[i] );
	fprintf ( logfp, "clevel: %s\n", _dgtabl.clevel[i] );
	fprintf ( logfp, "cvcord: %s\n", _dgtabl.cvcord[i] );
	fprintf ( logfp, "cgdttm: %s\n", _dgtabl.cgdttm[i] );
	fprintf ( logfp, "icflnm: %d\n", _dgtabl.icflnm[i] );
	fprintf ( logfp, "nfargs: %d\n", _dgtabl.nfargs[i] );
	fprintf ( logfp, "\n" );
    }

    return;
}

static void dump_dgstck ( void )
{
    int i;
/*----------------------------------------------------------------------*/
    fprintf ( logfp, "\nDump _dgstck:\n\n" );

    fprintf ( logfp, "itop:   %d\n", _dgstck.itop );
    for ( i = 0; i <= _dgstck.itop; i++ ) {
	fprintf ( logfp, "i = %d\n", i );
        fprintf ( logfp, "stack:  %s\n", _dgstck.stack[i] );
	fprintf ( logfp, "istack: %d\n", _dgstck.istack[i] );
	fprintf ( logfp, "icpntr: %d\n", _dgstck.icpntr[i] );
	fprintf ( logfp, "\n" );
    }
}
