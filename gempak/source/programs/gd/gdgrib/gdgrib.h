/************************************************************************
 * gdgrib.h                                                             *
 *                                                                      *
 * This header file is used in the GDGRIB program.                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         09/06   Created                                 *
 ************************************************************************/

#ifndef GDGRIB_H_
#define GDGRIB_H_

#include        "geminc.h"
#include        "gemprm.h"

/*
 * Structure for user inputs.
 */
typedef struct {
    char gdfile[LLMXLN+1];      /* Grid file */
    char gfunc [LLMXLN+1];      /* Scalar grid */
    char gdatim[LLMXLN+1];      /* Grid date/time */
    char glevel[LLMXLN+1];      /* Grid level */
    char gvcord[LLMXLN+1];	/* Grid vertical coordinate */
    char gbtbls[LLMXLN+1];	/* Input GRIB decoding tables */
    char gbfile[LLMXLN+1];	/* GRIB data file name */
    char vercen[LLMXLN+1];	/* PDS byte_4/byte_5/byte_6/byte_26 */
    char pdsval[LLMXLN+1];	/* GRIB PDS grid identifier overrides */
    char precsn[LLMXLN+1];	/* Packing precision */
    char wmohdr[LLMXLN+1];	/* WMO_ID/Origin_ID/DDHHMM */
    char cpyfil[LLMXLN+1];	/* Grid file whose navigation is to be
				   used in new grid file | subare */
    char proj  [LLMXLN+1];      /* Map projection/angles/margins|drop flag */
    char gdarea[LLMXLN+1];	/* Area covered by grid */
    char kxky  [LLMXLN+1];	/* Number of grid points in x;y */
} GDGRIB_input;

/*
 * Could be removed after GDCFIL is translated.
 */
#ifdef UNDERSCORE
#define gdgcfl  gdgcfl_
#endif

/*
 * APIs.
 */
void bds_ibm  ( float *xref, unsigned char *ibm, int *iret );
void bds_mak  ( const int *igx, const int *igy, const char *precsn,
                float *grid, int *nbyts, unsigned char *cbds, int *ip10sc,
                int *misval, int *iret );
void bds_nbt  ( const float *rmin, const float *rmax, const float *rdb,
                int *nmbts, int *iscale, float *rmn, int *iret );
void bds_pgb  ( const float *grid, const int *igx, const int *igy,
                const float *qmin, const float *qmax, const int *nbits,
                int *lendat, unsigned char *cdata, int *iscale, int *iret );
void bms_mak  ( const float *grid, const int *igx, const int *igy,
                int *nbyts, unsigned char *cbms, int *iret );
void gdgpds   ( const char *pdsval, const char *vercen, const float *rnvblk,
                const char *gparm, const int *ivcord, const int *level1,
                const int *level2, const int *havbms, const int *ip10sc,
                const char *lasttm, const char *gdttm1, const char *gdttm2,
                const char *gbtbls, const int *igpds, int *nbpds,
                unsigned char *cpds, char *cdd, char *chhmm, int *iret );
void gdguin   ( GDGRIB_input *ui, int *iret );
void gdgwmo   ( const char *wmohdr, const int *ncntr, const char *cdd,
                const char *chhmm, unsigned char *chdr, int *iret );
void gdigit   ( const int *ival, const int *ibase, int *ndig, int *idigs,
                int *iret );
void gds_ced  ( const float *rnvblk, const int*nnv, int *nbytes,
                unsigned char *cgds, int *iret );
void gds_lcc  ( const int *navchg, const float *rnvblk, const int *nnv,
                int *nbytes, unsigned char *cgds, int *iret );
void gds_mak  ( const int *navchg, const float *rnvblk, const int *nnv,
                int *nbytes, unsigned char *cgds, int *iret );
void gds_mer  ( const float *rnvblk, const int *nnv, int *nbytes,
                unsigned char *cgds, int *iret );
void gds_str  ( const int *navchg, const float *rnvblk, const int *nnv,
                int *nbytes, unsigned char *cgds, int *iret );
void pds_byt7 ( const float *rnvblk, const int *nnv, unsigned char *byte7,
                int *ibyt7, int *iret );
void pds_byt9 ( const char *parm, const char *wmotb, const char *nceptb,
                unsigned char *byte9, int *ibyt9, int *idt, int *iret );
void pds_by10 ( const char *cvcrd, const char *wmotb, const char *nceptb,
                unsigned char *byte10, int *ibyt10, int *iscale, int *iret );
void pds_by11 ( const int *level1, const int *level2, int *iscale,
                unsigned char *byte10, int *ibyt10, unsigned char *byte11,
                int *ibyt11, unsigned char *byte12, int *ibyt12, int *iret );
void pds_make ( const int *noptv, const int *idoc, const int *idngp,
                const int *idosc, const float *rnvblk, const char *gparm,
                const char *oparm, const char *gvcrd, const char *ovcrd,
                const int *level1, const int *level2, const char *olevl,
                const int *havbms, const int *ipsc10, const char *lasttm,
                const char *flgdtm1, const char *flgdtm2, const char *gbtbls,
                const int *igpds, int *nbyts, unsigned char *cpds,
                int *idhm, int *iret );
void pds_vldt ( const char *flgdtm1, const char *flgdtm2,
                const char *lasttm, int *idt, unsigned char *b13_25,
                int *i13_25, int *iret );

/*
 * Could be removed after GDCFIL is translated.
 */
void gdgcfl ( const char *gdfile, const char *proj, const char *gdarea,
              const char *kxky, const char *maxgrd, const char *cpyfil,
              const char *anlyss, int *iret, size_t, size_t, size_t,
              size_t, size_t, size_t, size_t );

#endif
