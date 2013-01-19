/* kb2fy2.f -- translated by f2c (version 20031025).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    char cal_input__[4], cal_output__[4];
    integer src_byte_size__, dest_byte_size__;
} fy2fy2kb2_;

#define fy2fy2kb2_1 fy2fy2kb2_

/* Table of constant values */

static integer c__26112 = 26112;

integer kb2inify2_(char *input_cal__, char *output_cal__, integer *io_size__, 
	ftnlen input_cal_len, ftnlen output_cal_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

/* input calibration type (RAW) */
/* output cal type (TEMP,ALB,RAD,RAW,BRIT) */
/* source and destination byte depth */
/* input calibration type (RAW) */
/* output cal type (TEMP, ALB, RAD, RAW, BRI */
/* number of bytes for destination pixel */
/* number of bytes for source pixel */
    /* Parameter adjustments */
    --io_size__;

    /* Function Body */
    s_copy(fy2fy2kb2_1.cal_input__, input_cal__, (ftnlen)4, (ftnlen)4);
    s_copy(fy2fy2kb2_1.cal_output__, output_cal__, (ftnlen)4, (ftnlen)4);
    if (s_cmp(fy2fy2kb2_1.cal_input__, "RAW", (ftnlen)3, (ftnlen)3) != 0) {
	ret_val = -1;
    } else if (s_cmp(fy2fy2kb2_1.cal_output__, "TEMP", (ftnlen)4, (ftnlen)4) 
	    != 0 && s_cmp(fy2fy2kb2_1.cal_output__, "RAD", (ftnlen)3, (ftnlen)
	    3) != 0 && s_cmp(fy2fy2kb2_1.cal_output__, "RAW", (ftnlen)3, (
	    ftnlen)3) != 0 && s_cmp(fy2fy2kb2_1.cal_output__, "ALB", (ftnlen)
	    3, (ftnlen)3) != 0 && s_cmp(fy2fy2kb2_1.cal_output__, "BRIT", (
	    ftnlen)4, (ftnlen)4) != 0) {
	ret_val = -1;
    } else {
	fy2fy2kb2_1.src_byte_size__ = io_size__[1];
	fy2fy2kb2_1.dest_byte_size__ = io_size__[2];
	ret_val = 0;
    }
    return ret_val;
} /* kb2inify2_ */

integer kb2calfy2_(integer *prefix, integer *area_dir__, integer *
	num_pixels__, integer *band, integer *ibuf)
{
    /* Initialized data */

    static integer last_area__ = -1;
    static integer last_band__ = -1;
    static integer vis_band_offset__ = 192;
    static integer vis_detector__[4] = { 1819017216,-1263271936,-656932864,
	    -50593792 };

    /* System generated locals */
    integer ret_val;
    real r__1;

    /* Builtin functions */
    double sqrt(doublereal);
    integer i_nint(real *), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static real radiance;
    static logical vis_data__;
    static integer detector;
    extern doublereal temp_to_radfy2kb2__(real *, integer *);
    static integer i__, j, alb_table__[256], cal_block__[6528], rad_table__[
	    1024], temp_brit__[1024], store_albedo_brit__[1280]	/* was [5][
	    256] */, cal_offset__, temp_table__[1024], vis_offset__, 
	    albedo_brit__[256], area_number__;
    static real temperature, albedo;
    extern /* Subroutine */ int araget_(integer *, integer *, integer *, 
	    integer *), mpixel_(integer *, integer *, integer *, integer *);
    static integer ir_band_offset__;
    extern /* Subroutine */ int gryscl_(real *, integer *), mpixtb_(integer *,
	     integer *, integer *, integer *, integer *);
    static logical ir_data__;
    static integer store_alb_table__[1280]	/* was [5][256] */;

/* defines NUMAREAOPTIONS */
/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: areaparm.inc,v 1.1 2000/07/12 13:12:23 gad Exp $ *** */
/*  area subsystem parameters */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/* NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE NOTE */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  IF YOU CHANGE THESE VALUES, YOU MUST ALSO CHANGE THEM IN */
/*   MCIDAS.H !! */
/* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX */
/*  MAXGRIDPT		maximum number of grid points */
/*  MAX_BANDS		maximum number of bands within an area */

/*  MAXDFELEMENTS	maximum number of elements that DF can handle */
/* 			in an area line */
/*  MAXOPENAREAS		maximum number of areas that the library can */
/* 			have open (formerly called `NA') */
/*  NUMAREAOPTIONS	number of options settable through ARAOPT() */
/* 			It is presently 5 because there are five options */
/* 			that ARAOPT() knows about: */
/* 				'PREC','SPAC','UNIT','SCAL','CALB' */
/* 			(formerly called `NB') */
/* --- Size (number of words) in an area directory */
/* 	MAX_AUXBLOCK_SIZE	size (in bytes) of the internal buffers */
/* 				used to recieve AUX blocks during an */
/* 				ADDE transaction */

/* ----- MAX_AREA_NUMBER        Maximum area number allowed on system */


/* ----- MAXAREARQSTLEN - max length of area request string */

/* line prefix information to determine detector */
/* area directory */
/* number of pixels in ibuf array */
/* band number */
/* I/O array containing pixels to be modified */
/* converts temperature to radiance */
/* input calibration type RAW */
/* output cal type (TEMP, ALB, RAD, RAW, BRI */
/* albedo table determined from raw values t */
/* brightness table determined from albedo */
/* area number found in area directory */
/* cal block - includes directory and table */
/* offset into area directory to get cal block */
/* number of bytes for destination pixel */
/* which detector is used (1-4) */
/* do loop index */
/* offset into ir tables to find band temp t */
/* offset into cal block to find ir tables */
/* do loop index */
/* last area used for araget */
/* last area used for araget */
/* radiance table determined from temperatu */
/* number of bytes for source pixel */
/* albedo table determined from raw */
/* brightness table determined fro */
/* brightness table determined from tempera */
/* temperature table determined from raw v */
/* offset into cal block to find vis tables */
/* codes determining visible detector used */
/* offset into cal block to find albedo table */
/* albedo read in from calibration table */
/* radiance returned from temp_to_radfy2kb2 */
/* temperature read in from calibration table */
/* flag indicating ir data requested */
/* flag indicating visible data requested */
    /* Parameter adjustments */
    --ibuf;
    --area_dir__;
    --prefix;

    /* Function Body */
    ret_val = 0;
    if (*band == 1) {
	vis_data__ = TRUE_;
	ir_data__ = FALSE_;
    } else {
	vis_data__ = FALSE_;
	ir_data__ = TRUE_;
    }
    area_number__ = area_dir__[33];
    cal_offset__ = area_dir__[63];
    if (last_area__ != area_number__) {
	araget_(&area_number__, &cal_offset__, &c__26112, cal_block__);
    }
    last_area__ = area_number__;
    if (vis_data__) {
	detector = 1;
	for (i__ = 1; i__ <= 4; ++i__) {
	    if (vis_detector__[i__ - 1] == prefix[2]) {
		detector = i__;
	    }
/* L10: */
	}
    }
    if (vis_data__ && last_band__ != *band) {
	for (j = 1; j <= 4; ++j) {
	    for (i__ = 1; i__ <= 256; i__ += 4) {
		vis_offset__ = vis_band_offset__ + (j - 1 << 6);
		albedo = (real) cal_block__[vis_offset__ + i__ / 4] / 1e4f;
		r__1 = sqrt(albedo) * 25.5f + .5f;
		store_albedo_brit__[j + i__ * 5 - 6] = i_nint(&r__1);
		r__1 = albedo * 100.f;
		store_alb_table__[j + i__ * 5 - 6] = i_nint(&r__1);
		r__1 = sqrt(albedo) * 25.5f + .5f;
		store_albedo_brit__[j + (i__ + 1) * 5 - 6] = i_nint(&r__1);
		r__1 = albedo * 100.f;
		store_alb_table__[j + (i__ + 1) * 5 - 6] = i_nint(&r__1);
		r__1 = sqrt(albedo) * 25.5f + .5f;
		store_albedo_brit__[j + (i__ + 2) * 5 - 6] = i_nint(&r__1);
		r__1 = albedo * 100.f;
		store_alb_table__[j + (i__ + 2) * 5 - 6] = i_nint(&r__1);
		r__1 = sqrt(albedo) * 25.5f + .5f;
		store_albedo_brit__[j + (i__ + 3) * 5 - 6] = i_nint(&r__1);
		r__1 = albedo * 100.f;
		store_alb_table__[j + (i__ + 3) * 5 - 6] = i_nint(&r__1);
/* L30: */
	    }
/* L20: */
	}
    }
    for (i__ = 1; i__ <= 256; ++i__) {
	alb_table__[i__ - 1] = store_alb_table__[detector + i__ * 5 - 6];
	albedo_brit__[i__ - 1] = store_albedo_brit__[detector + i__ * 5 - 6];
/* L40: */
    }
    if (ir_data__ && last_band__ != *band) {
	ir_band_offset__ = cal_block__[(*band - 2 << 1) + 7] / 4;
	for (i__ = 1; i__ <= 1024; ++i__) {
	    temperature = (real) cal_block__[ir_band_offset__ + i__ - 1] / 
		    1e3f;
	    radiance = temp_to_radfy2kb2__(&temperature, band);
	    r__1 = temperature * 100.f;
	    temp_table__[i__ - 1] = i_nint(&r__1);
	    r__1 = radiance * 1e3f;
	    rad_table__[i__ - 1] = i_nint(&r__1);
	    gryscl_(&temperature, &temp_brit__[i__ - 1]);
/* L110: */
	}
    }
    if (vis_data__) {
	if (s_cmp(fy2fy2kb2_1.cal_output__, "RAW", (ftnlen)3, (ftnlen)3) == 0)
		 {
	    mpixel_(num_pixels__, &fy2fy2kb2_1.src_byte_size__, &
		    fy2fy2kb2_1.dest_byte_size__, &ibuf[1]);
	} else if (s_cmp(fy2fy2kb2_1.cal_output__, "ALB", (ftnlen)3, (ftnlen)
		3) == 0) {
	    mpixtb_(num_pixels__, &fy2fy2kb2_1.src_byte_size__, &
		    fy2fy2kb2_1.dest_byte_size__, &ibuf[1], alb_table__);
	} else if (s_cmp(fy2fy2kb2_1.cal_output__, "BRIT", (ftnlen)4, (ftnlen)
		4) == 0) {
	    mpixtb_(num_pixels__, &fy2fy2kb2_1.src_byte_size__, &
		    fy2fy2kb2_1.dest_byte_size__, &ibuf[1], albedo_brit__);
	}
    }
    if (ir_data__) {
	if (s_cmp(fy2fy2kb2_1.cal_output__, "RAW", (ftnlen)3, (ftnlen)3) == 0)
		 {
	    mpixel_(num_pixels__, &fy2fy2kb2_1.src_byte_size__, &
		    fy2fy2kb2_1.dest_byte_size__, &ibuf[1]);
	} else if (s_cmp(fy2fy2kb2_1.cal_output__, "RAD", (ftnlen)3, (ftnlen)
		3) == 0) {
	    mpixtb_(num_pixels__, &fy2fy2kb2_1.src_byte_size__, &
		    fy2fy2kb2_1.dest_byte_size__, &ibuf[1], rad_table__);
	} else if (s_cmp(fy2fy2kb2_1.cal_output__, "TEMP", (ftnlen)4, (ftnlen)
		4) == 0) {
	    mpixtb_(num_pixels__, &fy2fy2kb2_1.src_byte_size__, &
		    fy2fy2kb2_1.dest_byte_size__, &ibuf[1], temp_table__);
	} else if (s_cmp(fy2fy2kb2_1.cal_output__, "BRIT", (ftnlen)4, (ftnlen)
		4) == 0) {
	    mpixtb_(num_pixels__, &fy2fy2kb2_1.src_byte_size__, &
		    fy2fy2kb2_1.dest_byte_size__, &ibuf[1], temp_brit__);
	}
    }
    last_band__ = *band;
    return ret_val;
} /* kb2calfy2_ */

integer kb2optfy2_(char *option, integer *param_in__, integer *param_out__, 
	ftnlen option_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static logical vis_data__;
    extern integer lit_(char *, ftnlen);
    static integer band;
    extern /* Subroutine */ int movwc_(integer *, char *, ftnlen);
    extern integer ischar_(integer *), brkset_(char *, char *, ftnlen, ftnlen)
	    ;
    static logical ir_data__;
    static char su_file__[8];

/* option */
/* input parameters */
/* output parameters */
/* checks SU table */
/* checks for character string */
/* four byte integer representation of char*4 */
/* input calibration type (RAW) */
/* output cal type (TEMP, ALB, RAD, RAW, BRI */
/* stretch table file */
/* band number */
/* number of bytes for destination pixel */
/* number of bytes for source pixel */
/* flag indicating ir data requested */
/* flag indicating visible data requested */
    /* Parameter adjustments */
    --param_out__;
    --param_in__;

    /* Function Body */
    ret_val = 0;
    if (s_cmp(option, "KEYS", (ftnlen)4, (ftnlen)4) == 0) {
	band = param_in__[4];
	if (band == 1) {
	    vis_data__ = TRUE_;
	    ir_data__ = FALSE_;
	} else {
	    vis_data__ = FALSE_;
	    ir_data__ = TRUE_;
	}
	if (vis_data__) {
	    param_out__[1] = 3;
	    param_out__[2] = lit_("RAW ", (ftnlen)4);
	    param_out__[3] = lit_("ALB ", (ftnlen)4);
	    param_out__[4] = lit_("BRIT", (ftnlen)4);
	} else if (ir_data__) {
	    param_out__[1] = 4;
	    param_out__[2] = lit_("RAW ", (ftnlen)4);
	    param_out__[3] = lit_("RAD ", (ftnlen)4);
	    param_out__[4] = lit_("TEMP", (ftnlen)4);
	    param_out__[5] = lit_("BRIT", (ftnlen)4);
	}
	if (ischar_(&param_in__[38]) == 1) {
	    movwc_(&param_in__[38], su_file__, (ftnlen)8);
	    if (brkset_(su_file__, fy2fy2kb2_1.cal_input__, (ftnlen)8, (
		    ftnlen)4) != 0) {
		ret_val = -3;
	    }
	}
    } else if (s_cmp(option, "BRKP", (ftnlen)4, (ftnlen)4) == 0) {
	movwc_(&param_in__[1], su_file__, (ftnlen)8);
	if (brkset_(su_file__, fy2fy2kb2_1.cal_input__, (ftnlen)8, (ftnlen)4) 
		!= 0) {
	    ret_val = -3;
	}
    } else if (s_cmp(option, "INFO", (ftnlen)4, (ftnlen)4) == 0) {
	band = param_in__[4];
	if (band == 1) {
	    vis_data__ = TRUE_;
	    ir_data__ = FALSE_;
	} else {
	    vis_data__ = FALSE_;
	    ir_data__ = TRUE_;
	}
	if (vis_data__) {
	    param_out__[1] = 3;
	    param_out__[2] = lit_("RAW ", (ftnlen)4);
	    param_out__[3] = lit_("ALB ", (ftnlen)4);
	    param_out__[4] = lit_("BRIT", (ftnlen)4);
	    param_out__[5] = lit_("    ", (ftnlen)4);
	    param_out__[6] = lit_("  % ", (ftnlen)4);
	    param_out__[7] = lit_("    ", (ftnlen)4);
	    param_out__[8] = 1;
	    param_out__[9] = 100;
	    param_out__[10] = 1;
	} else if (ir_data__) {
	    param_out__[1] = 4;
	    param_out__[2] = lit_("RAW ", (ftnlen)4);
	    param_out__[3] = lit_("RAD ", (ftnlen)4);
	    param_out__[4] = lit_("TEMP", (ftnlen)4);
	    param_out__[5] = lit_("BRIT", (ftnlen)4);
	    param_out__[6] = lit_("    ", (ftnlen)4);
	    param_out__[7] = lit_("MW**", (ftnlen)4);
	    param_out__[8] = lit_("  K ", (ftnlen)4);
	    param_out__[9] = lit_("    ", (ftnlen)4);
	    param_out__[10] = 1;
	    param_out__[11] = 1000;
	    param_out__[12] = 100;
	    param_out__[13] = 1;
	}
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* kb2optfy2_ */

doublereal temp_to_radfy2kb2__(real *temperature, integer *band)
{
    /* Initialized data */

    static real fk1[4] = { 9280.38f,7136.31f,37258.2f,224015.f };
    static real fk2[4] = { 1323.95f,1212.95f,2104.22f,3826.28f };
    static real tc[8]	/* was [2][4] */ = { .72122f,.9975f,1.00668f,.99621f,
	    3.76883f,.99108f,4.00279f,.99458f };

    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real radiance;
    static integer iband;
    static real adjusted_temp__;

/* temperature */
/* band number */
/* band number */
/* derived constants for each band */
/* derived constants for each band */
/* radiance value returned */
/* derived temp constants for each ban */
/* temperatured adjusted by derived constants */
    iband = *band - 1;
    adjusted_temp__ = tc[(iband << 1) - 2] + tc[(iband << 1) - 1] * *
	    temperature;
    radiance = fk1[iband - 1] / (exp(fk2[iband - 1] / adjusted_temp__) - 1.f);
    ret_val = radiance;
    return ret_val;
} /* temp_to_radfy2kb2__ */

