/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: timeunits.h,v 1.7 1995/12/07 15:31:02 russ Exp $ */

/* GRIB timeunits */

#ifndef TIMEUNITS_H_
#define TIMEUNITS_H_

#include "product_data.h"

/* Forecast time unit, Table 4 of GRIB, The WMO Format ... */
#define TUNITS_MIN	0
#define TUNITS_HOUR	1
#define TUNITS_DAY	2
#define TUNITS_MONTH	3
#define TUNITS_YEAR	4
#define TUNITS_DECADE	5
#define TUNITS_NORMAL	6
#define TUNITS_CENTURY	7
#define TUNITS_3HR	10
#define TUNITS_6HR	11
#define TUNITS_12HR	12
#define TUNITS_SECOND	254

/* Time range indicator, Table 5 of GRIB, The WMO Format ... */
#define TRI_P1		0
#define TRI_IAP		1
#define TRI_P12		2
#define TRI_Ave		3
#define TRI_Acc		4
#define TRI_Dif		5
#define TRI_LP1		10
#define TRI_AvgN	113
#define TRI_AccN	114
#define TRI_AvgN1	115
#define TRI_AccN1	116
#define TRI_AvgN2	117
#define TRI_VarN	118
#define TRI_SdN		119
#define TRI_AvgN3	123
#define TRI_AccN3	124

#ifdef __cplusplus
extern "C" char* tunitsname(int timeu);
extern "C" char* tunits(int timeu);
extern "C" char* triname(int timeu);
extern "C" int frcst_time(product_data *);
#elif defined(__STDC__)
extern char* tunitsname(int timeu);
extern char* tunits(int timeu);
extern char* triname(int timeu);
extern int frcst_time(product_data *);
#else
#define const
extern char* tunitsname ( /* int timeu */ );
extern char* tunits ( /* int timeu */ );
extern char* triname ( /* int timeu */ );
extern int frcst_time( /* product_data * */ );
#endif

#endif /* TIMEUNITS_H_ */
