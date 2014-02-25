/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: models.h,v 1.4 1995/10/04 16:40:57 russ Exp $ */

/* GRIB models */

#ifndef MODELS_H_
#define MODELS_H_

/* Generating Process or Model, Table A. of GRIB, The WMO Format ...
 * These numbers are specific to CENTER_NMC
 */
#define MODEL_UVPI	2	/* Ultra Violet Potential Index Model */
#define MODEL_SAT       5       /* Satellite Derived Precipitation and temperatures, from IR */
#define MODEL_SWF	10	/* Global Wind-Wave Forecast Model */
#define MODEL_LFM	19	/* Limited-area Fine Mesh (LFM) analysis*/
#define MODEL_SCA	25	/* Snow Cover Analysis */
#define MODEL_NGM	39	/* Nested Grid forecast Model (NGM) */
#define MODEL_GOI	42	/* Global Optimum Interpolation Analysis (GOI)
				   from "Aviation" run */
#define MODEL_FGOI	43	/* Global Optimum Interpolation Analysis (GOI)
				   from "Final" run */
#define MODEL_SST	44	/* Sea Surface Temperature Analysis */
#define MODEL_OAT	49	/* Ozone Analysis from TIROS Observations */
#define MODEL_OAN	52	/* Ozone Analysis from Nimbus 7 Observations */
#define MODEL_LFM4	53	/* LFM-Fourth Order Forecast Model */
#define MODEL_ROI	64	/* Regional Optimium Interpolation Analysis
				   (ROI) */
#define MODEL_AVN	68	/* 80 wave triangular, 18 layer Spectral
				   model from "Aviation" run */
#define MODEL_MRF	69	/* 80 wave triangular, 18 layer Spectral
				   model from "Medium Range Forecast" run */
#define MODEL_QLM	70	/* Quasi-Lagrangian Hurricane Model (QLM) */
#define MODEL_FOG	73	/* Fog Forecast model - Ocean Prod. Center */
#define MODEL_GMW	74	/* Gulf of Mexico Wind/Wave */
#define MODEL_GAW	75	/* Gulf of Alaska Wind/Wave */
#define MODEL_MRFB	76	/* Bias corrected Medium Range Forecast */
#define MODEL_AVN1	77	/* 126 wave triangular, 28 layer Spectral
				   model from "Aviation" run */
#define MODEL_MRF1	78	/* 126 wave triangular, 28 layer Spectral
				   model from "Medium Range Forecast" run */
#define MODEL_BCK	79	/* Backup from the previous run */
#define MODEL_T62	80	/* 62 wave triangular model for days 6 through
				   10 of from "Medium Range Forecast" run */
#define MODEL_ASSI	81	/* Spectral Statistical Interpolation (SSI)
				   analysis from "Aviation" run */
#define MODEL_FSSI	82	/* Spectral Statistical Interpolation (SSI)
				   analysis from "Final" run */
#define MODEL_ETA	83	/* ETA Model - 80 km version */
#define MODEL_ETA40	84	/* ETA Model - 40 km version */
#define MODEL_ETA30	85	/* ETA Model - 30 km version */
#define MODEL_MAPS	86	/* MAPS Model, FSL (Isentropic; 60km at 40N) */
#define MODEL_ENSMB	87	/* CAC Ensemble Forecasts from Spectral (ENSMB) */
#define MODEL_PWAV	88	/* Ocean Wave model with additional physics (PWAV) */
#define MODEL_ETA48	89	/* ETA Model - 48 km version */
#define MODEL_NWSRFS	150	/* NWS River Forecast System (NWSRFS) */
#define MODEL_NWSFFGS	151	/* NWS Flash Flood Guidance System (NWSFFGS) */
#define MODEL_W2PA	152	/* WSR-88D Stage II Precipitation Analysis */
#define MODEL_W3PA	153	/* WSR-88D Stage III Precipitation Analysis */

/* ECMWF Model numbers. These numbers are specific to CENTER_ECMWF. */
#define MODEL_T213	40	/* t213 31-level forecast model */

#ifdef __cplusplus
extern "C" char* modelname(int center, int model);
#elif defined(__STDC__)
extern char* modelname(int center, int model);
#else
#define const
extern char* modelname( /* int center, int model */ );
#endif

#endif /* MODELS_H_ */
