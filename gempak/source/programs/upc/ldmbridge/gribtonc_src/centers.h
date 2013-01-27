/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: centers.h,v 1.2 1995/05/18 22:03:30 russ Exp $ */

/* National/International Originating Centers (Assigned by the WMO) */

#ifndef CENTERS_H_
#define CENTERS_H_
#define CENTER_NMC	7	/* US National Meteorological Center */
#define CENTER_NWSTG	8	/* US NWS Telecomms Gateway */
#define CENTER_NWSFS	9	/* US NWS Field Stations */

#define CENTER_JMA	34	/* Japanese Meteorological Agency, Tokyo */
#define CENTER_NHC	52	/* US National Hurricane Center, Miami */
#define CENTER_CMS	54	/* Canadian Meteorological Service, Montreal */
#define CENTER_USAF	57	/* US Air Force - Global Weather Center */
#define CENTER_FNOC	58	/* US Navy Fleet Numerical Ocean. Center */
#define CENTER_FSL	59	/* NOAA Forecast Systems Lab, Boulder CO */
#define CENTER_UKMET	74	/* U.K. Met Office, Bracknell */
#define CENTER_FR	85	/* French Weather Service - Toulouse */
#define CENTER_ESA	97	/* European Space Agency (ESA) */
#define CENTER_ECMWF	98	/* Eur. Ctr. for Medium-Range Weather
				   Forecasts, Reading */
#define CENTER_NL	99	/* De Bilt, Netherlands */

/* National Sub-Centers (Assigned by the Nation) */

#define CENTER_REANA	  1	/* NMC Re-Analysis Project */
#define CENTER_ABRFC	150	/* Arkansas-Red River RFC, Tulsa OK */
#define CENTER_AKFC	151	/* FC, Anchorage, AK */
#define CENTER_CBRFC	152	/* Colo. Basin RFC, Salt Lake City, UT */
#define CENTER_CNRFC	153	/* Cal-Nevada RFC, Sacramento, CA */
#define CENTER_LMRFC	154	/* Lower Mississippi RFC, Slidel, LA */
#define CENTER_MARFC	155	/* Mid Atlantic RFC, State College, PA */
#define CENTER_MBRFC	156	/* Missouri Basin RFC, Kansas City, MO */
#define CENTER_NCRFC	157	/* North Central RFC, Minneapolis, MN */
#define CENTER_NERFC	158	/* Northeast RFC, Hartford, CT */
#define CENTER_NWRFC	159	/* Northwest RFC, Portland, OR */
#define CENTER_OHRFC	160	/* Ohio Basin RFC, Cincinnati, OH */
#define CENTER_SERFC	161	/* Southeast RFC, Atlanta, GA */
#define CENTER_WGRFC	162	/* West Gulf RFC, Fort Worth, TX */
#define CENTER_OUN	170	/* Norman OK WFO */

#ifdef __cplusplus
extern "C" char* centername(int ii);
#elif defined(__STDC__)
extern char* centername(int ii);
#else
extern char* centername();
#endif

#endif /* CENTERS_H_ */
