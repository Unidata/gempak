/*
 *   Copyright 1995, University Corporation for Atmospheric Research.
 *   See ../COPYRIGHT file for copying and redistribution conditions.
 */
/* "$Id: params.h,v 1.10 1995/12/12 23:28:31 russ Exp $" */

/* GRIB Ed 1, Table 2, Parameters and Units, Version 1, from Stackpole */
#define PARM_RESERVED	0	/* Reserved */
#define PARM_PRESSURE	1	/* Pressure, Pa */
#define PARM_PMSL	2	/* Pressure reduced to MSL, Pa */
#define PARM_PTND	3	/* Pressure tendency, Pa/s */
#define PARM_GPT	6	/* Geopotential, m2/s2 */
#define PARM_GPT_HGT	7	/* Geopotential height, gpm */
#define PARM_GEOM_HGT	8	/* Geometric height, m */

#define PARM_HSTDV	9	/* Standard deviation of height, m */
#define PARM_TOTOZ	10	/* Total Ozone, Dobsons */
#define PARM_TEMP	11	/* Temperature, deg. K */
#define PARM_VTEMP	12	/* Virtual temperature, deg. K */
#define PARM_POT_TEMP	13	/* Potential temperature, deg. K */
#define PARM_APOT_TEMP	14	/* Pseudo-adiabatic potential temperature, deg. K */
#define PARM_MAX_TEMP	15	/* Maximum temperature, deg. K */
#define PARM_MIN_TEMP	16	/* Minimum temperature, deg. K */
#define PARM_DP_TEMP	17	/* Dew point temperature, deg. K */
#define PARM_DP_DEP	18	/* Dew point depression (or deficit), deg. K */
#define PARM_LAPSE	19	/* Lapse rate, deg. K/m */

#define PARM_VIS	20	/* Visibility, m */

#define PARM_RAD1	21	/* Radar Spectra, direction and frequency */
#define PARM_RAD2	22	/* Radar Spectra, direction and radial num */
#define PARM_RAD3	23	/* Radar Spectra, radial num and radial num */
#define PARM_TOZ	24	/* Total Ozone */
#define PARM_TANOM	25	/* Temperature anomaly, deg. K */
#define PARM_PANOM	26	/* Pressure anomaly, Pa */
#define PARM_ZANOM	27	/* Geopotential height anomaly, gpm */
#define PARM_WAV1	28	/* Wave Spectra, direction and frequency */
#define PARM_WAV2	29	/* Wave Spectra, direction and radial num */
#define PARM_WAV3	30	/* Wave Spectra, radial num and radial num */
#define PARM_WND_DIR	31	/* Wind direction, deg. true */
#define PARM_WND_SPEED	32	/* Wind speed, m/s */
#define PARM_U_WIND	33	/* u-component of wind, m/s */
#define PARM_V_WIND	34	/* v-component of wind, m/s */
#define PARM_STRM_FUNC	35	/* Stream function, m2/s */
#define PARM_VPOT	36	/* Velocity potential, m2/s */

#define PARM_MNTSF	37	/* Montgomery stream function, m2/s2 */

#define PARM_SIG_VEL	38	/* Sigma coord. vertical velocity, /s */
#define PARM_VERT_VEL	39	/* Pressure Vertical velocity, Pa/s */
#define PARM_GEOM_VEL	40	/* Geometric Vertical velocity, m/s */
#define PARM_ABS_VOR	41	/* Absolute vorticity, /s */
#define PARM_ABS_DIV	42	/* Absolute divergence, /s */
#define PARM_REL_VOR	43	/* Relative vorticity, /s */
#define PARM_REL_DIV	44	/* Relative divergence, /s */
#define PARM_U_SHR	45	/* Vertical u-component shear, /s */
#define PARM_V_SHR	46	/* Vertical v-component shear, /s */
#define PARM_CRNT_DIR	47	/* Direction of current, deg. true */
#define PARM_CRNT_SPD	48	/* Speed of current, m/s */
#define PARM_U_CRNT	49	/* u-component of current, m/s */
#define PARM_V_CRNT	50	/* v-component of current, m/s */
#define PARM_SPEC_HUM	51	/* Specific humidity, kg/kg */
#define PARM_REL_HUM	52	/* Relative humidity, % */
#define PARM_HUM_MIX	53	/* Humidity mixing ratio, kg/kg */
#define PARM_PR_WATER	54	/* Precipitable water, kg/m2 */
#define PARM_VAP_PR	55	/* Vapor pressure, Pa */
#define PARM_SAT_DEF	56	/* Saturation deficit, Pa */
#define PARM_EVAP	57	/* Evaporation, kg/m2 */

#define PARM_C_ICE	58	/* Cloud Ice, kg/m2 */

#define PARM_PRECIP_RT	59	/* Precipitation rate, kg/m2/s */
#define PARM_THND_PROB	60	/* Thunderstorm probability, % */
#define PARM_PRECIP_TOT	61	/* Total precipitation, kg/m2 */
#define PARM_PRECIP_LS	62	/* Large scale precipitation, kg/m2 */
#define PARM_PRECIP_CN	63	/* Convective precipitation, kg/m2 */
#define PARM_SNOW_RT	64	/* Snowfall rate water equivalent, kg/m2s */
#define PARM_SNOW_WAT	65	/* Water equiv. of accum. snow depth, kg/m2 */
#define PARM_SNOW	66	/* Snow depth, m */
#define PARM_MIXED_DPTH	67	/* Mixed layer depth, m */
#define PARM_TT_DEPTH	68	/* Transient thermocline depth, m */
#define PARM_MT_DEPTH	69	/* Main thermocline depth, m */
#define PARM_MTD_ANOM	70	/* Main thermocline anomaly, m */
#define PARM_CLOUD	71	/* Total cloud cover, % */
#define PARM_CLOUD_CN	72	/* Convective cloud cover, % */
#define PARM_CLOUD_LOW	73	/* Low cloud cover, % */
#define PARM_CLOUD_MED	74	/* Medium cloud cover, % */
#define PARM_CLOUD_HI	75	/* High cloud cover, % */
#define PARM_CLOUD_WAT	76	/* Cloud water, kg/m2 */

#define PARM_SNO_C	78	/* Convective snow, kg/m2 */
#define PARM_SNO_L	79	/* Large scale snow, kg/m2 */

#define PARM_SEA_TEMP	80	/* sea temperature in degrees K */
#define PARM_LAND_MASK	81	/* Land-sea mask (1=land; 0=sea), 1/0 */
#define PARM_SEA_MEAN	82	/* Deviation of sea level from mean, m */
#define PARM_SRF_RN	83	/* Surface roughness, m */
#define PARM_ALBEDO	84	/* Albedo, % */
#define PARM_SOIL_TEMP	85	/* Soil temperature, deg. K */
#define PARM_SOIL_MST	86	/* Soil moisture content, kg/m2 */
#define PARM_VEG	87	/* Vegetation, % */
#define PARM_SAL	88	/* Salinity, kg/kg */
#define PARM_DENS	89	/* Density, kg/m3 */

#define PARM_WATR	90	/* Water runoff, kg/m2 */

#define PARM_ICE_CONC	91	/* Ice concentration (ice=l; no ice=O), 1/0 */
#define PARM_ICE_THICK	92	/* Ice thickness, m */
#define PARM_ICE_DIR	93	/* Direction of ice drift, deg. true */
#define PARM_ICE_SPD	94	/* Speed of ice drift, m/s */
#define PARM_ICE_U	95	/* u-component of ice drift, m/s */
#define PARM_ICE_V	96	/* v-component of ice drift, m/s */
#define PARM_ICE_GROWTH	97	/* Ice growth, m */
#define PARM_ICE_DIV	98	/* Ice divergence, /s */

#define PARM_SNO_M	99	/* Snow melt, kg/m2 */

#define PARM_WAVE_HGT	100	/* Significant height of combined wind waves and swell, m */
#define PARM_SEA_DIR	101	/* Direction of wind waves, deg. true */
#define PARM_SEA_HGT	102	/* Significant height of wind waves, m */
#define PARM_SEA_PER	103	/* Mean period of wind waves, s */
#define PARM_SWELL_DIR	104	/* Direction of swell waves, deg. true */
#define PARM_SWELL_HGT	105	/* Significant height of swell waves, m */
#define PARM_SWELL_PER	106	/* Mean period of swell waves, s */
#define PARM_WAVE_DIR	107	/* Primary wave direction, deg. true */
#define PARM_WAVE_PER	108	/* Primary wave mean period, s */
#define PARM_WAVE2_DIR	109	/* Secondary wave direction, deg. true */
#define PARM_WAVE2_PER	110	/* Secondary wave mean period, s */
#define PARM_RDN_SWSRF	111	/* Net shortwave radiation (surface), W/m2 */
#define PARM_RDN_LWSRF	112	/* Net longwave radiation (surface), W/m2 */
#define PARM_RDN_SWTOP	113	/* Net shortwave radiation (top of atmos.), W/m2 */
#define PARM_RDN_LWTOP	114	/* Net longwave radiation (top of atmos.), W/m2 */
#define PARM_RDN_LW	115	/* Long wave radiation, W/m2 */
#define PARM_RDN_SW	116	/* Short wave radiation, W/m2 */
#define PARM_RDN_GLBL	117	/* Global radiation, W/m2 */
#define PARM_LAT_HT	121	/* Latent heat flux, W/m2 */
#define PARM_SEN_HT	122	/* Sensible heat flux, W/m2 */
#define PARM_BL_DISS	123	/* Boundary layer dissipation, W/m2 */

#define PARM_U_FLX	124	/* Momentum flux, u component, N/m2 */
#define PARM_V_FLX	125	/* Momentum flux, v component, N/m2 */
#define PARM_WMIXE	126	/* Wind mixing energy, J */

#define PARM_IMAGE	127	/* Image data */
#define PARM_MSLSA	128	/* Mean sea level pressure (std. atms. reduction), Pa */
#define PARM_PM		129	/* Mean sea level pressure (MAPS system reduction), Pa */
#define PARM_MSLET	130	/* Mean sea level pressure (ETA model reduction), Pa */
#define PARM_LIFT_INDX	131	/* Surface lifted index, Deg. K */
#define PARM_LIFT_INDX4	132	/* Best (4 layer) lifted index, Deg. K */
#define PARM_K_INDX	133	/* K index, Deg. K */
#define PARM_SW_INDX	134	/* Sweat index, Deg. K */
#define PARM_HM_DIV	135	/* Horizontal moisture divergence, kg/kg/s */
#define PARM_VERT_SSHR	136	/* Vertical speed shear, /s */
#define PARM_TSLSA	137	/* 3-hr pressure tendency, Std. Atmos. Reduction, Pa/s */
#define PARM_BVF_2	138	/* Brunt-Vaisala frequency (squared), 1/s2 */
#define PARM_PV_MW	139	/* Potential vorticity (density weighted), 1/s/m */
#define PARM_CRAIN	140	/* Categorical rain  (yes=1; no=0), non-dim */
#define PARM_CFRZRN	141	/* Categorical freezing rain  (yes=1; no=0), non-dim */
#define PARM_CICEPL	142	/* Categorical ice pellets  (yes=1; no=0), non-dim */
#define PARM_CSNOW	143	/* Categorical snow  (yes=1; no=0), non-dim */
#define PARM_COVMZ	150	/* Covariance between meridional and zonal components of wind, m2/s2 */
#define PARM_COVTZ	151	/* Covariance between temperature and zonal component of wind, K*m/s */
#define PARM_COVTM	152	/* Covariance between temperature and meridional component of wind, K*m/s */
#define PARM_GFLUX	155	/* Ground Heat Flux, W/m2 */
#define PARM_CIN	156	/* Convective inhibition, J/kg */
#define PARM_CAPE	157	/* Convective Available Potential Energy, J/kg */
#define PARM_TKE	158	/* Turbulent Kinetic Energy, J/kg */
#define PARM_CONDP	159	/* Condensation pressure of parcel lifted from indicated surface, Pa */
#define PARM_CSUSF	160	/* Clear Sky Upward Solar Flux, W/m2 */
#define PARM_CSDSF	161	/* Clear Sky Downward Solar Flux, W/m2 */
#define PARM_CSULF	162	/* Clear Sky upward long wave flux, W/m2 */
#define PARM_CSDLF	163	/* Clear Sky downward long wave flux, W/m2 */
#define PARM_CFNSF	164	/* Cloud forcing net solar flux, W/m2 */
#define PARM_CFNLF	165	/* Cloud forcing net long wave flux, W/m2 */
#define PARM_VBDSF	166	/* Visible beam downward solar flux, W/m2 */
#define PARM_VDDSF	167	/* Visible diffuse downward solar flux, W/m2 */
#define PARM_NBDSF	168	/* Near IR beam downward solar flux, W/m2 */
#define PARM_NDDSF	169	/* Near IR diffuse downward solar flux, W/m2 */
#define PARM_M_FLX	172	/* Momentum flux, N/m2 */
#define PARM_LMH	173	/* Mass point model surface, non-dim */
#define PARM_LMV	174	/* Velocity point model surface, non-dim */
#define PARM_MLYNO	175	/* Model layer number (from bottom up), non-dim */
#define PARM_NLAT	176	/* latitude (-90 to +90), deg */
#define PARM_ELON	177	/* east longitude (0-360), deg */
#define PARM_LPS_X	181	/* x-gradient of log pressure, 1/m */
#define PARM_LPS_Y	182	/* y-gradient of log pressure, 1/m */
#define PARM_HGT_X	183	/* x-gradient of height, m/m */
#define PARM_HGT_Y	184	/* y-gradient of height, m/m */
#define PARM_HELC	190	/* Storm relative helicity, m**2/s**2 */
#define PARM_USTM	196	/* u-component of storm motion, m/s */
#define PARM_VSTM	197	/* v-component of storm motion, m/s */
#define PARM_NOICE_WAT	201	/* Ice-free water surface, % */

#define PARM_DSWRF	204	/* downward short wave rad. flux, W/m2 */
#define PARM_DLWRF	205	/* downward long wave rad. flux, W/m2 */
#define PARM_UVPI	206	/* Ultra violet potential index, W/m2 */
#define PARM_MSTR_AVL	207	/* Moisture availability, % */
#define PARM_XCHG_COF	208	/* Exchange coefficient, (kg/m3)(m/s) */
#define PARM_NMIX_LYRS	209	/* No. of mixed layers next to surface, integer */

#define PARM_USWRF	211	/* upward short wave rad. flux, W/m2 */
#define PARM_ULWRF	212	/* upward long wave rad. flux, W/m2 */

#define PARM_CLOUD_NCN	213	/* Amount of non-convective cloud, % */

#define PARM_CPRAT	214	/* Convective Precipitation rate, kg/m2/s */
#define PARM_TTDIA	215	/* Temperature tendency by all physics, K/s */

#define PARM_RDN_TTND	216	/* Temperature tendency by all radiation, Deg.K/s */

#define PARM_TTPHY	217	/* Temperature tendency by non-radiation physics, K/s */
#define PARM_PREIX	218	/* precip.index(0.0-1.00)(see note), fraction */
#define PARM_TSD1D	219	/* Std. dev. of IR T over 1x1 deg area, K */

#define PARM_LN_PRES	220	/* Natural log of surface pressure, ln(kPa) */
#define PARM_GPT_HGT5	222	/* 5-wave geopotential height, gpm */

#define PARM_C_WAT	223	/* Plant canopy surface water, kg/m2 */
#define PARM_BMIXL	226	/* Blackadar's mixing length scale, m */
#define PARM_AMIXL	227	/* Asymptotic mixing length scale, m */
#define PARM_PEVAP	228	/* Potential evaporation, kg/m2 */
#define PARM_SNOHF	229	/* Snow phase-change heat flux, W/m2 */
#define PARM_MFLUX	231	/* Convective cloud mas flux, Pa/s */
#define PARM_DTRF	232	/* Downward total radiation flux, W/m2 */
#define PARM_UTRF	233	/* Upward total radiation flux, W/m2 */
#define PARM_BGRUN	234	/* Baseflow-groundwater runoff, kg/m2 */
#define PARM_SSRUN	235	/* Storm surface runoff, kg/m2 */
#define PARM_SNO_CVR	238	/* Snow cover, percent */
#define PARM_SNO_T	239	/* Snow temperature, K */
#define PARM_LRGHR	241	/* Large scale condensat. heat rate, K/s */
#define PARM_CNVHR	242	/* Deep convective heating rate, K/s */
#define PARM_CNVMR	243	/* Deep convective moistening rate, kg/kg/s */
#define PARM_SHAHR	244	/* Shallow convective heating rate, K/s */
#define PARM_SHAMR	245	/* Shallow convective moistening rate, kg/kg/s */
#define PARM_VDFHR	246	/* Vertical diffusion heating rate, K/s */
#define PARM_VDFUA	247	/* Vertical diffusion zonal acceleration, m/s2 */
#define PARM_VDFVA	248	/* Vertical diffusion meridional accel, m/s2 */
#define PARM_VDFMR	249	/* Vertical diffusion moistening rate, kg/kg/s */
#define PARM_SWHR	250	/* Solar radiative heating rate, K/s */
#define PARM_LWHR	251	/* long wave radiative heating rate, K/s */
#define PARM_CD		252	/* Drag coefficient, non-dim */
#define PARM_FRICV	253	/* Friction velocity, m/s */
#define PARM_RI		254	/* Richardson number, non-dim. */

#define PARM_MISSING	255	/* Missing */

/* GRIB 0 parameters that were deleted for GRIB edition 1 */
#define PARM_VERT_SHR	256	/* vertical wind shear in m/sec/km */
#define PARM_CON_PRECIP	257	/* convective precip. amount in mm */
#define PARM_PRECIP     258	/* precipitation amount in mm */
#define PARM_NCON_PRECIP 259	/* non-convective precip. amount in mm */
#define PARM_SST_WARM	260	/* afternoon SST warming in degrees C */
#define PARM_UND_ANOM	261	/* underwater temp. anomaly in degrees C */
#define PARM_SEA_TEMP_0	262	/* sea temperature in 0.1 degrees C */
#define PARM_PRESSURE_D 263	/* pressure in 10 pascals */
#define PARM_GPT_THICK	264	/* geopotential thickness in gpm */
#define PARM_GPT_HGT_D	265	/* geopotential height in gpm */
#define PARM_GEOM_HGT_D	266	/* geometrical height in m */
#define PARM_TEMP_D	267	/* temperature in 0.1 degrees C */
#define PARM_REL_HUM_D	268	/* relative humididty in 0.1 percent */
#define PARM_LIFT_INDX_D 269	/* stability (lifted) index in 0.1 degrees C */
#define PARM_REL_VOR_D	270	/* relative vorticity in 10**-6/sec */
#define PARM_ABS_VOR_D	271	/* absolute vorticity in 10**-6/sec */
#define PARM_VERT_VEL_D	272	/* vertical velocity in 10 pascals/sec */
#define PARM_SEA_TEMP_D	273	/* sea surface temperature in 0.01 degrees C */
#define PARM_SST_ANOM	274	/* SST anomaly in 0.1 degrees C */
#define PARM_QUAL_IND	275	/* quality indicators (for generating model) */
#define PARM_GPT_DEP	276	/* departure from climatological normal geopotential in gpm */
#define PARM_PRESSURE_DEP 277	/* departure from climatological normal pressure in 100 pascals */

#define PARM_LAST_ENTRY 278	/*** Keep this current if table is expanded.
				     Must be one more than last defined
				     parameter code. */

#ifdef __cplusplus
extern "C" char* grib_pname ( int param_code );
extern "C" char* grib_units ( int param_code );
extern "C" char* grib0_units ( int param_code );
extern "C" int grib_pcode ( char *pname );
extern "C" int param_code ( int grib_edition, int param );
extern "C" int sfcparam(int param);
extern "C" int mslparam(int param);
extern "C" int lisoparam(int param);
#elif defined(__STDC__)
extern char* grib_pname ( int param_code );
extern char* grib_units ( int param_code );
extern char* grib0_units ( int param_code );
extern int grib_pcode ( char *pname );
extern int param_code ( int grib_edition, int param );
extern int sfcparam(int param);
extern int mslparam(int param);
extern int lisoparam(int param);
#else
extern char* grib_pname (/* int param_code */);
extern char* grib_units (/* int param_code */);
extern char* grib0_units (/* int param_code */);
extern int grib_pcode (/* char *pname */);
extern int param_code (/* int grib_edition, int param */);
extern int sfcparam( /* int param */ );
extern int mslparam( /* int param */ );
extern int lisoparam( /* int param */ );
#endif
