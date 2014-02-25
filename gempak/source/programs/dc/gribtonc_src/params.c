/*
 *	Copyright 1991 University Corporation for Atmospheric Research
 *	         All copies to include this notice.
 */
/* "$Id: params.c,v 1.15 1995/12/12 23:28:26 russ Exp $" */

#include <string.h>
#include "params.h"

#ifdef __STDC__
static void init_lookup(void);
#endif

#ifndef STREQ
#define STREQ(a, b) (*(a) == *(b) && strcmp((a), (b)) == 0)
#endif

struct param_table {
    int code;			/* GRIB code for parameter */
    char* name;			/* parameter name for netCDF files */
    char* units;		/* GRIB units as understood by udunits(3) */
};

struct param_table ptable[] = {
    {PARM_RESERVED,	"reserved",	"none"},
    {PARM_PRESSURE,	"P",		"Pa"},
    {PARM_PMSL,		"P_msl",	"Pa"},
    {PARM_PTND,		"Ptend",	"Pa/s"},
    {PARM_GPT,		"gpt",		"m2/s2"},
    {PARM_GPT_HGT,	"Z",		"gp m"},
    {PARM_GEOM_HGT,	"alt",		"m"},

    {PARM_HSTDV,	"hstdv",	"m"},
    {PARM_TOTOZ,	"totoz",	"Dobson"},
    {PARM_TEMP,		"T",		"degK"},
    {PARM_VTEMP,	"Tv",		"degK"},
    {PARM_POT_TEMP,	"theta",	"degK"},
    {PARM_APOT_TEMP,	"thpa",		"degK"},
    {PARM_MAX_TEMP,	"Tmax",		"degK"},
    {PARM_MIN_TEMP,	"Tmin",		"degK"},
    {PARM_DP_TEMP,	"TD",		"degK"},
    {PARM_DP_DEP,	"T_TD",		"degK"},
    {PARM_LAPSE,	"dTdz",		"degK/m"},

    {PARM_VIS,		"vis",		"m"},

    {PARM_RAD1,		"radspec_df",	"none"},
    {PARM_RAD2,		"radspec_dr",	"none"},
    {PARM_RAD3,		"radspec_rr",	"none"},
    {PARM_TOZ,		"toto3",	"Dobson"},
    {PARM_TANOM,	"Tdev",		"degK"},
    {PARM_PANOM,	"Pdev",		"Pa"},
    {PARM_ZANOM,	"Zdev",		"gp m"},
    {PARM_WAV1,		"wavspec_df",	"none"},
    {PARM_WAV2,		"wavspec_dr",	"none"},
    {PARM_WAV3,		"wavspec_rr",	"none"},
    {PARM_WND_DIR,	"DIR",		"degrees_true"},
    {PARM_WND_SPEED,	"SPD",		"m/s"},
    {PARM_U_WIND,	"u",		"m/s"},
    {PARM_V_WIND,	"v",		"m/s"},
    {PARM_STRM_FUNC,	"strm_func",	"m2/s"},
    {PARM_VPOT,		"velpot",	"m2/s"},

    {PARM_MNTSF,	"mntsf",	"m2/s2"},

    {PARM_SIG_VEL,	"sigvvel",	"1/s"},
    {PARM_VERT_VEL,	"omega",	"Pa/s"},
    {PARM_GEOM_VEL,	"w",		"m/s"},
    {PARM_ABS_VOR,	"absvor",	"1/s"},
    {PARM_ABS_DIV,	"absdiv",	"1/s"},
    {PARM_REL_VOR,	"relvor",	"1/s"},
    {PARM_REL_DIV,	"reldiv",	"1/s"},
    {PARM_U_SHR,	"dudz",		"1/s"},
    {PARM_V_SHR,	"dvdz",		"1/s"},
    {PARM_CRNT_DIR,	"crnt_dir",	"degrees_true"},
    {PARM_CRNT_SPD,	"crnt_spd",	"m/s"},
    {PARM_U_CRNT,	"u_crnt",	"m/s"},
    {PARM_V_CRNT,	"v_crnt",	"m/s"},
    {PARM_SPEC_HUM,	"spec_hum",	"kg/kg"},
    {PARM_REL_HUM,	"RH",		"percent"},
    {PARM_HUM_MIX,	"hum_mix",	"kg/kg"},
    {PARM_PR_WATER,	"pr_water",	"kg/m2"},
    {PARM_VAP_PR,	"E",		"Pa"},
    {PARM_SAT_DEF,	"sat_def",	"Pa"},
    {PARM_EVAP,		"evap",		"kg/m2"},

    {PARM_C_ICE,	"c_ice",	"kg/m2"},

    {PARM_PRECIP_RT,	"precip_rt",	"kg/(m2 s)"},
    {PARM_THND_PROB,	"thnd_prob",	"percent"},
    {PARM_PRECIP_TOT,	"PRECIP",	"kg/m2"},
    {PARM_PRECIP_LS,	"precip_ls",	"kg/m2"},
    {PARM_PRECIP_CN,	"precip_cn",	"kg/m2"},
    {PARM_SNOW_RT,	"snow_rt",	"kg/m2/s"},
    {PARM_SNOW_WAT,	"snow_wat",	"kg/m2"},
    {PARM_SNOW,		"snow",		"m"},
    {PARM_MIXED_DPTH,	"mixed_dpth",	"m"},
    {PARM_TT_DEPTH,	"tt_depth",	"m"},
    {PARM_MT_DEPTH,	"mt_depth",	"m"},
    {PARM_MTD_ANOM,	"mtd_anom",	"m"},
    {PARM_CLOUD,	"N",		"percent"},
    {PARM_CLOUD_CN,	"Nc",		"percent"},
    {PARM_CLOUD_LOW,	"Nl",		"percent"},
    {PARM_CLOUD_MED,	"Nm",		"percent"},
    {PARM_CLOUD_HI,	"Nh",		"percent"},
    {PARM_CLOUD_WAT,	"cloud_wat",	"kg/m2"},

    {PARM_SNO_C,	"sno_c",	"kg/m2"},
    {PARM_SNO_L,	"sno_l",	"kg/m2"},

    {PARM_SEA_TEMP,	"SST",		"degK"},
    {PARM_LAND_MASK,	"land_mask",	"bit"},
    {PARM_SEA_MEAN,	"sea_mean",	"m"},
    {PARM_SRF_RN,	"srf_rn",	"m"},
    {PARM_ALBEDO,	"albedo",	"percent"},
    {PARM_SOIL_TEMP,	"T_soil",	"degK"},
    {PARM_SOIL_MST,	"soil_mst",	"kg/m2"},
    {PARM_VEG,		"veg",		"percent"},
    {PARM_SAL,		"sal",		"kg/kg"},
    {PARM_DENS,		"dens",		"kg/m3"},

    {PARM_WATR,		"watr",		"kg/m2"},

    {PARM_ICE_CONC,	"ice_conc",	"bit"},
    {PARM_ICE_THICK,	"ice_thick",	"m"},
    {PARM_ICE_DIR,	"ice_dir",	"degrees_true"},
    {PARM_ICE_SPD,	"ice_spd",	"m/s"},
    {PARM_ICE_U,	"ice_u",	"m/s"},
    {PARM_ICE_V,	"ice_v",	"m/s"},
    {PARM_ICE_GROWTH,	"ice_growth",	"m"},
    {PARM_ICE_DIV,	"ice_div",	"1/s"},

    {PARM_SNO_M,	"sno_m",	"kg/m2"},

    {PARM_WAVE_HGT,	"wave_hgt",	"m"},
    {PARM_SEA_DIR,	"sea_dir",	"degrees_true"},
    {PARM_SEA_HGT,	"sea_hgt",	"m"},
    {PARM_SEA_PER,	"sea_per",	"s"},
    {PARM_SWELL_DIR,	"swell_dir",	"degrees_true"},
    {PARM_SWELL_HGT,	"swell_hgt",	"m"},
    {PARM_SWELL_PER,	"swell_per",	"s"},
    {PARM_WAVE_DIR,	"wave_dir",	"degrees_true"},
    {PARM_WAVE_PER,	"wave_per",	"s"},
    {PARM_WAVE2_DIR,	"wave2_dir",	"degrees_true"},
    {PARM_WAVE2_PER,	"wave2_per",	"s"},
    {PARM_RDN_SWSRF,	"rdn_swsrf",	"W/m2"},
    {PARM_RDN_LWSRF,	"rdn_lwsrf",	"W/m2"},
    {PARM_RDN_SWTOP,	"rdn_swtop",	"W/m2"},
    {PARM_RDN_LWTOP,	"rdn_lwtop",	"W/m2"},
    {PARM_RDN_LW,	"rdn_lw",	"W/m2"},
    {PARM_RDN_SW,	"rdn_sw",	"W/m2"},
    {PARM_RDN_GLBL,	"rdn_glbl",	"W/m2"},
    {PARM_LAT_HT,	"lat_ht",	"W/m2"},
    {PARM_SEN_HT,	"sen_ht",	"W/m2"},
    {PARM_BL_DISS,	"bl_diss",	"W/m2"},

    {PARM_U_FLX,	"u_flx",	"N/m2"},
    {PARM_V_FLX,	"v_flx",	"N/m2"},
    {PARM_WMIXE,	"wmixe",	"J"},

    {PARM_IMAGE,	"image",	"none"},
    {PARM_MSLSA,	"Psl_sa",	"Pa"},
    {PARM_PM,		"Pm"	,	"Pa"},
    {PARM_MSLET,	"Psl_et",	"Pa"},
    {PARM_LIFT_INDX,	"LI",		"degK"},
    {PARM_LIFT_INDX4,	"LI4",		"degK"},
    {PARM_K_INDX,	"Kind",		"degK"},
    {PARM_SW_INDX,	"sweat",	"degK"},
    {PARM_HM_DIV,	"mois_div",	"kg/kg/s"},
    {PARM_VERT_SSHR,	"vert_sshr",	"1/s"},
    {PARM_TSLSA,	"tslsa",	"Pa/s"},
    {PARM_BVF_2,	"bvf_2",	"1/s2"},
    {PARM_PV_MW,	"pv_mw",	"1/s/m"},
    {PARM_CRAIN,	"crain",	"bit"},
    {PARM_CFRZRN,	"cfrzrn",	"bit"},
    {PARM_CICEPL,	"cicepl",	"bit"},
    {PARM_CSNOW,	"csnow",	"bit"},
    {PARM_COVMZ,	"covmz",	"m2/s2"},
    {PARM_COVTZ,	"covtz",	"K*m/s"},
    {PARM_COVTM,	"covtm",	"K*m/s"},
    {PARM_GFLUX,	"gflux",	"W/m2"},
    {PARM_CIN,		"cin",		"J/kg"},
    {PARM_CAPE,		"cape",		"J/kg"},
    {PARM_TKE,		"tke",		"J/kg"},
    {PARM_CONDP,	"condp",	"Pa"},
    {PARM_CSUSF,	"csusf",	"W/m2"},
    {PARM_CSDSF,	"csdsf",	"W/m2"},
    {PARM_CSULF,	"csulf",	"W/m2"},
    {PARM_CSDLF,	"csdlf",	"W/m2"},
    {PARM_CFNSF,	"cfnsf",	"W/m2"},
    {PARM_CFNLF,	"cfnlf",	"W/m2"},
    {PARM_VBDSF,	"vbdsf",	"W/m2"},
    {PARM_VDDSF,	"vddsf",	"W/m2"},
    {PARM_NBDSF,	"nbdsf",	"W/m2"},
    {PARM_NDDSF,	"nddsf",	"W/m2"},
    {PARM_M_FLX,	"m_flx",	"N/m2"},
    {PARM_LMH,		"lmh",		"1"},
    {PARM_LMV,		"lmv",		"1"},
    {PARM_MLYNO,	"mlyno",	"1"},
    {PARM_NLAT,		"nlat",		"deg"},
    {PARM_ELON,		"elon",		"deg"},
    {PARM_LPS_X,	"lps_x",	"1/m"},
    {PARM_LPS_Y,	"lps_y",	"1/m"},
    {PARM_HGT_X,	"hgt_x",	"m/m"},
    {PARM_HGT_Y,	"hgt_y",	"m/m"},
    {PARM_HELC,		"helc",		"m2/s2"},
    {PARM_USTM,		"ustm",		"m/s"},
    {PARM_VSTM,		"vstm",		"m/s"},
    {PARM_NOICE_WAT,	"noice_wat",	"percent"},

    {PARM_DSWRF,	"dswrf",	"W/m2"},
    {PARM_DLWRF,	"dlwrf",	"W/m2"},
    {PARM_UVPI,		"uvpi",		"W/m2"},

    {PARM_MSTR_AVL,	"mstr_avl",	"%"},
    {PARM_XCHG_COF,	"xchg_cof",	"kg/m2/s"},
    {PARM_NMIX_LYRS,	"nmix_lyrs",	"1"},

    {PARM_USWRF,	"uswrf",	"W/m2"},
    {PARM_ULWRF,	"ulwrf",	"W/m2"},

    {PARM_CLOUD_NCN,	"cloud_ncn",	"%"},

    {PARM_CPRAT,	"cprat",	"kg/m2/s"},
    {PARM_TTDIA,	"ttdia",	"K/s"},

    {PARM_RDN_TTND,	"rdn_ttnd",	"degK/s"},

    {PARM_TTPHY,	"ttphy",	"K/s"},
    {PARM_PREIX,	"preix",	"fraction"},
    {PARM_TSD1D,	"tsd1d",	"K"},

    {PARM_LN_PRES,	"ln_pres",	"ln(kPa)"},
    {PARM_GPT_HGT5,	"gpt_hgt5",	"gp m"},

    {PARM_C_WAT,	"c_wat",	"kg/m2"},
    {PARM_BMIXL,	"bmixl",	"m"},
    {PARM_AMIXL,	"amixl",	"m"},
    {PARM_PEVAP,	"pevap",	"kg/m2"},
    {PARM_SNOHF,	"snohf",	"W/m2"},
    {PARM_MFLUX,	"mflux",	"Pa/s"},
    {PARM_DTRF,		"dtrf",		"W/m2"},
    {PARM_UTRF,		"utrf",		"W/m2"},
    {PARM_BGRUN,	"bgrun",	"kg/m2"},
    {PARM_SSRUN,	"ssrun",	"kg/m2"},
    {PARM_SNO_CVR,	"sno_cvr",	"percent"},
    {PARM_SNO_T,	"sno_t",	"K"},
    {PARM_LRGHR,	"lrghr",	"K/s"},
    {PARM_CNVHR,	"cnvhr",	"K/s"},
    {PARM_CNVMR,	"cnvmr",	"kg/kg/s"},
    {PARM_SHAHR,	"shahr",	"K/s"},
    {PARM_SHAMR,	"shamr",	"kg/kg/s"},
    {PARM_VDFHR,	"vdfhr",	"K/s"},
    {PARM_VDFUA,	"vdfua",	"m/s2"},
    {PARM_VDFVA,	"vdfva",	"m/s2"},
    {PARM_VDFMR,	"vdfmr",	"kg/kg/s"},
    {PARM_SWHR,		"swhr",		"K/s"},
    {PARM_LWHR,		"lwhr",		"K/s"},
    {PARM_CD,		"cd",		"non-dim"},
    {PARM_FRICV,	"fricv",	"m/s"},
    {PARM_RI,		"ri",		"non-dim."},

    {PARM_MISSING,	"missing",	"none"},

    /* GRIB Edition 0 */
    {PARM_VERT_SHR,	"vert_shr",	"m/sec/km"},
    {PARM_CON_PRECIP,	"con_precip",	"mm h2o/g"},
    {PARM_PRECIP,	"PRECIP",	"mm h2o/g"},
    {PARM_NCON_PRECIP,	"ncon_precip",	"mm h2o/g"},
    {PARM_SST_WARM,	"sst_warm",	"degC"},
    {PARM_UND_ANOM,	"und_anom",	"degC"},
    {PARM_SEA_TEMP_0,	"SST",		"0.1 degC"},
    {PARM_PRESSURE_D,	"pressure_d",	"10 pascals"},
    {PARM_GPT_THICK,	"gpt_thick",	"gp m"},
    {PARM_GPT_HGT_D,	"gpt_hgt_d",	"gp m"},
    {PARM_GEOM_HGT_D,	"geom_hgt_d",	"m"},
    {PARM_TEMP_D,	"temp_d",	"0.1 degC"},
    {PARM_REL_HUM_D,	"rel_hum_d",	"0.1 percent"},
    {PARM_LIFT_INDX_D,	"lift_indx_d",	"0.1 degC"},
    {PARM_REL_VOR_D,	"rel_vor_d",	"10**-6/sec"},
    {PARM_ABS_VOR_D,	"abs_vor_d",	"10**-6/sec"},
    {PARM_VERT_VEL_D,	"omega",	"10 pascals/sec"},
    {PARM_SEA_TEMP_D,	"sea_temp_d",	"0.01 degC"},
    {PARM_SST_ANOM,	"sst_anom",	"0.1 degC"},
    {PARM_QUAL_IND,	"qual_ind",	"none"},
    {PARM_GPT_DEP,	"gpt_dep",	"gp m"},
    {PARM_PRESSURE_DEP,	"pressure_dep",	"100 pascals"},
    {PARM_LAST_ENTRY,   "reserved",     "none"}
};

#define PARM_TABLE_SIZE sizeof(ptable)/sizeof(ptable[0])

static int lookup[PARM_LAST_ENTRY + 1];	/* private lookup table for speed */
static int lookup_initialized = 0; /* gets set to 1 when lookup initialized */

static void
init_lookup() {			/* initialize fast lookup table from the
				 * parameter table */
    if (! lookup_initialized) {
	int i;
	for (i=0; i < PARM_TABLE_SIZE; i++)
	  lookup[ptable[i].code] = i;
	lookup_initialized = 1;
    }
}


/*
 * Given the parameter code from grib data, returns a netCDF variable
 * name chosen for this parameter.  Returns NULL if not found.
 */
char *
grib_pname(param)
     int param;			/* parameter code from GRIB data */
{
    if (! lookup_initialized)
	init_lookup();

    if (param < 0 || param >= PARM_LAST_ENTRY)
      return 0;

    return ptable[lookup[param]].name;
}

/*
 * Given the netCDF variable name for a GRIB parameter, returns the GRIB
 * parameter code (the inverse of the grib_pname() mapping).  Since we only do
 * this rarely, it uses a linear search.  Returns -1 if name doesn't match any
 * GRIB parameter names.
 * Comparison is made *without* any level suffixes we have may added
 * (e.g. "sfc", "trop", "maxwind").
 */
int
grib_pcode(pname)
     char *pname;		/* parameter name */
{
    int i = strlen(pname);
#define PARAM_NAME_LEN_MAX 128
    char pname2[PARAM_NAME_LEN_MAX];		
    static char *suffixes[] = {	/* list of suffixes returned from
				   levelsuffix() function */
	"_sfc",			/* surface of the Earth */
	"_clbs",		/* cloud base level */
	"_cltp",		/* cloud top level */
	"_frzlvl",		/* 0 degree isotherm level */
	"_adcn",		/* adiabatic condensation level */
	"_maxwind",		/* maximium wind speed level */
	"_trop",		/* at the tropopause */
	"_topa",		/* nominal top of atmosphere */
	"_sbot",		/* sea bottom */
	"_liso",		/* layer between two isobaric levels */
	"_msl",			/* mean sea level */
	"_fh",			/* fixed height level */
	"_lfhm",		/* layer between 2 height levels above MSL */
	"_fhg",			/* fixed height above ground */
	"_lfhg",		/* layer between 2 height levels above
				   ground */
	"_sigma",		/* sigma level */
	"_ls",			/* layer between 2 sigma levels */
	"_hybr",		/* Hybrid level */
	"_lhyb",		/* Layer between 2 hybrid levels */
	"_bls",			/* Depth below land surface */
	"_lbls",		/* Layer between 2 depths below land surface */
	"_isen",		/* Isentropic (theta) level */
	"_lisn",		/* Layer between 2 isentropic (theta) levels */
	"_pdg",			/* level at specified pressure difference from
				   ground */
	"_lpdg",		/* layer between levels at specif. pressure
				   diffs from ground */
	"_pv",			/* level of specified potential vorticity */
	"_lish",		/* layer between 2 isobaric surfaces (high
				   precision) */
	"_fhgh",		/* height level above ground (high
				   precision) */
	"_lsh",			/* layer between 2 sigma levels (high
				   precision) */
	"_lism",		/* layer between 2 isobaric surfaces (mixed
				   precision) */
	"_dbs",			/* depth below sea level */
	"_atm",			/* entire atmosphere considered as a single
				   layer */
	"_ocn",			/* entire ocean considered as a single layer */
    };
    int suf;
    
    if (STREQ(pname, "P_msl"))	/* exceptional case, "P" and "P_msl" are
				   2 different GRIB parameters */
	return PARM_PMSL;

    strncpy(pname2,pname,PARAM_NAME_LEN_MAX);
    for (suf = 0; suf < sizeof(suffixes)/sizeof(suffixes[0]); suf++) {
	int lensuf = strlen(suffixes[suf]);
	if (strcmp(pname2+i-lensuf, suffixes[suf]) == 0) /* strip off suffix */
	    pname2[i-lensuf] = '\0';
    }

    for(i=0; i < PARM_TABLE_SIZE; i++)
	if (STREQ(pname2, ptable[i].name)) {
	    return ptable[i].code;
	}
    return -1;
}


/*
 * Given the parameter code from grib data, returns GRIB units string for this
 * parameter.  Returns NULL if not found.
 */
char *
grib_units(param)
     int param;			/* parameter code from GRIB data */
{
    if (! lookup_initialized)
	init_lookup();

    if (param < 0 || param >= PARM_LAST_ENTRY)
      return 0;

    return ptable[lookup[param]].units;
}


/*
 * Maps (GRIB edition, parameter code) pair into a unique parameter code,
 * based on the GRIB edition 1 codes.  This is necessary because the meaning
 * of the parameter tables (Table 5 of StackPole's GRIB 0 descritpion, Table
 * 2 of the GRIB 1 description) completely changed between editions.
 */
int
param_code(grib_edition, param)
     int grib_edition;
     int param;
{
    switch (grib_edition) {
      case 1:
	return param;
      case 0:			/* soon to be obsolete ... */
	switch (param) {
	  case 1: return PARM_PRESSURE;
	  case 2: return PARM_GPT_HGT;
	  case 3: return PARM_GEOM_HGT;
	  case 4: return PARM_TEMP;
	  case 5: return PARM_MAX_TEMP;
	  case 6: return PARM_MIN_TEMP;
	  case 8: return PARM_POT_TEMP;
	  case 10: return PARM_DP_TEMP;
	  case 11: return PARM_DP_DEP;
	  case 12: return PARM_SPEC_HUM;
	  case 13: return PARM_REL_HUM;
	  case 14: return PARM_HUM_MIX;
	  case 15: return PARM_LIFT_INDX;
	  case 17: return PARM_LIFT_INDX4;
	  case 21: return PARM_WND_SPEED;
	  case 23: return PARM_U_WIND;
	  case 24: return PARM_V_WIND;
	  case 29: return PARM_STRM_FUNC;
	  case 30: return PARM_REL_VOR;
	  case 31: return PARM_ABS_VOR;
	  case 40: return PARM_VERT_VEL;
	  case 44: return PARM_VERT_SHR;
	  case 47: return PARM_PR_WATER;
	  case 48: return PARM_CON_PRECIP;
	  case 50: return PARM_PRECIP;
	  case 51: return PARM_SNOW;
	  case 55: return PARM_NCON_PRECIP;
	  case 58: return PARM_SST_WARM;
	  case 59: return PARM_UND_ANOM;
	  case 61: return PARM_SEA_TEMP_0;
	  case 64: return PARM_WAVE_HGT;
	  case 65: return PARM_SWELL_DIR;
	  case 66: return PARM_SWELL_HGT;
	  case 67: return PARM_SWELL_PER;
	  case 68: return PARM_SEA_DIR;
	  case 69: return PARM_SEA_HGT;
	  case 70: return PARM_SEA_PER;
	  case 75: return PARM_WAVE_DIR;
	  case 76: return PARM_WAVE_PER;
	  case 77: return PARM_WAVE2_DIR;
	  case 78: return PARM_WAVE2_PER;
	  case 90: return PARM_ICE_CONC;
	  case 91: return PARM_ICE_THICK;
	  case 92: return PARM_ICE_U;
	  case 93: return PARM_ICE_V;
	  case 94: return PARM_ICE_GROWTH;
	  case 95: return PARM_ICE_DIV;
	  case 100: return PARM_PRESSURE_D;
	  case 101: return PARM_GPT_THICK;
	  case 102: return PARM_GPT_HGT_D;
	  case 103: return PARM_GEOM_HGT_D;
	  case 104: return PARM_TEMP_D;
	  case 113: return PARM_REL_HUM_D;
	  case 115: return PARM_LIFT_INDX_D;
	  case 130: return PARM_REL_VOR_D;
	  case 131: return PARM_ABS_VOR_D;
	  case 141: return PARM_VERT_VEL_D;
	  case 162: return PARM_SEA_TEMP_D;
	  case 163: return PARM_SST_ANOM;
	  case 180: return PARM_MIXED_DPTH;
	  case 181: return PARM_TT_DEPTH;
	  case 182: return PARM_MT_DEPTH;
	  case 183: return PARM_MTD_ANOM;
	  case 190: return PARM_QUAL_IND;
	  case 210: return PARM_GPT_DEP;
	  case 211: return PARM_PRESSURE_DEP;
	  default:
	    return -1;
	}
      default:
	return -1;
    }
    /* NOT REACHED */
}


/*
 * Given the parameter code from grib data for a GRIB edition 0 parameter,
 * returns a units string for the units used in GRIB edition 0 for this
 * parameter.  Returns NULL if not found.
 */
char *
grib0_units(param)
     int param;			/* GRIB 1 parameter code from GRIB 0 data */
{
    switch (param) {
      case PARM_PRESSURE: return "hectopascals" ;
      case PARM_GPT_HGT: return "geopotential dekameters" ;
      case PARM_GEOM_HGT: return "10 m" ;
      case PARM_TEMP: return "celsius" ;
      case PARM_MAX_TEMP: return "celsius" ;
      case PARM_MIN_TEMP: return "celsius" ;
      case PARM_POT_TEMP: return "celsius" ;
      case PARM_DP_TEMP: return "celsius" ;
      case PARM_DP_DEP: return "celsius" ;
      case PARM_SPEC_HUM: return "0.1 g/kg" ;
      case PARM_REL_HUM: return "percent" ;
      case PARM_HUM_MIX: return "0.1 g/kg " ;
      case PARM_LIFT_INDX: return "celsius" ;
      case PARM_LIFT_INDX4: return "celsius" ;
      case PARM_WND_SPEED: return "meters/second" ;
      case PARM_U_WIND: return "meters/second" ;
      case PARM_V_WIND: return "meters/second" ;
      case PARM_STRM_FUNC: return "100000 m2/sec" ;
      case PARM_REL_VOR: return ".00001/sec" ;
      case PARM_ABS_VOR: return ".00001/sec" ;
      case PARM_VERT_VEL: return "millibars/second" ;
      case PARM_VERT_SHR: return "meters/second/km" ;
      case PARM_PR_WATER: return "mm h2o/g" ;
      case PARM_CON_PRECIP: return "mm h2o/g" ;
      case PARM_PRECIP: return "mm h2o/g" ;
      case PARM_SNOW: return "cm" ;
      case PARM_NCON_PRECIP: return "mm h2o/g" ;
      case PARM_SST_WARM: return "celsius" ;
      case PARM_UND_ANOM: return "celsius" ;
      case PARM_SEA_TEMP_0: return "0.1 celsius" ;
      case PARM_WAVE_HGT: return "0.5 m" ;
      case PARM_SWELL_DIR: return "10 degrees" ;
      case PARM_SWELL_HGT: return "0.5 m" ;
      case PARM_SWELL_PER: return "second" ;
      case PARM_SEA_DIR: return "10 degrees" ;
      case PARM_SEA_HGT: return "0.5 m" ;
      case PARM_SEA_PER: return "second" ;
      case PARM_WAVE_DIR: return "10 degrees" ;
      case PARM_WAVE_PER: return "second" ;
      case PARM_WAVE2_DIR: return "10 degrees" ;
      case PARM_WAVE2_PER: return "second" ;
      case PARM_ICE_CONC: return "bit" ;
      case PARM_ICE_THICK: return "m" ;
      case PARM_ICE_U: return "km/day" ;
      case PARM_ICE_V: return "km/day" ;
      case PARM_ICE_GROWTH: return "0.1 meters" ;
      case PARM_ICE_DIV: return "1/sec" ;
      case PARM_PRESSURE_D: return "10 pascals" ;
      case PARM_GPT_THICK: return "gp m" ;
      case PARM_GPT_HGT_D: return "gp m" ;
      case PARM_GEOM_HGT_D: return "m" ;
      case PARM_TEMP_D: return "0.1 celsius" ;
      case PARM_REL_HUM_D: return "0.1 percent" ;
      case PARM_LIFT_INDX_D: return "0.1 celsius" ;
      case PARM_REL_VOR_D: return ".000001/sec" ;
      case PARM_ABS_VOR_D: return ".000001/sec" ;
      case PARM_VERT_VEL_D: return "10 pascals/sec" ;
      case PARM_SEA_TEMP_D: return "0.01 celsius" ;
      case PARM_SST_ANOM: return "0.1 celsius" ;
      case PARM_MIXED_DPTH: return "cm" ;
      case PARM_TT_DEPTH: return "cm" ;
      case PARM_MT_DEPTH: return "cm" ;
      case PARM_MTD_ANOM: return "cm" ;
      case PARM_QUAL_IND: return "" ;
      case PARM_PRESSURE_DEP: return "hectopascals" ;
      default:
	return 0;
    }
}


/*
 * Return true (1) if parameter is implicitly a surface parameter that does
 * not need a surface-level suffix
 */
int sfcparam(param)
    int param;
{
    switch (param) {
    case PARM_PRECIP_RT: 	/* Precipitation rate, kg/m2/s */
    case PARM_THND_PROB: 	/* Thunderstorm probability, % */
    case PARM_PRECIP_TOT: 	/* Total precipitation, kg/m2 */
    case PARM_PRECIP_LS: 	/* Large scale precipitation, kg/m2 */
    case PARM_PRECIP_CN: 	/* Convective precipitation, kg/m2 */
    case PARM_SNOW_RT:		/* Snowfall rate water equivalent, kg/m2s */
    case PARM_SNOW_WAT: 	/* Water equiv. of accum. snow depth, kg/m2 */
    case PARM_SNOW:		/* Snow depth, m */
    case PARM_SNO_C:		/* Convective snow, kg/m2 */
    case PARM_SNO_L:		/* Large scale snow, kg/m2 */
    case PARM_SEA_TEMP:		/* sea temperature in degrees K */
    case PARM_SRF_RN:		/* Surface roughness, m */
    case PARM_SOIL_TEMP: 	/* Soil temperature, deg. K */
    case PARM_SOIL_MST: 	/* Soil moisture content, kg/m2 */
    case PARM_VEG:		/* Vegetation, % */
    case PARM_DENS:		/* Density, kg/m3 */
    case PARM_WATR:		/* Water runoff, kg/m2 */
    case PARM_ICE_CONC: 	/* Ice concentration (ice=l; no ice=O), 1/0 */
    case PARM_ICE_THICK: 	/* Ice thickness, m */
    case PARM_ICE_DIR:		/* Direction of ice drift, deg. true */
    case PARM_ICE_SPD:		/* Speed of ice drift, m/s */
    case PARM_ICE_U:		/* u-component of ice drift, m/s */
    case PARM_ICE_V:		/* v-component of ice drift, m/s */
    case PARM_ICE_GROWTH: 	/* Ice growth, m */
    case PARM_ICE_DIV:		/* Ice divergence, /s */
    case PARM_SNO_M:		/* Snow melt, kg/m2 */
    case PARM_LIFT_INDX: 	/* Surface lifted index, Deg. K */
    case PARM_CRAIN:		/* Categorical rain  (yes=1; no=0), non-dim */
    case PARM_CFRZRN:		/* Categorical freezing rain  (yes=1; no=0), non-dim */
    case PARM_CICEPL:		/* Categorical ice pellets  (yes=1; no=0), non-dim */
    case PARM_CSNOW:		/* Categorical snow  (yes=1; no=0), non-dim */
    case PARM_GFLUX:		/* Ground Heat Flux, W/m2 */
    case PARM_NOICE_WAT: 	/* Ice-free water surface, % */
    case PARM_MSTR_AVL: 	/* Moisture availability, % */
    case PARM_NMIX_LYRS: 	/* No. of mixed layers next to surface, integer */
    case PARM_CPRAT:		/* Convective Precipitation rate, kg/m2/s */
    case PARM_PREIX:		/* precip.index(0.0-1.00)(see note), fraction */
    case PARM_LN_PRES:		/* Natural log of surface pressure, ln(kPa) */
    case PARM_C_WAT:		/* Plant canopy surface water, kg/m2 */
    case PARM_SNOHF:		/* Snow phase-change heat flux, W/m2 */
    case PARM_BGRUN:		/* Baseflow-groundwater runoff, kg/m2 */
    case PARM_SSRUN:		/* Storm surface runoff, kg/m2 */
    case PARM_SNO_CVR:		/* Snow cover, percent */
    case PARM_SNO_T:		/* Snow temperature, K */
    case PARM_CON_PRECIP: 	/* convective precip. amount in mm */
    case PARM_PRECIP:		/* precipitation amount in mm */
    case PARM_NCON_PRECIP: 	/* non-convective precip. amount in mm */
	return 1;
    }
    return 0;
}


/*
 * Return true (1) if parameter is implicitly a MSL parameter that does
 * not need a MSL-level suffix
 */
int mslparam(param)
    int param;
{
    switch (param) {
    case PARM_PMSL:		/* Pressure reduced to MSL, Pa */
    case PARM_MSLSA:		/* Mean sea level pressure (std. atms. reduction), Pa */
    case PARM_MSLET:		/* Mean sea level pressure (ETA model reduction), Pa */
    case PARM_WAVE_HGT:		/* Significant height of combined wind waves and swell, m */
    case PARM_SEA_DIR:		/* Direction of wind waves, deg. true */
    case PARM_SEA_HGT:		/* Significant height of wind waves, m */
    case PARM_SEA_PER:		/* Mean period of wind waves, s */
    case PARM_SWELL_DIR:	/* Direction of swell waves, deg. true */
    case PARM_SWELL_HGT:	/* Significant height of swell waves, m */
    case PARM_SWELL_PER:	/* Mean period of swell waves, s */
    case PARM_WAVE_DIR:		/* Primary wave direction, deg. true */
    case PARM_WAVE_PER:		/* Primary wave mean period, s */
    case PARM_WAVE2_DIR:	/* Secondary wave direction, deg. true */
    case PARM_WAVE2_PER:	/* Secondary wave mean period, s */
	return 1;
    }
    return 0;
}


/*
 * Return true (1) if parameter is implicitly a LISO (layer between two
 * isobaric levels) parameter that does not need a LISO-level suffix
 */
int lisoparam(param)
    int param;
{
    switch (param) {
    case PARM_LIFT_INDX:	/* Surface lifted index, Deg. K */
				/* *** Are there more of these? *** */
	return 1;
    }
    return 0;
}

