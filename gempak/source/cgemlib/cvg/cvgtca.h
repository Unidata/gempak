/*****************************************************************
 * cvgtca.h                                                      *
 *                                                               *
 * This header file defines the tca tags used for encoding and   *
 * decoding data in TcaInfo and TcaFileInfo structures.          *
 *                                                               *
 **                                                              *
 * Log:                                                          *
 * B. Yin/SAIC		02/04	Created                          *
 * B. Yin/SAIC		03/04	Added storm type                 *
 * B. Yin/SAIC		07/04	Added break point number         *
 * B. Yin/SAIC		12/04	Added time zone		         *
 * B. Yin/SAIC		04/05	Added issue status, removed year *
 * S. Gilbert/NCEP	12/05	Added Text lat, lon, font, size  *
 *                              and width                        *
 * m.gamazaychikov/SAIC	05/07	Add TCA_FCSTPRD_TAG		 *
 ****************************************************************/

#define TCA_STORMNUM_TAG	"tca_stormNum"
#define TCA_ISSUESTATUS_TAG	"tca_issueStatus"
#define TCA_BASIN_TAG		"tca_basin"
#define TCA_ADVISORYNUM_TAG	"tca_advisoryNum"
#define TCA_STORMNAME_TAG	"tca_stormName"
#define TCA_STORMTYPE_TAG	"tca_stormType"
#define TCA_VALIDTIME_TAG	"tca_validTime"
#define TCA_FCSTPRD_TAG		"tca_period"
#define TCA_WWNUM_TAG		"tca_wwNum"
#define TCA_TCAWWSTR_TAG	"tca_tcawwStr"
#define TCA_BREAKPTS_TAG	"tca_breakPts"
#define TCA_NUMBKPTS_TAG	"tca_numBreakPts"
#define TCA_TIMEZONE_TAG	"tca_timezone"
#define TCA_TEXTLAT_TAG		"tca_textLat"
#define TCA_TEXTLON_TAG		"tca_textLon"
#define TCA_TEXTFONT_TAG	"tca_textFont"
#define TCA_TEXTSIZE_TAG        "tca_textSize"
#define TCA_TEXTWIDTH_TAG	"tca_textWidth"

/* Tags for TcTrack element */
#define TCT_ADVDATE_TAG		"tct_advDate"
#define TCT_TAU_TAG		"tct_tau"
#define TCT_MXWND_TAG		"tct_mxWnd"
#define TCT_WGUST_TAG		"tct_wGust"
#define TCT_MSLP_TAG		"tct_mslp"
#define TCT_TCDV_TAG		"tct_tcDv"
#define TCT_TVDVLBL_TAG		"tct_tcDvLbl"
#define TCT_TCDIR_TAG		"tct_tcDir"
#define TCT_TCSPD_TAG		"tct_tcSpd"
#define TCT_DTLBL_TAG		"tct_dtLbl"
#define TCT_TRKPNT_TAG		"tct_trkPnt"
#define TCT_NUMTRKPTS_TAG	"tct_numTrkPts"
