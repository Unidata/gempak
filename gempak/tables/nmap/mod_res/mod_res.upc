!************************************************************************************************************************************************
!																		*
!	mod_res.upc																*
!																		*
!	This table contains additions to the NCEP distributed mod_res.tbl.				  					*
!	The columns are not in fixed locations.													*
!																		*
!		Column 1 - product name as it is to appear in NMAP,										*
!		Column 2 - path/filename to restore file for product in column 1,								*
!		Column 3 - group name under which the product is to appear,									*
!		Column 4 - list of models (separated by semicolons, no spaces!) to which product will apply.					*
!																		*
! S. Chiswell/Unidata	 7/01	NOAAPORT/IDD changes												*
!	meso --> eta212																*
!	eta20 --> eta215															*
!	added wseta;wrf, avn002, avn003														*
!	added dgex, eta218															*
!																		*
!	deleted from model lists: etap, avnp, mrfp, fnlp, etasaudi, mm5, rsm, nww3_gfdl, eta32x, eta32kf					*
!																		*
!	commented out: ghm;ghmnest, vaftad, tdl_ngm, tdl_mrf, nww3_gfdl										*
!																		*
!************************************************************************************************************************************************
!
! PRODUCT NAME                  PATH/FILENAME                                        GROUP        MODELS
!
!
! Unidata Additions
500mb_Q-G                       $NMAP_RESTORE/stnd/500mb_Q-G                         Q-G_Theory dgex;gfs;gfshd;gfs002;gfs003;gfsthin;nam;nam40;nam20;nam12;wseta;wrf;ngm;ukmet;ruc
Model_Comparison_AVN-UKM_500    $NMAP_RESTORE/modl/compare/ukmet_vs_avn_500mb_hght   comp         dgex;gfs;gfshd;gfs002;gfs003;gfsthin;ukmet
Model_Comparison_AVN-UKM-NOGAPS $NMAP_RESTORE/modl/compare/avn_ukmet_nogaps_500mb_hght   comp  dgex;gfs;gfshd;gfs002;gfs003;gfsthin;ukmet;nogaps
Precip_rate                     $NMAP_RESTORE/prcp/precip_rate                       precip     ruc2;ruc_20
spc_cape_cin                    $NMAP_RESTORE/misc/spc_cape_cin                      qpf        nam;nam40;nam20;nam12;wseta;wrf;dgex
!
!
! Radar Coded Message Grid
!
Radar_Reflectivity             $NMAP_RESTORE/radar/rcm/reflectivity                  radar        rcmg
!
!  NEXRAD Level III Mosaic
Radar_Reflectivity             $NMAP_RESTORE/radar/nexr/n0r                          radar        nexr;nexl;nexf;nex1k
Radar_Reflectivity_qc          $NMAP_RESTORE/radar/nexr/n0r_qc                       radar        nexr;nexl;nexf;nex1k
!
! Stability additions
Precipitable_Water             $NMAP_RESTORE/misc/precip_water_mslp                  stability    nam;nam40;nam20;nam12;wseta;wrf;dgex
PRECIP_1hr                     $NMAP_RESTORE/prcp/precip_1hr                         precip       wseta;wrf
PRECIP_3hr                     $NMAP_RESTORE/prcp/precip_3hr                         precip       gfs;gfshd;gfs003;wseta;wrf
!
! Ensemble additions
250mb_hght                     $NMAP_RESTORE/modl/ensemble/hght_250_ens              ensemble     ens;enshr
500mb_hght                     $NMAP_RESTORE/modl/ensemble/hght_500_ens              ensemble     ens;enshr
1000mb_hght                    $NMAP_RESTORE/modl/ensemble/hght_1000_ens             ensemble     ens;enshr
SFC_pmsl                       $NMAP_RESTORE/modl/ensemble/pmsl_sfc_ens              ensemble     ens;enshr
!
500mb_hght_avg_spread          $NMAP_RESTORE/modl/ensemble/500mb_hght_avg_spread     ensemble     gefs:01;gefs:02;gefs:03
SFC_precipitation_type_prob    $NMAP_RESTORE/modl/ensemble/sfc_precip_type           ensemble     gefs:01;gefs:02;gefs:03
250mb_hght_members             $NMAP_RESTORE/modl/ensemble/250mb_hght_members        ensemble     gefs:01;gefs:02;gefs:03
500mb_hght_members             $NMAP_RESTORE/modl/ensemble/500mb_hght_members        ensemble     gefs:01;gefs:02;gefs:03
SFC_pres_members               $NMAP_RESTORE/modl/ensemble/sfc_pres_members          ensemble     gefs:01;gefs:02;gefs:03
!
! SFC Objective analysis additions
pmsl                           $NMAP_RESTORE/obj/surface/pmsl                        OA_SFC       sfca
tmpc                           $NMAP_RESTORE/obj/surface/tmpc                        OA_SFC       sfca
!
! RTMA
RTMA_2m_temperature            $NMAP_RESTORE/modl/rtma/tmpc_2m                       basic_wx     rtma
! SREF
SREF_CAPE_precipitation          $NMAP_RESTORE/modl/sref/prob_precip_cape            misc         sref
SREF_CAPE_probability            $NMAP_RESTORE/modl/sref/prob_cape                   misc         sref
