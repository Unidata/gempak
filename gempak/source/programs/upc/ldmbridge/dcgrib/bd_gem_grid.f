      BLOCK DATA bd_gem_grid
C*************************************************************************
C 
C  Information array for GEMPAK GRIB decoder Gribtogem version 2
C
C	INFO ARRAY ORDER:
C
C	1-6	= A-F grids (NMC id 21-26)
C	7	= G grid (NMC id 50)
C	8	= NMC id 5
C	9	= NMC id 6
C	10	= NMC id 27 (NH)
C	11	= NMC id 28 (SH)
C	12	= NMC id 100
C	13	= NMC id 101
C	14	= NMC id 104
C	15	= NMC id 105
C	16-19	= ECMWF id 1-4 (NH)
C	20-23	= ECMWF id 9-12 (SH)
C	24-27	= ECMWF id 5-8 (35N-35S)		
C	28	= ETA 80km Resolution id 211
C	29	= Grid 256-usually thickened NMC AVN thinned grids
C	30-33	= NMC id 61-64
C       34      = NMC id 87 US Maps/Ruc
C       35      = NMC id 212 40km ETA
C       36      = NMC id 207 Alaska grid
C       37      = NMC id 214 Alaska grid
C       38      = NMC id 2 Global 2.5
C       39      = NMC id 203 Alask AVN
C       40      = NMC id 215 20km ETA
C       41      = NMC id 94 ETA 29km 
C       42      = NMC id 96 ETA 48km
C       43      = NMC id 99 ETA 15km
C       44      = NMC id 192 ETA 10km West
C       45      = NMC id 196 ETA 10km East
C       46      = NMC id 3
C       47      = NMC 201
C       48	= NMC 202
C       49      = NMC 204
C       50	= NMC 205
C       51      = NMC 236 RUC2
C       52      = NMC 213 National CONUS double res
C       53	= NMC 103 N. Hemisphere polar stereographic 105W
C       54      = NMC 255, Use gds
C 
C Log
C Unknown
C P.Bruehl/Undiata	5/95	Gribtogem v.2.
C Chiz/Unidata		10/96	New grids
C***************************************************************
	INCLUDE 'gem_grid.inc'

C
C     nmc is the index into the info arrays for NMC grids
C
	DATA nmc/0, 38, 46, 0, 8, 9, 14*0, 1, 2, 3, 4, 5, 6, 10, 11, 
     &         21*0, 7, 10*0, 30, 31, 32, 33, 22*0, 34, 6*0, 41, 0,
     &         42, 2*0, 43, 12, 13, 0, 53, 14, 15, 86*0, 44, 3*0, 45,
     &         4*0, 47, 48, 39, 49, 50, 0, 36, 3*0, 28, 35, 52, 37, 40,
     &         20*0, 51, 18*0, 54, 29/
C
C     ecmwf is the index into the info arrays for ECMWF grids
C
	DATA ecmwf/16, 17, 18, 19, 24, 25, 26, 27, 20, 21, 22, 23,
     &             242*0,54,0/
C
	DATA ukm/0,38,46,251*0,54,0/
C
C     subgrid is a flag to indicate part of a larger grid
C
	DATA subgrid/6*.TRUE., 9*.FALSE., 12*.TRUE.,.FALSE.,.TRUE.,
     &	 	     4*.TRUE.,21*.FALSE./
C
C     tgrid_id is the grid id to check for compatibility
C
	DATA tgrid_id/4*1, 2*5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 8*16,
     &     4*20, 28, 29, 4*30, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
     &     44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54/
C
C     scol is the starting column for subgrids
C
	DATA scol/1, 37, 1, 37, 1, 1, 9*0, 109, 73, 37, 1,
     &     109, 73, 37, 1, 109, 73, 37, 1, 0, 9999, 1, 91, 1, 91, 21*0 /
C
C     srow is the starting row for subgrids 
C
	DATA srow/37, 37, 1, 1, 19, 1, 9*0, 4*37, 8*1, 0, 9999,
     &       46, 46, 1, 1, 21*0/
C
C     polepos is the position of the pole point (if applicable)
C	No longer needed, pole expansion done by {gribto}
C
C	DATA polepos/2*1333, 2*1, 1297, 1, 21*0, 0/
C
C     ngx is the number of longitude points in a large grid
C
	DATA ngx/4*73, 2*72, 9*0, 12*145, 0, 9999, 4*181, 21*0/
C
C     ngy is the number of latitude points in a large grid
C
	DATA ngy/4*73, 2*37, 9*0, 8*73, 4*29, 0, 9999, 4*91, 21*0/
C
C     rlatb is the bottom latitude of the grid
C
	DATA rlatb/6*-90.0, 20.0, 2*7.634903, -20.84062, 
     &     20.84062, 17.09924, 10.51680, -0.268327, 17.5288,
     &     8*-90.0, 4*-35.0, 12.190, -90., 4*-90., 22.8756,
     &     12.190, 2*42.085, -90., 19.132, 12.190, 9.678, -3.441,
     &     17.912, 29.366, 23.476, -90., -20.826, 7.838, -25.000,
     &     0.616, 16.28, 7.838, 22.405, -9999./
C
C     rlatt is the top latitude of the grid
C
	DATA rlatt/6*90.0, 60.0, 42.8876, 44.27935, -20.84062, 
     &     20.84062, 53.504, 44.48691, 32.7473, 53.7708,
     &     8*90.0, 4*35.0, 57.290, 90.,4*90., 46.0172, 57.290,
     &     2*63.976, 90., 57.634, 57.290, 53.708, 50.026, 45.508,
     &     49.150, 43.337, 90.,-20.826,35.617,60.644,45.620,55.48,
     &     35.617, 48.920, -9999. /

C
C     rlonl is the left longitude of the grid
C
	DATA rlonl/6*0.0, -140.0, 2*-133.4429, -125.0, 125.0,
     &  -129.2958, -137.1459, -139.475, -129.296, 12*0.0, -133.459,
     &  -30.,4*0., -120.4911, -133.459, 2*-175.641, 0., 174.163,
     &  -133.459, -128.826, -148.799, -98.303, -126.316, -96.745,
     &  0.,-150.000,-141.028,110.0,-84.904,-126.14,-141.028,-121.352,
     &  -9999. /
C
C     rlonr is the right longitude of the grid
C
	DATA rlonr/4*0.0,2*355.00,-52.5,2.102730,-23.74616,55.0,-55.0,
     &  -22.37376,-16.57655,-14.5993,-22.3738,12*0.0,-49.385,-30.,4*0.,
     &  -60.8284, -49.385, 2*-93.689, -2.5, -53.66, -49.385, -35.567,
     &  -3.230, -62.002, -99.844, -72.401, -1.0,30.000,-18.576,-109.129,
     &  -15.000,-57.38,-18.577,-50.811, -9999. /
C
C     ang1, ang2, ang3 are the angles for the polar stereographic grids
C
	DATA ang1/7*0, 3*90.0, -90.0, 4*90.0, 12*0, 25.,5*0, 90.0, 25.,
     &            2*90.0, 0, 90., 25., 41., 50., 33., 40., 34., 0,
     &            2*90.0, 0, 90.,25.,90.,90., -9999. /
	DATA ang2/7*0, 2*-105.0, -80.0, 100.0, 4*-105.0, 12*0, -95.,5*0,
     &       -105.0, -95., 2*-150.0,0,-150.,-95.,-97., -111., -83., 
     &       -115., -86., 0, 2*-105., 0., -60.,-95.,2*-105., -9999. /
	DATA ang3/27*0, 25., 5*0, 0, 25., 4*0, 25., 10*0,25.,2*0,-9999. /
C
C     proj is the projection type
C
	DATA proj/7*'CED',3*'STR','SPS',4*'STR',12*'CED','LCC',5*'CED',
     &            'STR', 'LCC', 2*'STR', 'CED', 'STR', 'LCC', 6*'CED',
     &            2*'STR','MER','STR','LCC',2*'STR','xxx' /

      END
