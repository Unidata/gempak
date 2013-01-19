	PROGRAM TESTPR
C************************************************************************
C* TESTPR								*
C*									*
C* This program tests the PARAMETER CONVERSION library functions.	*
C*									*
C**									*
C* Log:									*
C* I. Graffman/RDS	6/84						*
C* M. Goodman//RDS 	9/84						*
C* I. Graffman/RDS	12/87	Update for GEMPAK4			*
C* S. Schotz/GSC	10/89   Update for GEMPAK5			*
C* S. Schotz/GSC	 4/90   Added PR_NSYM and PR_GSYM		*
C* K. Brill/NMC		02/92	Removed PR_PTND				*
C* S. Jacobs/NCEP	 3/96	Added PR_WCEQ, PR_HMTR, PR_HEAT		*
C*				Removed PR_HTCL				*
C* D. Kidwell/NCEP 	 6/96	Added PR_CMTN                           *
C* S. Jacobs/NCEP	11/96	Removed PR_RWSH, PR_6SYM		*
C* K. Tyle/GSC		 3/97	Added PR_TMWB, removed inum array	*
C* D. Kidwell/NCEP       5/97   Added PR_VSKN                           *
C* T. Lee/GSC		11/97	Added PR_THWC				*
C* D. Kidwell/NCEP       5/98   Removed PR_CMTN                         *
C* S. Jacobs/NCEP	12/98	Fixed typo				*
C* D. Kidwell/NCEP      12/98   Added PR_WVPH and PR_WVDD               *
C* D. Kidwell/NCEP       2/99   Added PR_WPHF, PR_WPHM, PR_WVSW         *
C* D. Kidwell/NCEP       3/99   Added PR_DMAX, PR_DMIN                  *
C* S. Jacobs/NCEP	 3/99	Added PR_TPFR				*
C* S. Jacobs/NCEP	 3/99	Added format for output of PR_TPFR	*
C* A. Hardy/GSC          4/99   Added PR_PR6X                           *
C* J. Green/AWC          6/99   Added PR_XVFR                           *
C* A. Hardy/GSC          8/99   Added PR_KNMH                           *
C* D. Kidwell/NCEP      10/99   Added PR_WMAO, PR_PWAO                  *
C* D. Kidwell/NCEP	11/99	Added PR_HGNM				*
C* T. Lee/GSC		12/99	Added PR_ALTP				*
C* T. Lee/GSC		01/00	Added PR_PMST				*
C* D. Kidwell/NCEP       3/00   Added PR_IGRO                           *
C* D. Kidwell/NCEP      11/00   Added PR_PR24                           *
C* T. Lee/SAIC		 9/01	Added PR_WCHT, PR_MHKN			*
C* J. Ator/NCEP		01/02	Added PR_HCDM				*
C* D. Kidwell/NCEP       5/03   Added PR_CMSL, PR_MOBS, PR_WCFR,        *
C*				PR_WXVF, PR_TPWN, PR_AWNM               *
C* T. Lee/SAIC		 6/03	Added PR_FOSB                           *
C* D. Kidwell/NCEP       8/03   Added PR_WCMS                           *
C* M. Li/SAIC		 7/06   Added PR_CLDB				*
C* S. Chiswell		 1/07	Added PR_VAPI, PR_RELI			*
C************************************************************************
	CHARACTER 	name*7, cptnd*4, cfig*2
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
30	    FORMAT ( A )
	    CALL TM_CHAR ( 'Select a subroutine', .false.,
     +                      .false., 1, name, n, ier )
            IF ( ier .eq. 2 ) THEN
	       name   = ' '
               iostat = -1
	    END IF
	    CALL ST_LCUC (name, name, ier)
C------------------------------------------------------------------------
	    IF (name (1:1) .eq. 'H') THEN
	    WRITE ( 6, 20 )
20	    FORMAT ( ' Temperature subroutines: '/
     +          ' PR_TMCF    PR_TMFC    PR_TMKC    PR_TMCK   ',
     +          ' PR_TMFK    PR_TMKF  '/' PR_TMPK    PR_TLCL   ',   
     +          ' PR_DDEP    PR_DWDP    PR_DMAX    PR_DMIN '/, 
     +          ' Wind subroutines: '/
     +          ' PR_DRCT    PR_SPED    PR_UWND    PR_VWND  '/,
     +          ' PR_KNMS    PR_MSKN    PR_PSPD    PR_WIND  '/,
     +          ' PR_PKSS    PR_PKDD    PR_KNMH    PR_MHKN '/,  
     +          ' Moisture subroutines: '/
     +          ' PR_DWPT    PR_THTA    PR_THTE    PR_VAPR   ',
     +          ' PR_VAPI    PR_RELH    PR_RELI    PR_MIXR  '/
     +          ' PR_LHVP    PR_TVRK    PR_RHDP    PR_TMWB   ',
     +          ' PR_THWC    '/
     +          ' Pressure subroutines: '/
     +          ' PR_PRES    PR_PLCL    PR_PALT    PR_ALTP   ',
     +          ' PR_ALTM    '/,
     +          ' PR_P03C    PR_PTSY    PR_P03D    PR_ALTI   ',
     +          ' PR_PMSL ' )
	    WRITE ( 6, 21 )
21	    FORMAT ( ' Height subroutines:'/
     +          ' PR_HGMK    PR_HGMD    PR_HGKM    PR_HGFM   ',
     +          ' PR_MHGT    PR_SCLH  '/' PR_HGFS    PR_HGSF   ',
     +          ' PR_ZALT    PR_HGMF    PR_D100    PR_M100 '/
     +          ' PR_MMIN    PR_INMM    PR_VSKN    PR_HGNM   ',
     +          ' PR_HCDM '/
     +          ' Miscellaneous: '/
     +          ' PR_NSYM    PR_WMAO    PR_PWAO    PR_RZLL '/
     +          ' PR_TMST    PR_DDEN    PR_WCEQ   ',
     +          ' PR_WCHT    PR_HMTR    PR_HEAT '/' PR_WVPH   ',
     +          ' PR_WVDD    PR_WPHF    PR_WPHM    PR_WVSW   ',
     +          ' PR_TPFR '/ ' PR_PR6X    PR_PMST    PR_IGRO   ',
     +          ' PR_PR24    PR_TPWN    PR_AWNM '/' PR_FOSB'/
     +          ' Cloud subroutines: '/
     +          ' PR_CLOA    PR_CLCT    PR_CMBC    PR_COMT   ',
     +          ' PR_COMX    PR_COML  '/' PR_COMM    PR_COMH   ',
     +          ' PR_CLCX    PR_CLHX    PR_CTCC    PR_SKYX  '/,  
     +          ' PR_XVFR    PR_CMSL    PR_MOBS    PR_WCFR   ',
     +          ' PR_WXVF    PR_WCMS    '/' PR_CLDB ' )
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_DRCT') THEN
	       	WRITE(6,*)' Enter the U and V components'
	       	READ (5,*) uwnd, vwnd
	       	drct = PR_DRCT (uwnd, vwnd)
	       	WRITE(6,*)' DIRECTION= ', drct
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_SPED') THEN
	       	WRITE(6,*)' Enter U and V components'
	       	READ (5,*) uwnd, vwnd
	       	sped = PR_SPED (uwnd, vwnd)
		WRITE(6,*)' SPEED= ', sped
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_UWND') THEN
	       	WRITE(6,*)' Enter the wind speed and direction'
 		READ (5,*) sped, drct
	       	uwnd = PR_UWND (sped, drct)
	       	WRITE(6,*)' U COMPONENT= ', uwnd
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_VWND') THEN
 	       	WRITE(6,*)' Enter the wind speed and direction'
	       	READ (5,*) sped, drct
	       	vwnd = PR_VWND (sped, drct)
	       	WRITE(6,*)' V COMPONENT= ', vwnd
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_KNMS') THEN
	       	WRITE(6,*)' Enter the wind speed in knots'
	       	READ (5,*) sknt
	       	sped = PR_KNMS (sknt)
	       	WRITE(6,*) ' SPEED (m/s)= ',sped
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_MSKN') THEN
	       	WRITE(6,*)' Enter the wind speed in meters/second'
	       	READ (5,*) sped
	       	sknt = PR_MSKN (sped)
	       	WRITE(6,*)' SPEED (kts)= ', sknt
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PSPD') THEN
	       	WRITE(6,*)' Enter the wind direction and speed'
	       	READ (5,*) drct, sped
	       	pspd = PR_PSPD (drct, sped)
	       	WRITE(6,*)' PACKED SPEED AND DIRECTION=',  pspd
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WIND') THEN
	       	WRITE(6,*)' Enter the wind direction and speed'
	       	READ (5,*) drct, sped
	       	pspd = PR_WIND (drct, sped)
	       	WRITE(6,*)' Combined SPEED AND DIRECTION=',  pspd
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PKSS') THEN
	       	WRITE(6,*)' Enter PSPD'
 		READ (5,*) ddsss
 	       	pkss = PR_PKSS (ddsss)
	       	WRITE(6,*)' PACKED SPEED= ', pkss
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PKDD') THEN
	       	WRITE(6,*)' Enter PSPD'
	       	READ (5,*) ddsss
	       	pkdd = PR_PKDD (ddsss)
	       	WRITE(6,*)' PACKED DIRECTION= ', pkdd
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_KNMH') THEN
	       	WRITE(6,*)' Enter the wind speed in knots'
	       	READ (5,*) sped
	       	smph = PR_KNMH (sped)
	       	WRITE(6,*)' SPEED (mph)= ', smph
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_MHKN') THEN
	       	WRITE(6,*)' Enter the wind speed in miles per hour'
	       	READ (5,*) smph
	       	sped = PR_MHKN (smph)
	       	WRITE(6,*) ' SPEED (knots) = ',sped
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMCF') THEN
	       	WRITE(6,*)' Enter the temperature in degrees Celsius'
	       	READ (5,*) temp
	       	tmpf = PR_TMCF (temp)
	       	WRITE(6,*)' TEMPERATURE (F)= ', tmpf
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMFC') THEN
	       	WRITE(6,*)' Enter the temperature in Fahrenheit'
	       	READ (5,*) tmpf
	       	temp = PR_TMFC (tmpf)
	       	WRITE(6,*)' TEMPERATURE (C)= ', temp
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMCK') THEN
	       	WRITE(6,*)' Enter the temperature in degrees Celsius'
	       	READ (5,*) temp
	       	tmpk = PR_TMCK (temp)
	       	WRITE(6,*)' TEMPERATURE (K)= ', tmpk
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMKC') THEN
	       	WRITE(6,*)' Enter the temperature in Kelvin'
	       	READ (5,*) tmpk
	       	temp = PR_TMKC (tmpk)
	       	WRITE(6,*)' TEMPERATURE (C)= ', temp
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMFK') THEN
	       	WRITE(6,*)' Enter the temperature in Fahrenheit'
	       	READ (5,*) tmpf
	       	tmpk = PR_TMFK (tmpf)
	       	WRITE(6,*)' TEMPERATURE (K)= ', tmpk
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMKF') THEN
	       	WRITE(6,*)' Enter the temperature in Kelvin'
	       	READ (5,*) tmpk
	       	tmpf = PR_TMKF (tmpk)
	       	WRITE(6,*)' TEMPERATURE (F)= ', tmpf
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMPK') THEN
	       	WRITE(6,*)' Enter the pressure (mb) and theta (K)'
	       	READ (5,*) pres, thta
 	       	tmpk = PR_TMPK (pres, thta)
	       	WRITE(6,*)' TEMPERATURE (K)= ', tmpk
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PRES')THEN
	       	WRITE(6,*)' Enter the temperature (C) and theta (K)'
	       	READ (5,*) temp, thta
 		pres = PR_PRES (temp, thta)
	       	WRITE(6,*)' PRESSURE (MB)= ', pres   
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_DWPT') THEN
	       	WRITE(6,*)' Enter MIXR, PRES'
	       	READ (5,*) rmix, pres
	       	dwpt = PR_DWPT (rmix, pres)
	       	WRITE(6,*)' DEW POINT (C)= ', dwpt
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_THTA') THEN
	       	WRITE(6,*)' Enter the temperature (C) and pressure (mb)'
	       	READ (5,*) temp, pres
	 	thta = PR_THTA (temp, pres)
	       	WRITE(6,*)' POTENTIAL TEMPERATURE (K)= ', thta
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_THTE') THEN
	       	WRITE(6,*)' Enter PRES,TEMP,DWPT'
 		READ (5,*) pres, temp, dwpt
	       	thte = PR_THTE (pres, temp, dwpt)
 		WRITE(6,*)' THTE', thte
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_VAPR') THEN
	       	WRITE(6,*)' Enter the dewpoint (C)'
  	       	READ (5,*) dwpt
	       	vapr = PR_VAPR (dwpt)
  	       	WRITE(6,*)' VAPOR PRESSURE (MB)= ', vapr
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_VAPI') THEN
                WRITE(6,*)' Enter the temperature (C)'
                READ (5,*) temp
                vapr = PR_VAPI (temp)
                WRITE(6,*)' SAT. VAPOR PRESSURE WRT ICE (MB)= ', vapr
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_RELH') THEN
	       	WRITE(6,*)' Enter temperature (C) and dewpoint (C)'
	       	READ (5,*) temp, dwpt
 	       	relh = PR_RELH (temp, dwpt)
	       	WRITE(6,*)' RELATIVE HUMIDITY= ', relh
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_RELI') THEN
                WRITE(6,*)' Enter temperature (C) and dewpoint (C)'
                READ (5,*) temp, dwpt
                relh = PR_RELI (temp, dwpt)
                WRITE(6,*)' RELATIVE HUMIDITY WRT ICE= ', relh
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_MIXR') THEN
	       	WRITE(6,*)' Enter the dewpoint (C) and pressure (mb)'
 		READ (5,*) temp, pres
	       	rmix = PR_MIXR (temp, pres)
 	       	WRITE(6,*)' MIXING RATIO (G/KG)= ', rmix
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TLCL') THEN
	       	WRITE(6,*)' Enter the temperature (C), dewpoint (C)'
	       	READ (5,*) temp, dwpt
	       	tlcl= PR_TLCL (temp, dwpt)
	       	WRITE(6,*)' TEMPERATURE (K) AT THE LCL= ',  tlcl
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PLCL') THEN
	       	WRITE(6,*)' Enter the parcel temp (C) and pressure(mb)'
	       	READ (5,*) temp, pres
	       	WRITE(6,*)' Enter the temperature at the LCL (K)'
	       	READ (5,*) tlcl
	       	plcl= PR_PLCL (temp, pres, tlcl)
	       	WRITE(6,*)' PRESSURE AT THE LCL= ',  plcl
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_LHVP') THEN
	       	WRITE(6,*)' Enter the temperature (C)'
	       	READ (5,*) temp
	       	heat= PR_LHVP (temp)
	       	WRITE(6,*)' LATENT HEAT OF VAPORIZATION (J/KG)= ',  heat
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TVRK') THEN
	       	WRITE(6,*)' Enter TMPC,DWPC,PRES'
	       	READ (5,*) temp, dwpt, pres
	       	virt = PR_TVRK (temp, dwpt, pres)
	       	WRITE(6,*)' VIRTUAL TEMPERATURE (K)= ', virt
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMST') THEN
	       	WRITE(6,*)' Enter THTE, PRES'
	       	READ (5,*) thte, pres
		WRITE (6,*)' Enter a guess temperature'
		READ (5,*) tguess
	   	tmst = PR_TMST (thte, pres, tguess)
	       	WRITE(6,*)' TMST= ',  tmst
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PMST') THEN
	       	WRITE(6,*)' Enter THTE, TMPK'
	       	READ (5,*) thte, tmpk
	   	pmst = PR_PMST (thte, tmpk)
	       	WRITE(6,*)' PMST= ',  pmst
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_DDEN') THEN
	       	WRITE(6,*)' Enter the pressure (mb) and temperature (C)'
 		READ (5,*) pres, tmpc
	       	dden = PR_DDEN (pres, tmpc)
 	       	WRITE(6,*)' Dry density (KG/M**3) = ', dden
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PALT') THEN
	       	WRITE(6,*)' Enter altimeter (mb) and elevation'
	       	READ (5,*) altm, elev
	   	p = PR_PALT (altm, elev)
	       	WRITE(6,*)' station pressure =',  p
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_ALTP') THEN
	       	WRITE(6,*)' Enter station pressure and elevation'
	       	READ (5,*) palt, selv
	   	altp = PR_ALTP (palt, selv)
	       	WRITE(6,*)' altimeter in inches =',  altp
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_ALTM') THEN
	       	WRITE(6,*)' Enter altimeter in inches'
	       	READ (5,*) alti
	   	altm = PR_ALTM (alti)
	       	WRITE(6,*)' altimeter=',  altm
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_ALTI') THEN
	       	WRITE(6,*)' Enter altimeter in millibars'
	       	READ (5,*) altm
	   	alti = PR_ALTI (altm)
	       	WRITE(6,*)' altimeter =',  alti
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_P03D') THEN
	       	WRITE(6,*)' Enter pressure tendency string'
	       	READ (5,30) cptnd
	   	p03d = PR_P03D (cptnd)
	       	WRITE(6,*)' Pressure tendency number = ', p03d
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PTSY') THEN
	       	WRITE(6,*)' Enter pressure tendency number'
	       	READ (5,*) ptndn
	   	ptsy = PR_PTSY (ptndn)
	       	WRITE(6,*)' Pressure tendency symbol number = ', ptsy
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_P03C') THEN
	       	WRITE(6,*)' Enter pressure tendency number'
	       	READ (5,*) ptndn
	   	p03c = PR_P03C (ptndn)
	       	WRITE(6,*)' Pressure tendency = ', p03c
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HGMK') THEN
	       	WRITE(6,*)' Enter height in meters'
	       	READ (5,*) hght
	   	akilo = PR_HGMK (hght)
	       	WRITE(6,*)' kilometers =', akilo
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HGMD') THEN
	       	WRITE(6,*)' Enter height in meters'
	       	READ (5,*) hght
	   	deka = PR_HGMD (hght)
	       	WRITE(6,*)' dekameters =', deka
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HGKM') THEN
	       	WRITE(6,*)' Enter height in kilometers'
	       	READ (5,*) akilo
	   	hght = PR_HGKM (akilo)
	       	WRITE(6,*)' meters =', hght
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HGFM') THEN
	       	WRITE(6,*)' Enter height in feet'
	       	READ (5,*) hgft
	   	hght = PR_HGFM (hgft)
	       	WRITE(6,*)' hgft =', hght
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PMSL') THEN
	       	WRITE(6,*)' Enter P, T, Td, elev'
	       	READ (5,*) P, T, Td, elev
	   	pmsl = PR_PMSL (P, T, Td, elev)
	       	WRITE(6,*)' MSL  =', pmsl
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_CLOA') THEN
	       	WRITE(6,*)' Enter cloud code'
	       	READ (5,*) cloud
	   	oa = PR_CLOA (cloud)
	       	WRITE(6,*)' obj anal val=', oa
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_CLCT') THEN
	       	WRITE(6,*)' Enter cloud codes for 3 levels'
	       	READ (5,*) cloud1, cloud2, cloud3
	   	oatot = PR_CLCT (cloud1, cloud2, cloud3)
	       	WRITE(6,*)' total cld obj anal val=', oatot
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_CTCC') THEN
	       	WRITE(6,*)' Enter combined cloud codes for 3 levels'
	       	READ (5,*) cloud1, cloud2, cloud3
	   	oatot = PR_CTCC (cloud1, cloud2, cloud3)
	       	WRITE(6,*)' total cld obj anal val=', oatot
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_CMBC') THEN
	       	WRITE(6,*)' Enter cloud codes for 3 levels'
	       	READ (5,*) cloud1, cloud2, cloud3
	   	oatot = PR_CMBC (cloud1, cloud2, cloud3)
	       	WRITE(6,*)' combined cloud = ', oatot
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_COMT') THEN
		WRITE(6,*)' Enter combined cloud codes for 3 levels'
		READ (5,*) coml, comm, comh
		comt = PR_COMT (coml, comm, comh)
		WRITE(6,*)' maximum combined code = ', comt
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_COMX') THEN
		WRITE(6,*)' Enter cloud height and coverage code'
		READ (5,*) clhx, clcx
 		comx = PR_COMX (clhx,clcx)
		WRITE(6,*)' combined height and coverage = ', comx
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_COML') THEN
		WRITE(6,*)' Enter 3 combined height and cloud reports'
		READ (5,*) chc1,chc2,chc3
		coml = PR_COML(chc1,chc2,chc3)
		WRITE(6,*)' low level combined height-coverage is = ',
     +		            coml
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_COMM') THEN
		WRITE(6,*)' Enter 3 combined height and cloud reports'
		READ (5,*) chc1,chc2,chc3
		comm = PR_COMM(chc1,chc2,chc3)
		WRITE(6,*)' mid level combined height-coverage is = ',
     +		            comm
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_COMH') THEN
		WRITE(6,*)' Enter 3 combined height and cloud reports'
		READ (5,*) chc1,chc2,chc3
		comh = PR_COMH(chc1,chc2,chc3)
		WRITE(6,*)' high level combined height-coverage is = ',
     +		            comh
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_CLCX') THEN
		WRITE(6,*) ' Enter combined height and cloud code'
		READ (5,*) comx
		clcx = PR_CLCX(comx)
		WRITE(6,*)' Numeric cloud coverage code is = ', clcx
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_CLHX') THEN
		WRITE(6,*) ' Enter combined height and cloud code'
		READ (5,*) comx
		clhx = PR_CLHX(comx)
		WRITE(6,*)' Cloud height in hundreds of feet = ', clhx
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_SKYX') THEN
		WRITE(6,*) ' Enter sky cover, drct, speed'
		READ (5,*) skyc, drct, sped
		skyx = PR_SKYX(skyc, drct, sped )
		WRITE(6,*)' Combined sky cover and wind = ',skyx
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_DDEP') THEN
	       	WRITE(6,*)' Enter temperature and dewpoint '
	       	READ (5,*) t, d
	   	ddep = PR_DDEP (t, d)
	       	WRITE (6,*) ' Dewpoint depression = ', ddep
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_MHGT') THEN
	       	WRITE(6,*)' Enter bottom height, bottom and top',
     +                    ' P and scale height '
	       	READ (5,*) hb, pb, pt, scale
	   	hei = PR_MHGT (hb, pb, pt, scale)
	       	WRITE(6,*)' moist hydrostatic height = ', hei
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_NSYM') THEN
		WRITE(6,*)' Enter GEMPAK numeric weather code'
		READ (5,*) wnum
		wsym = PR_NSYM(wnum)
		WRITE(6,*) ' Synoptic weather symbol code = ', wsym
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WMAO') THEN
		WRITE(6,*)' Enter WMO automatic station weather code'
		READ (5,*) wnum
		wsym = PR_WMAO(wnum)
		WRITE(6,*) ' WMO manned station weather code = ', wsym
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PWAO') THEN
		WRITE(6,*)' Enter WMO auto station past weather code'
		READ (5,*) wnum
		wsym = PR_PWAO(wnum)
		WRITE(6,*) ' WMO manned station past weather code = ', 
     +			   wsym
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_RZLL') THEN
	       	WRITE(6,*)' Enter station lat, lon, range, az, ht '
	       	READ (5,*) sla, sln, ra, az, ht
	   	CALL = PR_RZLL (sla, sln, ra, az, ht, slat, slon, iret)
	       	WRITE(6,*)' lat - lon, iret', slat, slon, iret
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_SCLH') THEN
	       	WRITE(6,*)' Enter bott, top, t, td, p'
	       	READ (5,*) tb, tt, tdb, tdt, pb, pt
	   	scale = PR_SCLH (tb, tt, tdb, tdt, pb, pt)
	       	WRITE(6,*)' scale height = ', scale
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_ZALT') THEN
	       	WRITE(6,*)' Enter altimeter and station pressure in MB'
	       	READ (5,*) altm, pres
	   	zalt = PR_ZALT (altm, pres)
	       	WRITE(6,*)' station height = ', zalt
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_DWDP') THEN
	       	WRITE(6,*)' Enter temperature and dewpoint depression'
	       	READ (5,*)  temp, dwdp
	   	dwpt = PR_DWDP (temp, dwdp)
	       	WRITE(6,*)' dewpoint = ', dwpt
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_RHDP') THEN
	       	WRITE(6,*)' Enter TEMP and RELH'
	       	READ (5,*) temp, relh
	   	dwpt = PR_RHDP (temp, relh)
	       	WRITE(6,*)' dwpt = ', dwpt
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_THWC') THEN
	       	WRITE(6,*)' Enter PRES, TMPC, and DWPC:'
	       	READ (5,*) pres, tmpc, dwpc
		thwc = PR_THWC (pres, tmpc, dwpc)
	       	WRITE(6,*)' thwc = ', thwc
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TMWB') THEN
	       	WRITE(6,*)' Enter TMPC, DWPC, and PRES:'
	       	READ (5,*) tmpc, dwpc, pres 
		tmpk = PR_TMCK (tmpc)
		rmix = PR_MIXR (dwpc, pres)
		tmwk = PR_TMWB (tmpk, rmix, pres)
		tmwc = PR_TMKC (tmwk)
	       	WRITE(6,*)' tmwc = ', tmwc
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HGMF') THEN
	       	WRITE(6,*)' Enter height in meters'
	       	READ (5,*) hght
	   	hgft = PR_HGMF (hght)
	       	WRITE(6,*)' hgft = ', hgft
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_HGFS') THEN
                WRITE(6,*)' Enter height in feet'
                READ (5,*) hgft
                hgml = PR_HGFS (hgft)
                WRITE(6,*)' hgml = ', hgml
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HGSF') THEN
		WRITE(6,*)' Enter height in statute miles'
                READ (5,*) hgml
		hgft = PR_HGSF (hgml)                
                WRITE(6,*)' hgft = ', hgft
C------------------------------------------------------------------------
 	      ELSE IF (name .EQ. 'PR_D100') THEN
		WRITE(6,*)' Enter value to be divided by 100'
		READ (5,*) hvalue
                value = PR_D100(hvalue)
                WRITE(6,*)' value = ', value
C------------------------------------------------------------------------
 	      ELSE IF (name .EQ. 'PR_M100') THEN
		WRITE(6,*)' Enter value to be multiplied by 100'
		READ (5,*) value
                hvalue = PR_M100(value)
                WRITE(6,*)' value = ', hvalue
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_INMM') THEN
		WRITE(6,*)' Enter value in hundreths of inches'
    		READ (5,*) xinch
		xmilm = PR_INMM(xinch)
                WRITE(6,*)' value in millimeters = ', xmilm
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_VSKN') THEN
		WRITE(6,*)' Enter value in kilometers'
    		READ (5,*) vsbk
		vsbn = PR_VSKN(vsbk)
                WRITE(6,*)' value in nautical miles = ', vsbn
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HGNM') THEN
		WRITE(6,*)' Enter value in nautical miles'
		READ (5,*) hgnm
		hgtm = PR_HGNM(hgnm)
		WRITE(6,*)' value in meters = ', hgtm
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HCDM') THEN
		WRITE(6,*)' Enter code figure'
		READ (5,30) cfig 
		hgtm = PR_HCDM(cfig)
		WRITE(6,*)' value in meters = ', hgtm
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_MMIN') THEN
		WRITE(6,*)' Enter value in millimeters'
		READ (5,*) xmilm
  		xinch = PR_MMIN(xmilm)
		WRITE(6,*)' value in hundreths of inches = ', xinch
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WCEQ') THEN
		WRITE(6,*)' Enter temperature in Fahrenheit'
		READ (5,*) tmpf
		WRITE(6,*)' Enter the wind speed in knots'
		READ (5,*) sknt
		wceq = PR_WCEQ ( tmpf, sknt )
		WRITE(6,*) ' Wind chill equivalent temp = ', wceq
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WCHT') THEN
		WRITE(6,*)' Enter temperature in Fahrenheit'
		READ (5,*) tmpf
		WRITE(6,*)' Enter the wind speed in knots'
		READ (5,*) sknt
		wcht = PR_WCHT ( tmpf, sknt )
		WRITE(6,*) ' Wind chill temp = ', wcht
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HMTR') THEN
		WRITE(6,*)' Enter temperature in Fahrenheit'
		READ (5,*) tmpf
		WRITE(6,*)' Enter dewpoint in Fahrenheit'
		READ (5,*) dwpf
		hmtr = PR_HMTR ( tmpf, dwpf )
		WRITE(6,*) ' Humiture index = ', hmtr
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_HEAT') THEN
		WRITE(6,*)' Enter temperature in Fahrenheit'
		READ (5,*) tmpf
		WRITE(6,*)' Enter relative humidity'
		READ (5,*) relh
		heat = PR_HEAT ( tmpf, relh )
		WRITE(6,*) ' Heat index = ', heat
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WVPH') THEN
	       	WRITE(6,*)' Enter wave period'
	       	READ (5,*) powv
	       	WRITE(6,*)' Enter wave height'
	       	READ (5,*) howv
	       	WRITE(6,*)' Enter group number'
	       	READ (5,*) group
	   	wvph = PR_WVPH (powv, howv, group)
	       	WRITE(6,*)' wvph = ', wvph
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WVDD') THEN
	       	WRITE(6,*)' Enter first wave direction in degrees'
	       	READ (5,*) dosw
	       	WRITE(6,*)' Enter second wave direction in degrees'
	       	READ (5,*) dos2
		wvdd = PR_WVDD (dosw, dos2)
	       	WRITE(6,*)' wvdd = ', wvdd
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WPHF') THEN
	       	WRITE(6,*)' Enter wave period'
	       	READ (5,*) powv
	       	WRITE(6,*)' Enter wave height in meters'
	       	READ (5,*) howv
	       	WRITE(6,*)' Enter group number'
	       	READ (5,*) group
	   	wphf = PR_WPHF (powv, howv, group)
	       	WRITE(6,*)' wphf = ', wphf
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WPHM') THEN
	       	WRITE(6,*)' Enter instrument wave period'
	       	READ (5,*) wper
	       	WRITE(6,*)' Enter instrument wave height in meters'
	       	READ (5,*) whgt
	       	WRITE(6,*)' Enter wind wave period'
	       	READ (5,*) poww
	       	WRITE(6,*)' Enter wind wave height in meters'
	       	READ (5,*) howw
	   	wphm = PR_WPHM (wper, whgt, poww, howw)
	       	WRITE(6,*)' wphm = ', wphm
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_WVSW') THEN
	       	WRITE(6,*)' Enter wave direction in degrees'
	       	READ (5,*) dosw
	       	WRITE(6,*)' Enter wave period'
	       	READ (5,*) posw
	       	WRITE(6,*)' Enter wave height in meters'
	       	READ (5,*) hosw
	   	wvsw = PR_WVSW (dosw, posw, hosw)
	       	WRITE(6,*)' wvsw = ', wvsw
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_DMAX') THEN
	       	WRITE(6,*)' Enter 6-hour max temp at 00Z'
	       	READ (5,*) t00x
	       	WRITE(6,*)' Enter 6-hour max temp at 06Z'
	       	READ (5,*) t06x
	       	WRITE(6,*)' Enter local midnight max temp'
	       	READ (5,*) tdxc
	   	dmax = PR_DMAX (t00x, t06x, tdxc)
	       	WRITE(6,*)' dmax = ', dmax
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_DMIN') THEN
	       	WRITE(6,*)' Enter 6-hour min temp at 12Z'
	       	READ (5,*) t12n
	       	WRITE(6,*)' Enter 6-hour min temp at 18Z'
	       	READ (5,*) t18n
	   	dmin = PR_DMIN (t12n, t18n)
	       	WRITE(6,*)' dmin = ', dmin
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_TPFR') THEN
	       	WRITE(6,*)' Enter Min temp'
	       	READ (5,*) tmin
	       	WRITE(6,*)' Enter Max temp'
	       	READ (5,*) tmax
	       	WRITE(6,*)' Enter 24-hr POP'
	       	READ (5,*) pp24
		tpfr = PR_TPFR ( tmin, tmax, pp24 )
	       	WRITE(6,1000) tpfr
1000		FORMAT ( ' tpfr = ', 1PE14.7 )
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PR6X') THEN
		WRITE(6,*)' Enter the FIRST precip. amount'
	       	READ (5,*) p01
	       	WRITE(6,*)' Enter the SECOND precip. amount'
	       	READ (5,*) p02
	       	WRITE(6,*)' Enter the THIRD precip. amount'
	       	READ (5,*) p03
	       	WRITE(6,*)' Enter the FOURTH precip. amount'
	       	READ (5,*) p04
 		pr6x = PR_PR6X ( p01, p02, p03, p04 )
	       	WRITE(6,*)' pr6x = ', pr6x
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_XVFR') THEN
                WRITE(6,*)' Enter the ceiling in hundreds of feet'
                READ (5,*) ceil 
                WRITE(6,*)' Enter the visibility in miles'
                READ (5,*) vsby 
                xvfr = PR_XVFR ( ceil, vsby )
                WRITE(6,*)' xvfr = ', xvfr 
C------------------------------------------------------------------------
	      ELSE IF (name .eq. 'PR_IGRO') THEN
		WRITE(6,*)' Enter the surface air temperature in deg. C'
		READ (5,*) tmpc
		WRITE(6,*)' Enter the sea-surface temperature in deg. C'
		READ (5,*) sstc
		WRITE(6,*)' Enter the wind speed in meters/second'
		READ (5,*) sped
		prigro = PR_IGRO ( tmpc, sstc, sped )
		WRITE(6,*)' igro = ', prigro
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_PR24') THEN
		WRITE(6,*)' Enter the first precip. amount'
	       	READ (5,*) p01
	       	WRITE(6,*)' Enter the second precip. amount'
	       	READ (5,*) p02
	       	WRITE(6,*)' Enter the third precip. amount'
	       	READ (5,*) p03
	       	WRITE(6,*)' Enter the fourth precip. amount'
	       	READ (5,*) p04
 		pr24 = PR_PR24 ( p01, p02, p03, p04 )
	       	WRITE(6,*)' pr24 = ', pr24
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_CMSL') THEN
                WRITE(6,*)' Enter the ceiling in hundreds of feet'
                READ (5,*) ceil 
                WRITE(6,*)' Enter the station elevation in meters'
                READ (5,*) selv 
                cmsl = PR_CMSL ( ceil, selv )
                WRITE(6,*)' cmsl = ', cmsl 
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_MOBS') THEN
                WRITE(6,*)' Enter the MSL ceiling in hundreds of feet'
                READ (5,*) cmsl 
                WRITE(6,*)' Enter the mtn. obsc. threshold (100s ft)'
                READ (5,*) otval 
                obsind = PR_MOBS ( cmsl, otval )
                WRITE(6,*)' mobs = ', obsind 
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_WCFR') THEN
                WRITE(6,*)' Enter the prevailing flight rules id no.'
                READ (5,*) xvfr 
                WRITE(6,*)' Enter the tempo/prob flight rules id no.'
                READ (5,*) txvf 
                WRITE(6,*)' Enter the prevailing total cloud cover'
                READ (5,*) cfrt 
                WRITE(6,*)' Enter the tempo/prob total cloud cover'
                READ (5,*) tcfr 
                wcfr = PR_WCFR ( xvfr, txvf, cfrt, tcfr )
                WRITE(6,*)' wcfr = ', wcfr 
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_WXVF') THEN
                WRITE(6,*)' Enter the prevailing flight rules id no.'
                READ (5,*) xvfr 
                WRITE(6,*)' Enter the tempo/prob flight rules id no.'
                READ (5,*) txvf 
                wxvf = PR_WXVF ( xvfr, txvf )
                WRITE(6,*)' wxvf = ', wxvf 
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_TPWN') THEN
                WRITE(6,*)' Enter the tempo/prob numeric weather code'
                READ (5,*) twnm 
                WRITE(6,*)' Enter the vicinity numeric weather code'
                READ (5,*) vwnm 
                WRITE(6,*)' Enter the probability'
                READ (5,*) pprb 
                tpwn = PR_TPWN ( twnm, vwnm, pprb )
                WRITE(6,*)' tpwn = ', tpwn 
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_AWNM') THEN
                WRITE(6,*)' Enter the prevailing numeric weather code'
                READ (5,*) wnum 
                WRITE(6,*)' Enter the tempo/prob numeric weather code'
                READ (5,*) twnm 
                WRITE(6,*)' Enter the vicinity numeric weather code'
                READ (5,*) vwnm 
                WRITE(6,*)' Enter the probability'
                READ (5,*) pprb 
                awnm = PR_AWNM ( wnum, twnm, vwnm, pprb )
                WRITE(6,*)' awnm = ', awnm 
C------------------------------------------------------------------------
	      ELSE IF (name .EQ. 'PR_FOSB') THEN
	       	WRITE(6,*)' Enter TEMP(C), RELH(%), WIND SPEED(m/s)'
	       	READ (5,*) temp, relh, sped
 	       	fosb = PR_FOSB (temp, relh, sped)
	       	WRITE(6,*)' Fosberg Index = ', fosb
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_WCMS') THEN
                WRITE(6,*)' Enter prevailing ceiling - hundreds of feet'
                READ (5,*) ceil 
                WRITE(6,*)' Enter tempo/prob ceiling - hundreds of feet'
                READ (5,*) tcms 
                wcms = PR_WCMS ( ceil, tcms )
                WRITE(6,*)' wcms = ', wcms 
C------------------------------------------------------------------------
              ELSE IF (name .EQ. 'PR_CLDB') THEN
                WRITE(6,*)' Enter ceiling and 3 cloud reports'
                READ (5,*) ceil, chc1, chc2, chc3 
                WRITE(6,*)' input = ',  ceil, chc1, chc2, chc3 
                cldb = PR_CLDB (ceil, chc1, chc2, chc3)
                WRITE(6,*)' The lowest ceiling is = ', cldb 
C------------------------------------------------------------------------
	    END IF
	END DO
	END
