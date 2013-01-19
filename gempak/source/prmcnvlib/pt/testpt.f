	PROGRAM TESTPT
C************************************************************************
C* TESTPT								*
C*									*
C* This program tests the PARAMETER CHARACTER library functions.	*
C*									*
C**									*
C* Log:									*
C* I. GRAFFMAN/RDS	11/84						*
C* S. Schotz/GSC	 4/90	Added PT_WSYM				*
C* L. Sager/NCEP	 2/96	Added PT_WMTN & PT_WMCO			*
C* S. Jacobs/NCEP	 8/96	Cleaned up				*
C* S. Jacobs/NCEP	11/96	Removed PT_WTHR, PT_WASH, PT_WMTN,	*
C*				PT_WMCO, PT_WMSY			*
C* S. Jacobs/NCEP	11/96	Increased all chars from 8 to 12	*
C* A. Hardy/GSC         12/97   Corrected PT_WCMT to PT_WNMT            *
C* D. Kidwell/NCEP       2/99   Added PT_SWEL                           *
C* D. Kidwell/NCEP       3/99   Added PT_DPRC                           *
C* S. Jacobs/NCEP	 3/99	Added PT_TPFC				*
C* A. Hardy/GSC          4/99   Added PT_VSBF				*
C* A. Hardy/GSC          7/99   Added PT_TURB, PT_FQOT, PT_TICE  	*
C* D. Kidwell/NCEP       1/00   Added PT_ACTP, PT_PATN                  *
C* D. Kidwell/NCEP       3/00   Added PT_DIGR                           *
C* A. Hardy/SAIC        11/01   Added PT_VSBC				*
C* R. Jones/NCEP	 9/06	Added PT_DPDX				*
C************************************************************************
	CHARACTER	chardat*12, wthr*12, wthrmt*20,
     +			pt_wcod*12, pt_wtmo*12, pt_pwth*12,
     +			pt_clds*12, pt_cldn*12, pt_cmcl*12,
     +                  pt_swel*12, pt_dprc*12, pt_tpfc*12,
     +			pt_vsbf*12, pt_turb*12, pt_fqot*12,
     +			pt_tice*12, pt_actp*12, pt_digr*12,
     +                  pt_vsbc*12, pt_dpdx*12
C------------------------------------------------------------------------
	CALL IN_BDTA ( iret )
	iostat = 0
	DO WHILE ( iostat .eq. 0 )
	    WRITE ( 6, 20 )
20	    FORMAT ( '  1 = PT_WCOD   2 = PT_WNUM   3 = PT_WTMO ', /,
     +               '  4 = PT_WSYM   5 = PT_WNMT   6 = PT_PWTH ', /,
     +               ' 10 = PT_CLDS  11 = PT_CCNM  12 = PT_CMCL ', /,
     +               ' 13 = PT_CLDN  14 = PT_SWEL  15 = PT_DPRC ', /,
     +               ' 16 = PT_TPFC  17 = PT_VSBF  18 = PT_TURB ', /,
     +               ' 19 = PT_FQOT  20 = PT_TICE  21 = PT_ACTP ', /,
     +               ' 22 = PT_PATN  23 = PT_DIGR  24 = PT_VSBC ', /,
     +               ' 25 = PT_DPDX ',/ )
C
	    CALL TM_INT ( 'Select a subroutine number', .false.,
     +                     .false., 1, numsub, n, ier )
	    IF ( ier .eq. 2 ) THEN
	       iostat = -1
	       numsub = -1
	    END IF
	    chardat = ' '
30	    FORMAT ( A )
C------------------------------------------------------------------------
	    IF (numsub .eq. 1) THEN
	       	WRITE(6,*)' Enter GEMPAK numerical weather code'
	       	READ (5,*) wnum
	       	chardat = PT_WCOD (wnum)
	       	WRITE(6,*)' GEMPAK character weather code = ', chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 2) THEN
	       	WRITE(6,*)' Enter GEMPAK character weather code'
	       	READ (5,30) chardat
		CALL ST_LCUC ( chardat, chardat, ier)
	       	wnum = PT_WNUM (chardat)
	       	WRITE(6,*)' GEMPAK numerical weather code = ', wnum
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 3) THEN
	       	WRITE(6,*)' Enter WMO numerical weather code'
	       	READ (5,*) wwmo
	       	chardat = PT_WTMO (wwmo)
	       	WRITE(6,*)' WMO character weather code = ', chardat
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 4) THEN
                WRITE(6,*)' Enter WMO character weather code'
                READ (5,30) wthr
		CALL ST_LCUC ( wthr, wthr, ier)
		wsym = PT_WSYM (wthr)
                WRITE(6,*)' WMO numerical weather code = ', wsym
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 5) THEN
                WRITE(6,*)' Enter METAR character weather code'
                READ (5,30) wthrmt
		CALL ST_LCUC ( wthrmt, wthrmt, ier)
		wnum = PT_WNMT (wthrmt)
                WRITE(6,*)' GEMPAK numerical weather code = ', wnum
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 6) THEN
	       	WRITE(6,*)' Enter past weather numerical code'
	       	READ (5,*) pwth
	       	chardat = PT_PWTH (pwth)
	       	WRITE(6,*)' Past weather character code = ', chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 10) THEN
	       	WRITE(6,*)' Enter combined cloud'
	       	READ (5,*) cldc
	        chardat= PT_CLDS (cldc)
		WRITE(6,*)' cloud code ',chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 11) THEN
	       	WRITE(6,*)' Enter cloud cover code'
	       	READ (5,30) chardat
		CALL ST_LCUC ( chardat, chardat, ier)
	       	numb = PT_CCNM (chardat)
	       	WRITE(6,*)' Cloud code = ', numb
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 12) THEN
	       	WRITE(6,*)' Enter combined height, cloud number'
	       	READ (5,*) htcld
	        chardat= PT_CMCL (htcld)
		WRITE(6,*)' combined cloud code ',chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 13) THEN
	       	WRITE(6,*)' Enter cloud number'
	       	READ (5,*) cloud
	        chardat= PT_CLDN (cloud)
		WRITE(6,*)' cloud code ',chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 14) THEN
	       	WRITE(6,*)' Enter combined wave direction, period, hgt'
	       	READ (5,*) wvsw
	        chardat= PT_SWEL (wvsw)
		WRITE(6,*)' wave direction, period, height ',chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 15) THEN
	       	WRITE(6,*)' Enter 24-hour precipitation in inches'
	       	READ (5,*) p24i
	        chardat= PT_DPRC (p24i)
		WRITE(6,*)' daily weather map precip ',chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 16) THEN
	       	WRITE(6,*)' Enter the coded Min temp, Max temp, POP'
	       	READ (5,*) tpfr
	        chardat= PT_TPFC ( tpfr )
		WRITE(6,*)' Medium range output values ',chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 17) THEN
		WRITE(6,*)' Enter the visibility in statute miles'
                READ (5,*) vsbf
                chardat= PT_VSBF ( vsbf )
                WRITE(6,*)'Visibility ', chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 18) THEN
		WRITE(6,*)' Enter the turbulence type ( 1, 2, or 3 )'
                READ (5,*) tpot
                chardat= PT_TURB ( tpot )
                WRITE(6,*)'Turbulence ', chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 19) THEN
		WRITE(6,*)' Enter the turbulence frequency (1,2,or3)'
                READ (5,*) fqot 
                chardat= PT_FQOT ( fqot )
                WRITE(6,*)'Turbulence frequency ', chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 20) THEN
		WRITE(6,*)' Enter the icing type ( 1 - 9 )'
                READ (5,*)  tpoi
                chardat= PT_TICE ( tpoi )
                WRITE(6,*)'Type of icing ', chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 21) THEN
	       	WRITE(6,*)' Enter GEMPAK numerical aircraft type'
	       	READ (5,*) atpn
	       	chardat = PT_ACTP (atpn)
	       	WRITE(6,*)' Character aircraft type = ', chardat
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 22) THEN
                WRITE(6,*)' Enter character aircraft type'
                READ (5,30) chardat
		atpn = PT_PATN (chardat)
                WRITE(6,*)' GEMPAK numerical aircraft type = ', atpn
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 23) THEN
	       	WRITE(6,*)' Enter ice accretion rate in inches / 3 hrs'
	       	READ (5,*) accret
	        chardat= PT_DIGR (accret)
		WRITE(6,*)' Character ice accretion value = ', chardat
C------------------------------------------------------------------------
	      ELSE IF (numsub .eq. 24) THEN
		WRITE(6,*)' Enter the visibility in statute miles'
                READ (5,*) vsbf
                chardat= PT_VSBC ( vsbf )
                WRITE(6,*)'Visibility ', chardat
C------------------------------------------------------------------------
              ELSE IF (numsub .eq. 25) THEN
                WRITE(6,*)' Enter the dewpoint depression (Celsius) '
                READ (5,*) dpdc
                chardat= PT_DPDX ( dpdc )
                WRITE(6,*)'Dewpoint depression ', chardat
C------------------------------------------------------------------------
	    END IF
	END DO
C
	END
