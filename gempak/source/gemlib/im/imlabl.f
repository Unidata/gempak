	SUBROUTINE IM_LABL ()
C************************************************************************
C* IM_LABL								*
C*									*
C* This subroutine creates the proper label for the color bar based	*
C*	 upon image type.						*
C*									*
C* IM_LABL ()								*
C*									*
C* Input parameters:							*
C*	NONE								*
C* Output parameters:							*
C*	NONE								*
C **									*
C * Log:								*
C * T. Piper/SAIC	07/06	Created					*
C * S. Chiswell/Unidata	07/06	Added check for supplied calibrations	*
C ***********************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'imcmn.cmn'
C
	REAL		PR_TMCK, PR_TMKC
	CHARACTER	dirnam*256, basnam*(MXFLSZ)
C------------------------------------------------------------------------
C
	IF ( imcalbar .eq. 1 ) THEN
C
C*	    We have already created the levels
C
	    imndlv = min ( 256, (immxpx-immnpx+1) )
	    RETURN
	ENDIF
C
C*  For sat images, create the data level labels
C
	IF ( imradf .eq. 0 )  THEN
	    CALL FL_PATH(cmfile, dirnam, basnam, ier)
	    IF ( basnam(1:4) .eq. "CAPE" )  THEN
		imndlv = 64
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO 
		cmbunt = "J/kg"
		cmblev(1)  = "0"
		cmblev(5)  = "1000"
		cmblev(12) = "2000"
		cmblev(18) = "3000"
		cmblev(24) = "4000"
		cmblev(30) = "5000"
		cmblev(36) = "6000"
		cmlutf = "gdpicape.tbl"
	    ELSEIF ( basnam(1:4) .eq. "CINH" )  THEN
		imndlv = 64
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO
		cmbunt = "J/kg"
		cmblev(1) = "0"
		cmblev(2) = "30"
		cmblev(4) = "60"
		cmblev(6) = "90"
		cmblev(10) = "120"
		cmlutf = "gdpicinh.tbl"
	    ELSEIF ( basnam(1:2) .eq. "LI" )  THEN
		imndlv = 64
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO
		cmbunt = "Deg C"
		cmblev(25) = "+5"
		cmblev(31) = "0"
		cmblev(38) = "-5"
		cmblev(44) = "-10"
		cmlutf = "gdpili.tbl"
	    ELSEIF ( basnam(1:2) .eq. "PW" )  THEN
		imndlv = 64
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO
		cmbunt = "Inches"
		cmblev(44) = "0.0"
		cmblev(47) = "0.5"
		cmblev(51) = "1.0"
		cmblev(53) = "1.5"
		cmblev(57) = "2.0"
		cmblev(60) = "2.5"
		cmlutf = "micropw.tbl"
	    ELSEIF ( basnam(1:4) .eq. "RAIN" )  THEN
		imndlv = 64
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO
		cmbunt = "In/hour"
		cmblev(27) = "0.00"
		cmblev(32) = "0.25"
		cmblev(37) = "0.50"
		cmblev(41) = "0.75"
		cmblev(44) = "1.00"
		cmblev(47) = "1.25"
		cmblev(50) = "1.50"
		cmblev(53) = "1.75"
		cmblev(56) = "2.00"
		cmlutf = "microrain.tbl"
	    ELSEIF ( basnam(1:3) .eq. "TPW" )  THEN
		imndlv = 64
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO
		cmbunt = "Inches"
		cmblev(1) = "0.0"
		cmblev(10) = "0.5"
		cmblev(19) = "1.0"
		cmblev(29) = "1.5"
		cmblev(38) = "2.0"
		cmblev(49) = "2.5"
		cmlutf = "gdpipw.tbl"
	    ELSEIF ( basnam(1:4) .eq. "WIND" )  THEN
		imndlv = 32
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO
		cmbunt = "Knots"
		cmblev(14) = "0"
		cmblev(19) = "10"
		cmblev(21) = "15"
		cmblev(23) = "19"
		cmblev(25) = "28"
		cmblev(26) = "36"
		cmlutf = "ssmiwind.tbl"
C
C*  For IR images calibrated to brightness, create Deg C levels
C
	    ELSEIF ( cmcalb .eq. "BRIT" .and.
     +		   ( imtype .eq. 8 .or. imtype .eq. 128) )  THEN
		imndlv = min ( 256, (immxpx-immnpx+1) )
		DO ii = 1, imndlv
		    cmblev(ii) = ' '
		ENDDO
		cmbunt = "Deg C"
		CALL IM_BTOT ( 1, immnpx, tmpk, ier)
		tmpc = PR_TMKC(tmpk)
		CALL ST_RLCH ( tmpc, 1, cmblev(1), ier)
		ibrit = (imndlv-1) + immnpx
		CALL IM_BTOT ( 1, ibrit, tmpk, ier )
		tmpc = PR_TMKC(tmpk)
		CALL ST_RLCH ( tmpc, 1, cmblev(imndlv), ier)
		tmpc = -100.0
		DO WHILE ( tmpc .lt. 51.0 )
		    tmpk = PR_TMCK(tmpc)
		    CALL IM_TTOB ( 1, tmpk, brit, ier )
		    ibrit = nint ( brit ) - immnpx
		    IF ( ibrit .gt. 0 )  THEN
			CALL ST_INCH ( INT (tmpc), cmblev(ibrit), ier )
		    ENDIF
		    tmpc = tmpc + 10.0
		ENDDO
C
C*  For anything else, create labels in native units
C
	    ELSE 
		cmbunt = cmcalb
		imndlv = min ( 256, (immxpx-immnpx+1) )
		ratio = (immxpx - immnpx)/255.0
		CALL ST_INCH ( immnpx, cmblev(1), ier )
		DO ii = 2, imndlv-1
		    IF ( mod(ii-1,16) .eq. 0 )  THEN
			level = nint((ii-1)*ratio) + immnpx
			CALL ST_INCH ( level, cmblev(ii), ier )
		    ELSE
			cmblev(ii) = ' '
		    ENDIF
		ENDDO
		level = nint( (imndlv - 1) * ratio) + immnpx
		CALL ST_INCH ( level, cmblev(imndlv), ier)
	    ENDIF
	ENDIF
C
	RETURN
	END
