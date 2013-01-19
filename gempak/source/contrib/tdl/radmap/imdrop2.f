	SUBROUTINE IM_DROP2  ( DoMissing, DoColorBar, iret )
C************************************************************************
C* IM_DROP								*
C*									*
C* From version in TAMU GDP driver.  Optimized for NIDS (only) plotting * 
C**									*
C* Log:									*
C* D.W.Plummer/NCEP      3/03   Change calling seq of IM_BTOT & IM_TTOB *
C* M. Li/SAIC           11/03   Removed IM_CBAR                         *
C* M. Li/SAIC           11/03   Color bar value plotted as temperature  *
C*                              only for imtype = 8 or 128              *
C* D.W.Plummer/NCEP      7/04   Reverse order of IM_ICMN and GSICMN     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	logical         DoMissing , DoColorBar
C------------------------------------------------------------------------
	iret = 0
C
C*	Check if IM_SIMG was called for seting up the navigation
C
	IF ( imftyp .eq. IFINVD ) RETURN
C
C*      Set image common information in APPL imgdef.h
C*      Send through first entry in COMMON /IMGDEF/ which is 'imftyp'.
C*      The imgdef.h information is necessary for IM_GVTOTA
C
	CALL IM_ICMN ( imftyp, iret )
C
C*      Set image common information in GEMPLT
C
	CALL GSICMN ( iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Drop the image
C
	IF ( .not. DoMissing ) THEN
	    CALL GSATIM ( cmfile, iret )
 	    IF ( iret .ne. 0 ) return
	END IF   

	RETURN
	END
