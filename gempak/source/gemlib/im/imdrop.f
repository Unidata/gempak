	SUBROUTINE IM_DROP  ( iret )
C************************************************************************
C* IM_DROP								*
C*									*
C* This subroutine drops the requested image.  It has to be called 	*
C* after IM_SIMG is called. 						*
C*									*
C* IM_DROP  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95						*
C* C. Lin/EAI	 	 6/95						*
C* C. Lin/EAI	 	 9/95	Add ER_WMSG after GSATIM		*
C* J. Cowie/COMET	10/95	Fixed arg in call to ER_WMSG()		*
C* C. Lin/EAI	         1/96	pass the error back if GSATIM fails	*
C* J. Cowie/COMET	 7/96	Updated color bar to use calibration	*
C* S. Jacobs/NCEP	 8/96	Commented call to IM_CBAR, for now	*
C* J. Cowie/COMET	10/96	Fixed IFINVD typo, minpv fix for brit	*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* S. Jacobs/NCEP	 4/97	Removed call to IM_RTBL			*
C* J. Cowie/COMET	12/97	Replaced imsorc check with imradf	*
C* S. Jacobs/NCEP	12/97	Added color bar for radar images only	*
C* D.W.Plummer/NCEP	 3/03	Change calling seq of IM_BTOT & IM_TTOB	*
C* M. Li/SAIC		11/03	Removed IM_CBAR				*
C* M. Li/SAIC		11/03	Color bar value plotted as temperature	*
C*				only for imtype = 8 or 128		*
C* S. Chiswell/Unidata	 2/04	Added imcmn.cmn for calibration bar flag*
C* D.W.Plummer/NCEP	 7/04	Reverse order of IM_ICMN and GSICMN	*
C* S. Chiswell/Unidata	 7/04	Added plotting of additional NIDS parts *
C* T. Piper/SAIC	07/06	Moved color bar legend to im_labl	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'imcmn.cmn'
C
	CHARACTER	nidsfile*132
C*
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
	IF ( iret .ne. 0 ) RETURN
C
C*      Set image common information in GEMPLT
C
	CALL GSICMN ( iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Set non-image start flag if NIDS level III
C
	istart = 1
	IF ( ( imftyp .eq. IFNIDS ) .or. ( imftyp .eq. IFNEXZ ) ) THEN
	    IF ( imrdfl .ge. 2 ) istart = 0
	ENDIF
C
C*	Drop the image
C
	IF ( istart .gt. 0 ) CALL GSATIM ( cmfile, iret )
	IF ( iret .ne. 0 ) THEN
	    CALL ER_WMSG ( 'GEMPLT', iret, ' ', ierr )
	    RETURN
	ENDIF
C
C*	If a NIDS level III product, plot the auxilary parts- if any
C
	IF ( ( imftyp .eq. IFNIDS ) .or. ( imftyp .eq. IFNEXZ ) ) THEN
	    IF ( imnblk .ge. ( istart + 3 ) ) THEN
		CALL ST_NULL  ( cmfile, nidsfile, lens, iret )
		CALL im_nidsdriver ( nidsfile, imftyp, imprsz,
     +			imlenf, imnblk, imboffs, istart )
	    ENDIF
	ENDIF
	CALL IM_LABL()
C
	RETURN
	END
