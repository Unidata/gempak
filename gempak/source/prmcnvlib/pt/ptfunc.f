	SUBROUTINE  PT_FUNC  ( rdata, chfunc, cdata, iret )
C************************************************************************
C* PT_FUNC								*
C*									*
C* This subroutine converts the real data value in rdata to a		*
C* character string using the function chfunc.				*
C*									*
C* PT_FUNC  ( RDATA, CHFUNC, CDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	RDATA			REAL		Real-valued data	*
C*	CHFUNC			CHAR*8		Conversion function	*
C*									*
C* Output parameters:							*
C*	CDATA			CHAR*		Converted data		*
C*	IRET			INTEGER		Return code		*
C*					  	   0 = normal return	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 1/86	Added ozone in characters		*
C* S. Schotz/GSC	10/89	Removed references to PTBLNK		*
C* S. Jacobs/NCEP	11/96	Removed PT_OCHR, PT_WASH, PT_WTHR	*
C* D. Kidwell/NCEP	 2/99	Added PT_SWEL                           *
C* D. Kidwell/NCEP	 3/99	Added PT_DPRC                           *
C* S. Jacobs/NCEP	 3/99	Added PT_TPFC				*
C* A. Hardy/GSC          4/99   Added PT_VSBF				*
C* A. Hardy/GSC          7/99   Added PT_TURB, PT_FQOT, PT_TICE	        *
C* D. Kidwell/NCEP	 1/00	Added PT_ACTP				*	
C* D. Kidwell/NCEP	 3/00	Added PT_DIGR                           *
C* A. Hardy/SAIC	11/01   Added PT_VSBC				*
C* R. Jones/NCEP	 9/06	Added PT_DPDX				*
C************************************************************************
	CHARACTER*(*)	chfunc, cdata
	CHARACTER       PT_FQOT
	CHARACTER*2     PT_TICE
	CHARACTER*3     PT_TURB, PT_DPDX
	CHARACTER*8	PT_CLDS, PT_CLDN, PT_CMCL, PT_SALT
	CHARACTER*8	PT_PWTH, PT_WTMO, PT_WCOD, PT_SWEL, PT_DPRC
	CHARACTER*8	PT_VSBF, PT_ACTP, PT_DIGR, PT_VSBC
	CHARACTER*12	PT_TPFC
C------------------------------------------------------------------------
	iret = 0
C
C
	IF        ( chfunc .eq. 'PT_WCOD' ) THEN
	                cdata = PT_WCOD ( rdata )
C*
	  ELSE IF ( chfunc .eq. 'PT_WTMO' ) THEN
			cdata = PT_WTMO ( rdata )
C*
	  ELSE IF ( chfunc .eq. 'PT_PWTH' ) THEN
			cdata = PT_PWTH ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_CLDS' ) THEN
			cdata = PT_CLDS ( rdata )
C*
	  ELSE IF ( chfunc .eq. 'PT_CLDN' ) THEN
			cdata = PT_CLDN ( rdata )
C*
	  ELSE IF ( chfunc .eq. 'PT_CMCL' ) THEN
			cdata = PT_CMCL ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_SALT' ) THEN
			cdata = PT_SALT ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_SWEL' ) THEN
			cdata = PT_SWEL ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_DPRC' ) THEN
			cdata = PT_DPRC ( rdata )
C*
C*
          ELSE IF ( chfunc .eq. 'PT_DPDX' ) THEN
                        cdata = PT_DPDX ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_TPFC' ) THEN
			cdata = PT_TPFC ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_VSBF' ) THEN
			cdata = PT_VSBF ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_VSBC' ) THEN
			cdata = PT_VSBC ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_TURB' ) THEN
			cdata = PT_TURB ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_FQOT' ) THEN
			cdata = PT_FQOT ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_TICE' ) THEN
			cdata = PT_TICE ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_ACTP' ) THEN
			cdata = PT_ACTP ( rdata )
C*
C*
	  ELSE IF ( chfunc .eq. 'PT_DIGR' ) THEN
			cdata = PT_DIGR ( rdata )
	END IF
C*
	RETURN
	END
