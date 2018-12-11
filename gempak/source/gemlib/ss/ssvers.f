	SUBROUTINE SS_VERS  ( vmsg, iret )
C************************************************************************
C* SS_VERS								*
C*									*
C* This subroutine returns the version number of the installed		*
C* software.								*
C*									*
C* SS_VERS  ( VMSG, IRET )						*
C*									*
C* Output parameters:							*
C*	VMSG		CHAR*		Version string			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return 		*
C**									*
C* Log:									*
C* T. Piper/SAIC	09/04		Updated to 5.8.1		*
C* T. Piper/SAIC	12/04		Updated to 5.8.2		*
C* T. Piper/SAIC	03/05		Updated to 5.8.3		*
C* T. Piper/SAIC	05/05		Updated to 5.8.4		*
C* T. Piper/SAIC	09/05		Updated to 5.9.1		*
C* T. Piper/SAIC	12/05		Updated to 5.9.2		*
C* T. Piper/SAIC	03/06		Updated to 5.9.3		*
C* T. Lee/SAIC		06/06		Updated to 5.9.3a		*
C* T. Piper/SAIC	06/06		Updated to 5.9.3b		*
C* T. Piper/SAIC	06/06		Updated to 5.9.4		*
C* T. Piper/SAIC	08/06		Updated to 5.10.1		*
C* T. Piper/SAIC	11/06		Updated to 5.10.2		*
C* T. Piper/SAIC	02/07		Updated to 5.10.2a		*
C* T. Piper/SAIC	02/07		Updated to 5.10.3		*
C* T. Piper/SAIC	06/07		Updated to 5.10.4		*
C* T. Piper/SAIC	08/07		Updated to 5.11.1		*
C* T. Piper/SAIC	11/07		Updated to 5.11.2		*
C* T. Piper/SAIC	02/08		Updated to 5.11.3		*
C* T. Piper/SAIC	06/08		Updated to 5.11.4		*
C* T. Piper/SAIC	09/08		Updated to 5.11.4a		*
C* S. Jacobs/NCEP	03/09		Updated to 5.11.4b		*
C* S. Jacobs/NCEP	06/09		Updated to 5.11.4c		*
C* S. Jacobs/NCEP	08/09		Updated to 5.11.4d		*
C* S. Jacobs/NCEP	12/09		Updated to 5.11.4e		*
C* S. Jacobs/NCEP	 1/10		Updated to 6.0			*
C* S. Jacobs/NCEP	 6/10		Updated to 6.1.0		*
C* S. Jacobs/NCEP	 6/10		Updated to 6.1.1		*
C* S. Jacobs/NCEP	 9/10		Updated to 6.2.0		*
C* S. Jacobs/NCEP	12/10		Updated to 6.3.0		*
C* S. Jacobs/NCEP	 5/11		Updated to 6.4.0		*
C* T. Lee/NCEP		11/11		Updated to 6.5.0		*
C* S. Jacobs/NCEP	 3/12		Updated to 6.6.0		*
C* S. Jacobs/NCEP	 6/12		Updated to 6.7.0		*
C* S. Jacobs/NCEP	10/12		Updated to 6.8.0		*
C* S. Jacobs/NCEP	 1/13		Updated to 6.9.0		*
C* S. Jacobs/NCEP	 5/13		Updated to 6.10.0		*
C* S. Jacobs/NCEP	 8/13		Updated to 7.0.0		*
C* S. Jacobs/NCEP	 2/14		Updated to 7.1.0		*
C* S. Jacobs/NCEP	 4/14		Updated to 7.2.0		*
C* S. Gilbert/NCEP	 4/14		Backdated to 7.1.1		*
C* S. Jacobs/NCEP	 6/14		Updated to 7.2.0		*
C* S. Jacobs/NCEP	 6/15		Updated to 7.2.0-3		*
C* S. Gilbert/NCEP      10/15           Updated to 7.3.0                *
C* S. Gilbert/NCEP      02/16           Updated to 7.3.1                *
C* S. Gilbert/NCEP      12/16           Updated to 7.4.0                *
C* J. Huber/NCEP        08/17           Updated to 7.4.1                *
C* S. Guan/NCEP         12/17           Updated to 7.4.2                *
C* M. James/UCAR         4/18           Updated to 7.4.3                *
C* M. James/UCAR        10/18           Updated to 7.4.4                *
C* M. James/UCAR        12/18           Updated to 7.4.5                *
C************************************************************************
	CHARACTER*(*)	vmsg
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the version string and add a null to the end.
C
	vmsg = 'Version 7.4.5'
	CALL ST_NULL ( vmsg, vmsg, lens, ier )
C*
	RETURN
	END
