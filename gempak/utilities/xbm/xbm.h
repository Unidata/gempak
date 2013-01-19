/************************************************************************
 * xbm.h								*
 *									*
 * This header file declares the variables used in the XBM driver.	*
 *									*
 **									*
 * Log:									*
 * T. Piper		12/07	Created					*
 ***********************************************************************/

#ifdef PGLOBAL

	int		bpp;
	char		*pixmap;
	float		rot;
	Boolean		landscape;
	Cardinal	bpscan;
	Cardinal	num_scans;

#else

	extern int	bpp;
	extern char	*pixmap;
	extern float	rot;
	extern Boolean	landscape;
	extern Cardinal	bpscan;
	extern Cardinal	num_scans;

#endif
