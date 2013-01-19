#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgftbl.h"

typedef struct layerinfo {
    char	layer_name[FILE_NAMESZ];
    Boolean     in_use;
    Boolean	fill;
    Boolean	dsply_on;
    Boolean	dsply_all_color;
    int		mono_color_val;
    char	file_name[FILE_NAMESZ];
    char	file_path[LLPATH];
    int		dir_position;
    Boolean	file_saved;
    Boolean	changes_made;    
    int		def_grp;
} LayerInfo;

static LayerInfo pgen_layer[MAX_LAYERS];
static int _curLayer = 0;
static int _numLayers = 1;     /* number of layers currently in use */

/************************************************************************
 * nmap_pglayer.c							*
 *									*
 * This module handles all layering-related functionality in PGEN.	*
 * 									*
 * CONTENTS:								*
 *  pglayer_init()		Initilizes all layer-related attributes	*
 *  pglayer_setCurLayer()	Sets the value of current PGEN layer	*
 *  pglayer_setFileName()	Sets the VG file name on a PGEN layer	*
 *  pglayer_setFilePath()	Sets the VG file path on a PGEN layer	* 
 *  pglayer_setDirPos()		Sets the dir. position on a PGEN layer	*
 *  pglayer_setFileSaved()	Sets the file_saved flag on a PGEN layer*
 *  pglayer_setChngMade()	Sets changes_made flag on a PGEN layer	*
 *  pglayer_setName()           sets the name of a particular layer     *
 *  pglayer_setInUse()          sets the in_use flag of the layer       *
 *  pglayer_setFill()           sets the fill flag of the layer         *
 *  pglayer_setDsplOn()         sets the dsply_on flag of the layer     *
 *  pglayer_setDsplClr()        sets the dsply_all_color flag           *
 *  pglayer_setMonoClr()        sets the mono_color_val for a layer     *
 *  pglayer_setDefGrp()		sets the def_grp value for a layer	*
 *                                                                      *
 *  pglayer_getCurLayer()	Gets the value of current PGEN layer	*
 *  pglayer_getNumLayers()      gets the number of layers in use        *
 *  pglayer_getFileName()	Gets the VG file name on a PGEN layer	*
 *  pglayer_getFilePath()	Gets the VG file path on a PGEN layer	*
 *  pglayer_getDirPos()		Gets the dir. position on a PGEN layer	*
 *  pglayer_isFileSaved()	Checks file_saved flag on a PGEN layer	*
 *  pglayer_isChngMade()	Checks changes_made flag on a PGEN layer*
 *  pglayer_getName()           gets the name of a particular layer     *
 *  pglayer_getInUse()          gets the in_use flag of the layer       *
 *  pglayer_getFill()           gets the fill flag of the layer         *
 *  pglayer_getDsplOn()         gets the dsply_on flag of the layer     *
 *  pglayer_getDsplClr()        gets the dsply_all_color                *
 *  pglayer_getMonoClr()        gets the mono_color_val for a layer     *
 *  pglayer_getDefGrp()		gets the default group type for a layer *
 *  pglayer_getChngLayer()	gets next changed layer number in PGEN	*
 ***********************************************************************/

/*=====================================================================*/

void pglayer_init ( void )
/************************************************************************
 * pglayer_init								*
 *									*
 * This function initilizes all layer-related attributes.		*
 *									*
 * void pglayer_init( void )						*
 *									*
 * Input/Output parameters:						*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	12/01	initial coding				*
 * H. Zeng/EAI          01/02   changed variable name                   *
 * J. Wu/SAIC	 	03/02	start layering with layer one only	*
 * E. Safford/SAIC	03/02	change def color from 4 to 19		*
 * E. Safford/SAIC	03/02	add reset of _numLayers to 1  		*
 * J. Wu/SAIC	 	04/02	set 'dir_position' to _vgfUsrTbl.nitems	*
 * H. Zeng/XTRIA	03/03   changed def_grp default value           *
 ***********************************************************************/
{
    int			ii;
/*---------------------------------------------------------------------*/
 
    _curLayer = 0;
    _numLayers = 1;
    
    for ( ii = 0; ii < MAX_LAYERS; ii++ ) {
        sprintf( pgen_layer[ ii ].layer_name, "layer_%i", ii + 1 );          
        pgen_layer[ ii ].in_use = (ii == 0) ? TRUE : FALSE;
        pgen_layer[ ii ].fill = True;
        pgen_layer[ ii ].dsply_on = True;
        pgen_layer[ ii ].dsply_all_color = True;
        pgen_layer[ ii ].mono_color_val = 19;
        pgen_layer[ ii ].file_name[0] = '\0';	
        strcpy ( pgen_layer[ ii ].file_path, "./" );	
        pgen_layer[ ii ].dir_position = _vgfUsrTbl.nitems;	
        pgen_layer[ ii ].file_saved = FALSE;	
        pgen_layer[ ii ].changes_made = FALSE;	        	
	pgen_layer[ ii ].def_grp = NON_GRPID;
    }
        
}

/*=====================================================================*/

int pglayer_getCurLayer ( void )
/************************************************************************
 * pglayer_getCurLayer							*
 *									*
 * This function returns the value of PGEN current layer.		*
 *									*
 * int pglayer_getCurLayer( void )					*
 *									*
 * Input parameters:							*
 *	none								*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	pglayer_getCurLayer	int	The current PGEN layer value	*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	12/01	initial coding				*
 * H. Zeng/EAI          01/02   changed variable name                   *
 ***********************************************************************/
{
    return _curLayer;
}

/*=====================================================================*/

void pglayer_setCurLayer ( int layer )
/************************************************************************
 * pglayer_setCurLayer							*
 *									*
 * This function sets the value of the current PGEN layer.		*
 *									*
 * void pglayer_setCurLayer( layer )					*
 *									*
 * Input parameters:							*
 *	layer		int	value to be set as the current layer	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{
    if ( layer >= 0 && layer < MAX_LAYERS ) {
       _curLayer = layer;
    }    
}

/*=====================================================================*/

void pglayer_setFileName ( int layer, char *file_name )
/************************************************************************
 * pglayer_setFileName							*
 *									*
 * This function sets the VG file name on a given PGEN layer.		*
 *									*
 * void pglayer_setFileName( layer, file_name )				*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer to be set 			*
 *	*file_name	char	File name to be set on the given layer	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        strcpy ( pgen_layer[layer].file_name, file_name );
    }
}

/*=====================================================================*/

void pglayer_getFileName ( int layer, char *file_name )
/************************************************************************
 * pglayer_getFileName							*
 *									*
 * This function gets the VG file name on the given PGEN layer.		*
 *									*
 * void pglayer_getFileName( layer, file_name )				*
 *									*
 * Input parameters:							*
 *	layer		int	Layer value				*
 *									*
 * Output parameters:							*
 *	*file_name	char	VG file name on the given layer		*
 *									*
 * Return value:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{    
    file_name[0] = '\0';
    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        strcpy ( file_name, pgen_layer[layer].file_name );
    }
}

/*=====================================================================*/

void pglayer_setFilePath ( int layer, char *file_path )
/************************************************************************
 * pglayer_setFilePath							*
 *									*
 * This function sets the VG file path on a given PGEN layer.		*
 *									*
 * void pglayer_setFilePath( layer, file_path )				*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer to be set 			*
 *	*file_path	char	File path to be set on the given layer	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        strcpy ( pgen_layer[layer].file_path, file_path );
    }
}

/*=====================================================================*/

void pglayer_getFilePath ( int layer, char *file_path )
/************************************************************************
 * pglayer_getFilePath							*
 *									*
 * This function gets the VG file path on the given PGEN layer.		*
 *									*
 * void pglayer_getFilepath( layer, file_path )				*
 *									*
 * Input parameters:							*
 *	layer		int	Layer value				*
 *									*
 * Output parameters:							*
 *	*file_path	char	VG file path on the given layer		*
 *									*
 * Return value:							*
 *	none								*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        strcpy ( file_path, pgen_layer[layer].file_path );
    }
    else {
        strcpy ( file_path, "./" );    
    }    
}

/*=====================================================================*/

void pglayer_setDirPos ( int layer, int dir_pos )
/************************************************************************
 * pglayer_setDirPos							*
 *									*
 * This function sets the directory position for a given layer.		*
 *									*
 * void pglayer_setDirPos( layer, dir_pos )				*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer	 			*
 *	dir_pos		int	Directory position to be set 		*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        pgen_layer[layer].dir_position = dir_pos;
    }
}

/*=====================================================================*/

void pglayer_getDirPos ( int layer, int *dir_pos )
/************************************************************************
 * pglayer_getDirPos							*
 *									*
 * This function gets the directory position for a given layer.		*
 *									*
 * void pglayer_getDirPos( layer, dir_pos )				*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer	 			*
 *									*
 * Output parameters:							*
 *	*dir_pos	int	Directory position on the given layer	*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{
    *dir_pos = 0;
    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        *dir_pos = pgen_layer[layer].dir_position;
    }
}

/*=====================================================================*/

void pglayer_setFileSaved ( int layer, Boolean file_saved )
/************************************************************************
 * pglayer_setFileSaved							*
 *									*
 * This function set the VG file save flag for a given layer.		*
 *									*
 * void pglayer_setFileSaved ( layer, file_saved )			*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer	 			*
 *	file_saved	Boolean	Flag to show if the file is saved	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        pgen_layer[layer].file_saved = file_saved;
    }
}

/*=====================================================================*/

Boolean pglayer_isFileSaved ( int layer )
/************************************************************************
 * pglayer_isFileSaved							*
 *									*
 * This function checks if the VG file on a given layer has been saved.	*
 *									*
 * Boolean pglayer_isFileSaved ( layer )				*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer				*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	pglayer_isFileSaved	Boolean	Value of the file_saved flag	*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{    
    Boolean	file_saved = FALSE;
/*---------------------------------------------------------------------*/
    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        file_saved = pgen_layer[layer].file_saved;
    } 
    
    return  file_saved;  
}

/*=====================================================================*/

void pglayer_setChngMade ( int layer, Boolean changes_made )
/************************************************************************
 * pglayer_setChngMade							*
 *									*
 * This function set the changes-made flag for a given layer.		*
 *									*
 * void pglayer_setChngMade ( layer, changes_made )			*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer	 			*
 *	changes_made	Boolean	Flag to show if changes made on a layer	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	none								*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
        pgen_layer[layer].changes_made = changes_made;
    }
}

/*=====================================================================*/

Boolean pglayer_isChngMade ( int layer )
/************************************************************************
 * pglayer_isChngMade							*
 *									*
 * This function checks if any changes made for a given layer.		*
 *									*
 * Boolean pglayer_isChngMade ( layer )					*
 *									*
 * Input parameters:							*
 *	layer		int	PGEN layer	 			*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	pglayer_isChngMade	Boolean	Value of changes-made flag	*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	02/02	initial coding				*
 ***********************************************************************/
{    
    Boolean	changes_made = FALSE;
/*---------------------------------------------------------------------*/
    
    if ( layer >= 0 && layer < MAX_LAYERS ) {
       changes_made = pgen_layer[layer].changes_made;	
    }
    
    return changes_made;
}

/*=====================================================================*/

int pglayer_getNumLayers ( void )
/************************************************************************
 * pglayer_getNumLayers							*
 *                                                                      *
 * This function returns the value of _numLayers.                       *
 *                                                                      *
 * int	pglayer_getNumLayers()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	pglayer_getNumLayers	int	number of layers in use		*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    return _numLayers;
}

/*=====================================================================*/

void pglayer_setName ( int layer_index, char *name )
/************************************************************************
 * pglayer_setName							*
 *                                                                      *
 * This fucntion sets the name of a particular layer.                   *
 *									*
 * void	pglayer_setName(layer_index, name)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to set name	        *
 *      *name           char    name of the layer                       *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       if ( strcmp(pgen_layer[ layer_index ].layer_name, name) != 0 ) {
          strcpy(pgen_layer[ layer_index ].layer_name, name);
     
       }
    }
}

/*=====================================================================*/

char *pglayer_getName ( int layer_index )
/************************************************************************
 * pglayer_getName							*
 *                                                                      *
 * This fucntion returns the name of a particular layer.                *
 *									*
 * char*  pglayer_getName(layer_index)				        *
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to get name	        *
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 * Return parameters:                                                   *
 *      pglayer_getName char*   name of the layer                       *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       return (pgen_layer[ layer_index ].layer_name);
       
    }

    return(NULL);

}

/*=====================================================================*/

void pglayer_setInUse ( int layer_index, Boolean active )
/************************************************************************
 * pglayer_setInUse							*
 *                                                                      *
 * This fucntion sets the in_use flag of particular layer.              *
 *									*
 * void	pglayer_setInUse(layer_index, active)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to set in_use flag	*
 *      active          Boolean make the layer active or not            *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       if ( pgen_layer[ layer_index ].in_use != active ) {
          pgen_layer[ layer_index ].in_use = active;
          _numLayers = active ? (_numLayers+1) : (_numLayers-1); 
       }
    }
}

/*=====================================================================*/

Boolean pglayer_getInUse ( int layer_index )
/************************************************************************
 * pglayer_getInUse							*
 *                                                                      *
 * This fucntion returns the in_use flag of a particular layer.         *
 *									*
 * Boolean pglayer_getInUse(layer_index)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to get in_use flag	*
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 * Return parameters:                                                   *
 *      pglayer_getInUse Boolean   in_use flag of the layer             *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       return (pgen_layer[ layer_index ].in_use);
       
    }

    return(FALSE);

}

/*=====================================================================*/

void pglayer_setFill ( int layer_index, Boolean fill )
/************************************************************************
 * pglayer_setFill							*
 *                                                                      *
 * This fucntion sets the fill flag of particular layer.                *
 *									*
 * void	pglayer_setFill(layer_index, fill)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to set fill flag	        *
 *      fill            Boolean make the layer filled or not            *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       if ( pgen_layer[ layer_index ].fill != fill ) {
          pgen_layer[ layer_index ].fill = fill;

       }
    }
}

/*=====================================================================*/

Boolean pglayer_getFill ( int layer_index )
/************************************************************************
 * pglayer_getFill							*
 *                                                                      *
 * This fucntion returns the fill flag of a particular layer.           *
 *									*
 * Boolean  pglayer_getFill(layer_index)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to get fill flag	        *
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 * Return parameters:                                                   *
 *      pglayer_getFill Boolean   fill flag of the layer                *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 * J. Wu/SAIC           02/02    correct misspelling in function name   *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       return (pgen_layer[ layer_index ].fill);
       
    }

    return(FALSE);

}

/*=====================================================================*/

void pglayer_setDsplOn ( int layer_index, Boolean dsply_on )
/************************************************************************
 * pglayer_setDsplOn							*
 *                                                                      *
 * This fucntion sets the dsply_on flag of particular layer.            *
 *									*
 * void	pglayer_setDsplOn(layer_index, dsply_on)			*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to set dsply_on flag	*
 *      dsply_on        Boolean make the layer display on or off        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       if ( pgen_layer[ layer_index ].dsply_on != dsply_on ) {
          pgen_layer[ layer_index ].dsply_on = dsply_on;

       }
    }
}

/*=====================================================================*/

Boolean pglayer_getDsplOn ( int layer_index )
/************************************************************************
 * pglayer_getDsplOn							*
 *                                                                      *
 * This fucntion returns the dsply_on flag of a particular layer.       *
 *									*
 * Boolean  pglayer_getDsplOn(layer_index)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to get dsply_on flag	*
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 * Return parameters:                                                   *
 *      pglayer_getDsplOn Boolean   dsply_on flag of the layer          *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       return (pgen_layer[ layer_index ].dsply_on);
       
    }

    return(FALSE);

}

/*=====================================================================*/

void pglayer_setDsplClr ( int layer_index, Boolean dsply_all_color )
/************************************************************************
 * pglayer_setDsplClr							*
 *                                                                      *
 * This fucntion sets the dsply_all_color flag of particular layer.     *
 *									*
 * void	pglayer_setDsplClr(layer_index, dsply_all_color)		*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to set in_use flag	*
 *      dsply_all_color Boolean make display color TRUE or FALSE        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       if ( pgen_layer[ layer_index ].dsply_all_color 
                                   != dsply_all_color ) {
          pgen_layer[ layer_index ].dsply_all_color = dsply_all_color;

       }
    }
}

/*=====================================================================*/

Boolean pglayer_getDsplClr ( int layer_index )
/************************************************************************
 * pglayer_getDsplClr							*
 *                                                                      *
 * This fucntion returns the dsply_all_color flag of a particular layer.*
 *									*
 * Boolean  pglayer_getDsplClr(layer_index)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer                          *
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 * Return parameters:                                                   *
 *      pglayer_getDsplClr Boolean   dsply_all_color flag of the layer  *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       return (pgen_layer[ layer_index ].dsply_all_color);
       
    }

    return(FALSE);

}

/*=====================================================================*/

void pglayer_setMonoClr ( int layer_index, int value )
/************************************************************************
 * pglayer_setMonoClr							*
 *                                                                      *
 * This fucntion sets the mono_color_val flag of particular layer.      *
 *									*
 * void	pglayer_setMonoClr(layer_index, value)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to set mono_color_val	*
 *      value           int     value of mono_color_val                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       if ( pgen_layer[ layer_index ].mono_color_val != value ) {
          pgen_layer[ layer_index ].mono_color_val = value;

       }
    }
}

/*=====================================================================*/

int pglayer_getMonoClr ( int layer_index )
/************************************************************************
 * pglayer_getMonoClr							*
 *                                                                      *
 * This fucntion returns the mono_color_val of a particular layer.      *
 *									*
 * int  pglayer_getMonoClr(layer_index)				        *
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer to get mono_color_val	*
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 * Return parameters:                                                   *
 *      pglayer_getMonoClr int   mono_color_val of the layer            *
 *                               -1  if layer_index is invalid.         *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          01/02    initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
       return (pgen_layer[ layer_index ].mono_color_val);
       
    }

    return(-1);

}

/*=====================================================================*/

void pglayer_setDefGrp ( int layer_index, int value )
/************************************************************************
 * pglayer_setDefGrp							*
 *                                                                      *
 * This fucntion sets the def_grp valud of a particular layer.      	*
 *									*
 * void	pglayer_setDefGrp(layer_index, value)				*
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer                      	*
 *      value           int     new default group value                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	03/02	initial coding                         *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {
        pgen_layer[ layer_index ].def_grp = value;
    }
}

/*=====================================================================*/

int pglayer_getDefGrp ( int layer_index )
/************************************************************************
 * pglayer_getDefGrp 							*
 *                                                                      *
 * This function returns the def_grp value of a particular layer.       *
 *									*
 * int  pglayer_getDefGrp( layer_index )			        *
 *									*
 * Input parameters:                                                    *
 *	layer_index	int	index of layer				* 
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 *									*
 * Return parameters:                                                   *
 *      		int	def_grp value of the requested layer    *
 *                               -1  if layer_index is invalid.         *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC	03/02	initial coding                          *
 ***********************************************************************/
{
    if ( layer_index < MAX_LAYERS && layer_index >= 0 ) {

       return (pgen_layer[ layer_index ].def_grp);
    }

    return(-1);

}

/*=====================================================================*/

int pglayer_getChngLayer ( int st_layer )
/************************************************************************
 * pglayer_getChngLayer							*
 *									*
 * This function gets the first changed layer starting from st_layer.	*
 *									*
 * int pglayer_getChngLayer ( st_layer )				*
 *									*
 * Input parameters:							*
 *	st_layer		int	   Layer to start check		*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return value:							*
 *	pglayer_getChngLayer	int	   Layer changed		*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	03/02	initial coding				*
 ***********************************************************************/
{    
    int		ii, chng_layer;
/*---------------------------------------------------------------------*/
    
    chng_layer = -1;
    
    for ( ii = st_layer; ii < MAX_LAYERS; ii++ ) {
        if ( pgen_layer[ii].changes_made ) {
            chng_layer = ii;
 	    break;
	}
    }
          
    return chng_layer;
    
}
