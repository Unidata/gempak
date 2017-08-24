#include "da.h"

/************************************************************************
 * dagetdata.c                                                          *
 *                                                                      *
 * This file contains the functions for storing and accessing the       *
 * retrieved station parameter data stored by time stamp.               *
 *                                                                      *
 * Log:                                                                 *
 * P. Moyer/NCEP     7/16   R17968 Initial coding                       *
 ***********************************************************************/

/************************************************************************
 * da_insertStation                                                     *
 *                                                                      *
 * This function creates and inserts a new station record               *
 * and its data into the existing temporary station tree.               *
 * If the station exists, add new parameters to the existing station,   *
 * overwriting the duplicates                                           *
 *                                                                      *
 * da_insertStation (newStation,paramTree,root)                         *
 *                                                                      *
 * Input parameters:                                                    *
 *    *newStation       char      string for the station identifier     *
 *    *paramTree        void      root of the created parameter tree    *
 *                                (parameter_DN*)                       *
 *                                                                      *
 * Output parameters:                                                   *
 *    **void            root      root of the station tree (which will  *
 *                                be updated (station_DN*)              *
 ************************************************************************/
void da_insertStation(char* newStation, void* paramTree, void** root){

    station_DN *leaf = 0;
    station_DN *tree = (station_DN*) *root;

        if( tree == 0 )
        {
            /* if the Root is null, assign it. */
            /* assign values */
            leaf = (station_DN*) malloc( sizeof (station_DN));
            (leaf)->station = (char *) malloc ( STRSIZE * sizeof(char) );
            strcpy((leaf)->station,newStation);
            (leaf)->paramRoot = (parameter_DN*) paramTree;

            /* set leafs to NULL */
            (leaf)->leftSDN = 0;
            (leaf)->rightSDN = 0;
            *root = leaf;

        }
        else if((strcmp((tree)->station,newStation)) < 0)
        {
            da_insertStation(newStation, paramTree, (void**) &(((station_DN*)*root)->leftSDN));
        }
        else if((strcmp((tree)->station,newStation)) > 0)
        {
            da_insertStation(newStation, paramTree, (void**) &(((station_DN*)*root)->rightSDN));
        }
        else
        {
            /* traverse incoming parameter tree, inserting a copy of each parameter into
             * the existing station node's parameter tree.
             * we are already at the tree node, so we can recursively pass it while
             * whittling down the sub-param trees.
             */

            /* calling another function for this so we can traverse it recursively*/
            da_insertStationParams(tree, paramTree);
        }
}

/************************************************************************
 * da_insertStationParams                                               *
 *                                                                      *
 * This function traverses a new parameter tree and performs parameter  *
 * insertions into an existing station's parameter tree.  Any existing  *
 * duplicates in the tree will get overwritten by the new data by the   *
 * called function insertParam.                                         *
 *                                                                      *
 * da_insertStationParams (station, newParamTree)                       *
 *                                                                      *
 * Input parameters:                                                    *
 *    *newParamTree    void    the new tree of params to be inserted    *
 *                             (parameter_DN*)                          *
 *                                                                      *
 * Output parameters:                                                   *
 *    *station  void   the station with an existing param tree          *
 *                     (station_DN*)                                    *
 ************************************************************************/
void da_insertStationParams(void* station, void* newParamTree)
{
    parameter_DN *root = (parameter_DN*) newParamTree;

    if (root != 0) {
        //attempt to insert current node's parameter values into the station's param tree

        if ((root)->paramSize == 0) {
            da_insertParam(root->paramName, root->paramSource, root->paramType, root->paramReport,
                              root->paramf, root->parami, root->paramStr,
                              (void**) &(((station_DN *)station)->paramRoot));
        }
        else {
            da_insertParamArray(root->paramName, root->paramSource, root->paramType,
                                root->paramReport, root->paramSize,
                                root->paramfArray, root->paramiArray, root->paramStrArray,
                                (void**) &(((station_DN *)station)->paramRoot));
        }

        //traverse to left
        da_insertStationParams(station, (void*) ((root)->leftPDN));

        //traverse to right
        da_insertStationParams(station, (void*) ((root)->rightPDN));
    }
}


/************************************************************************
 * da_returnStationValues                                               *
 *                                                                      *
 * This function traverses the station tree searching for the stored    *
 * station and returns its stored parameter tree if found.              *
 *                                                                      *
 *                                                                      *
 * da_returnStationValues (station,root, paramTree)                     *
 *                                                                      *
 * Input parameters:                                                    *
 *    *station   char  string containing the station label              *
 *    *root      void  root pointer to the station tree being searched  *
 *                     (station_DN*)                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *    **paramTree   void    Returned parameter tree if found (if not, 0)*
 *                          (parameter_DN*)                             *
 ************************************************************************/
void da_returnStationValueTree(char* station, void* root, void** paramTree){

    station_DN *leaf = (station_DN*)root;;

    if( leaf != 0 )
    {
        if((strcmp((leaf)->station,station))==0)
        {
            *paramTree = (leaf)->paramRoot;
        }
        else if((strcmp(leaf->station,station))<0)
        {
            da_returnStationValueTree(station, (void*) ((leaf)->leftSDN), paramTree);
        }
        else
        {
            da_returnStationValueTree(station, (void*) ((leaf)->rightSDN), paramTree);
        }
    }
    else {
        *paramTree = 0;
    }
}

/************************************************************************
 * da_timeExists                                                        *
 *                                                                      *
 * This function checks the internal storage of timestamp data to see if*
 * the time stamp has been already processed.  Returns 1 if true, 0 if  *
 * false.  Returns the pointer to the timestampDataNode if found.       *
 *                                                                      *
 * da_timeExists (time, iret, root, result)                             *
 *                                                                      *
 * Input parameters:                                                    *
 *    *time        char   timestamp string                              *
 *    *root        void   pointer timestampDataNode (typecast to void)  *
 *                                                                      *
 * Output parameters:                                                   *
 *    **result     void   pointer timestampDataNode (typecast to void)  *
 *    *iret        int    Return code                                   *
 ************************************************************************/
void da_timeExists(char* time, int* iret, void* root, void** result)
{
    timestamp_DN *leaf = (timestamp_DN*)root;

    if( leaf != 0 )
    {
        if((strcmp((leaf)->timestamp,time))==0)
        {
            /* return pointer to current node*/
            *iret = 1;
            *result = leaf;
        }
        else if((strcmp(leaf->timestamp,time))<0)
        {
            da_timeExists(time, iret, (void*) ((leaf)->leftTDN), result);
        }
        else
        {
            da_timeExists(time, iret, (void*) ((leaf)->rightTDN), result);
        }
    }
    else {
        *iret = 0;
        *result = 0;
    }

}

/************************************************************************
 * da_paramExistsStation                                                *
 *                                                                      *
 * This function checks the passed-in parameterDataNode tree to see if  *
 * the parameter has already been inserted for this particular Station. *
 * If found, the parameter data node is returned.                       *
 *                                                                      *
 * da_paramExistsStation (root, param, iret, node)                      *
 *                                                                      *
 *                                                                      *
 * Input parameters:                                                    *
 *      *root       void   pointer parameterDataNode (typecast to void) *
 *      *char       param  parameter to search for                      *
 *      *char       source data source of the parameter                 *
 *      *char       paramReport string of the parameter's report part   *
 *                                                                      *
 * Output parameters:                                                   *
  *     *iret       int    Return code                                  *
  *     *node       void   returned parameterDataNode (typecast to void)*
 ************************************************************************/
void da_paramExistsStation(void* root, char* param, char* source, char* paramReport, int* iret, void** node)
{
    parameter_DN *leaf = (parameter_DN*)root;
    int paramcmp = 0;

    if( leaf != 0 )
    {
        da_paramcmp((leaf)->paramName, (leaf)->paramSource, (leaf)->paramReport,
                    param, source, paramReport, &paramcmp);
        if(paramcmp==0)
        {
            /* return pointer to current node*/
            *iret = 1;
            *node = leaf;
        }
        else if(paramcmp<0)
        {
            da_paramExistsStation((void*) ((leaf)->leftPDN),param, source, paramReport, iret, node);
        }
        else
        {
            da_paramExistsStation((void*) ((leaf)->rightPDN), param, source, paramReport, iret, node);
        }
    }
    else {
        *iret = 0;
        *node = 0;
    }
}

/************************************************************************
 * da_insertTimestamp                                                   *
 *                                                                      *
 * This function creates and inserts a new timestamp record             *
 * and its station data into the existing timestamp tree.               *
 *                                                                      *
 *                                                                      *
 * da_insertTimestamp (timestamp,stationRoot, timestampRoot)            *
 *                                                                      *
 * Input parameters:                                                    *
 *     *timestamp    char  string form of timestamp                     *
 *     *stationRoot  void  pointer to station_DN tree to insert         *
 *                                                                      *
 * Output parameters:                                                   *
 *    **timestampRoot void pointer to the root of the timestamp tree    *
 ************************************************************************/
void da_insertTimestamp(char* timestamp, void* stationRoot, void** timestampRoot){

    timestamp_DN *leaf = 0;
    timestamp_DN *tree = (timestamp_DN*) *timestampRoot;
    station_DN *station = (station_DN*) stationRoot;

        if( tree == 0 )
        {
            /* if the Root is null, assign it. */
            /* assign values */
            leaf = (timestamp_DN*) malloc( sizeof (timestamp_DN));
            (leaf)->timestamp = (char *) malloc ( STRSIZE * sizeof(char) );
            strcpy((leaf)->timestamp,timestamp);
            (leaf)->paramNames = 0;
            (leaf)->stationRoot = station;

            /* set leafs to NULL */
            (leaf)->leftTDN = 0;
            (leaf)->rightTDN = 0;
            *timestampRoot = leaf;

        }
        else if((strcmp((tree)->timestamp,timestamp)) < 0)
        {
            da_insertTimestamp(timestamp, stationRoot, (void**) &(((timestamp_DN*)*timestampRoot)->leftTDN));
        }
        else if((strcmp((tree)->timestamp,timestamp)) > 0)
        {
            da_insertTimestamp(timestamp, stationRoot, (void**) &(((timestamp_DN*)*timestampRoot)->rightTDN));
        }
}

/************************************************************************
 * da_insertTimestampParams                                             *
 *                                                                      *
 * This function  inserts new parameter name records                    *
 * into the timestamp's parameter name tree.                            *
 *                                                                      *
 *                                                                      *
 *da_insertTimestampParam(timestampRoot,params,parmSize,type,parmSource)*
 *                                                                      *
 * Input parameters:                                                    *
 *     *timestampRoot  void  pointer to the timestamp (typecast to void)*
 *     **params        char  array of parameter names, in initial order *
 *     parmSize        int   size of the parameter array                *
 *     type            int   daret_t enum of parameter type             *
 *     *parmSource     char  string for the parameter DB source         *
 *     *paramReport char string of the parameter's report part          *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 ************************************************************************/
void da_insertTimestampParams(void* timestampRoot, char** params, int parmSize, int type, char* parmSource, char* paramReport){

    int index;
    timestamp_DN* timeRoot = (timestamp_DN*) timestampRoot;

    for (index = 0; index < parmSize; index++) {
        da_insertTimesParam((void**)&(((timeRoot)->paramNames)), params[index], type, parmSource, paramReport);
    }
}

/************************************************************************
 * da_insertTimesParam                                                  *
 *                                                                      *
 * This function creates and inserts a new parameter name record        *
 * into the existing temporary parameter name tree. Duplicates are      *
 * ignored.                                                             *
 *                                                                      *
 *                                                                      *
 * da_insertTimesParam(root,param,typeVal,parmSource)                   *
 *                                                                      *
 * Input parameters:                                                    *
 *    *param      char    name of the parameter                         *
 *     typeVal    int     daret_t enum of parameter type                *
 *    *parmSource char    database source of the parameter              *
 *    *paramReport char   string of the parameter's report part         *
 *                                                                      *
 * Output parameters:                                                   *
 *    **root    void    pointer to the parameter name tree              *
 *                                                                      *
 ************************************************************************/
void da_insertTimesParam(void** root, char* param, int typeVal, char* parmSource, char* paramReport){
    parameter_NN *leaf = 0;
    parameter_NN *tree = (parameter_NN*) *root;
    daret_t dataType = (daret_t) typeVal;

    int paramcmp = 0;

    if (tree != 0) {
        da_paramcmp((tree)->paramName, (tree)->paramSource, (tree)->paramReport,
                    param, parmSource, paramReport, &paramcmp);
    }

        if( tree == 0 )
        {
            /* if the Root is null, assign it. */
            /* assign values */
            leaf = (parameter_NN*) malloc( sizeof (parameter_NN));
            (leaf)->paramName = (char *) malloc ( STRSIZE * sizeof(char) );
            (leaf)->paramSource = (char *) malloc ( STRSIZE * sizeof(char) );
            (leaf)->paramReport = (char *) malloc ( STRSIZE * sizeof(char) );
            strcpy((leaf)->paramName,param);
            strcpy((leaf)->paramSource,parmSource);
            (leaf)->paramType = dataType;
            strcpy((leaf)->paramReport,paramReport);

            /* set leafs to NULL */
            (leaf)->leftPNN = 0;
            (leaf)->rightPNN = 0;
            *root = leaf;

        }
        else if(paramcmp < 0)
        //else if((strcmp((tree)->paramName,param)) < 0)
        {
            da_insertTimesParam((void**) &(((parameter_NN*)*root)->leftPNN), param, typeVal, parmSource, paramReport);
        }
        else if(paramcmp > 0)
        //else if((strcmp((tree)->paramName,param)) > 0)
        {
            da_insertTimesParam((void**) &(((parameter_NN*)*root)->rightPNN), param, typeVal, parmSource, paramReport);
        }
        else
        {
            //parameter string is equal, replace the data type?
        }
}

/************************************************************************
 * da_insertParam                                                       *
 *                                                                      *
 * This function creates and inserts a new parameter record             *
 * and its data into the existing temporary parameter tree.             *
 * If the parameter already exists, it replaces the type and value      *
 *                                                                      *
 *                                                                      *
 * da_insertParam (paramName, paramSource, typeVal, valuef, valuei,     *
 *                 valuec, root)                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *    *paramName  char    name of the parameter                         *
 *     typeVal    int     daret_t enum of parameter type                *
 *    *parmSource char    database source of the parameter              *
 *    *reportPart char    the report part identifier                    *
 *     valuef     float   float value for the parameter                 *
 *     valuei     integer integer value for the parameter               *
 *    *valuec     char    string value for the parameter                *
 *                                                                      *
 * Output parameters:                                                   *
 *    **root      void    root of the parameterDN tree to insert into   *
 ************************************************************************/
void da_insertParam(char* paramName, char* paramSource, int typeVal, char* reportPart, float valuef, int valuei, char* valuec, void** root){
    parameter_DN *leaf = 0;
    parameter_DN *tree = (parameter_DN*) *root;
    daret_t dataType = (daret_t) typeVal;

    int paramcmp = 0;

    if (tree != 0) {
        da_paramcmp((tree)->paramName, (tree)->paramSource, (tree)->paramReport, paramName, paramSource, reportPart, &paramcmp);
    }

        if( tree == 0 )
        {
            /* if the Root is null, assign it. */
            /* assign values */
            leaf = (parameter_DN*) malloc( sizeof (parameter_DN));
            (leaf)->paramName = (char *) malloc ( STRSIZE * sizeof(char) );
            (leaf)->paramSource = (char *) malloc ( STRSIZE * sizeof(char) );
            (leaf)->paramReport = (char *) malloc ( STRSIZE * sizeof(char) );
            strcpy((leaf)->paramName,paramName);
            strcpy((leaf)->paramSource,paramSource);
            (leaf)->paramType = dataType;
            strcpy((leaf)->paramReport,reportPart);

            /*set defaults*/
            (leaf)->paramSize = 0;
            (leaf)->parami = 0;
            (leaf)->paramf = 0;
            (leaf)->paramStr = 0;
            (leaf)->paramfArray = 0;
            (leaf)->paramiArray = 0;
            (leaf)->paramStrArray = 0;

            /* use case statement to set actuals */
            switch ( dataType ) {
                case DAINT:
                    (leaf)->parami = valuei;
                    break;
                case DAFLOAT:
                    (leaf)->paramf = valuef;
                    break;
                case DACHAR:
                    (leaf)->paramStr = (char *) malloc ( STRSIZE * sizeof(char) );
                    strcpy((leaf)->paramStr, valuec);
                    break;
            }

            /* set leafs to NULL */
            (leaf)->leftPDN = 0;
            (leaf)->rightPDN = 0;
            *root = leaf;
        }
        else if(paramcmp < 0)
        {
            da_insertParam(paramName, paramSource, typeVal, reportPart, valuef, valuei, valuec, (void**) &(((parameter_DN*)*root)->leftPDN));
        }
        else if(paramcmp > 0)
        {
            da_insertParam(paramName, paramSource, typeVal, reportPart, valuef, valuei, valuec, (void**) &(((parameter_DN*)*root)->rightPDN));
        }
        else
        {
            //parameter strings are equal, replace the type and value
            (tree)->paramType = dataType;
            switch ( dataType ) {
                case DAINT:
                    (tree)->parami = valuei;
                    break;
                case DAFLOAT:
                    (tree)->paramf = valuef;
                    break;
                case DACHAR:
                    free((tree)->paramStr);
                    (tree)->paramStr = (char *) malloc ( STRSIZE * sizeof(char) );
                    strcpy((tree)->paramStr, valuec);
                    break;
            }
        }
}

/************************************************************************
 * da_insertParamArray                                                  *
 *                                                                      *
 * This function creates and inserts a new parameter record             *
 * and its data array into the existing temporary parameter tree.       *
 * If the parameter already exists, it replaces the type and value      *
 *                                                                      *
 *                                                                      *
 * da_insertParamArray (paramName, paramSource, typeVal, size, valuef,  *
 *                 valuei, valuec, root)                                *
 *                                                                      *
 * Input parameters:                                                    *
 *    *paramName  char    name of the parameter                         *
 *     typeVal    int     daret_t enum of parameter type                *
 *    *parmSource char    database source of the parameter              *
 *    *reportPart char    the report part identifier                    *
 *     size       int     size of the passed-in array                   *
 *    *valuef     float   float array for the parameter                 *
 *    *valuei     integer integer array for the parameter               *
 *   **valuec     char    string array for the parameter                *
 *                                                                      *
 * Output parameters:                                                   *
 *    **root      void    root of the parameterDN tree to insert into   *
 ************************************************************************/
void da_insertParamArray(char* paramName, char* paramSource, int typeVal, char* reportPart, int size, float* valuef, int* valuei, char** valuec, void** root){
    parameter_DN *leaf = 0;
    parameter_DN *tree = (parameter_DN*) *root;
    daret_t dataType = (daret_t) typeVal;

    int paramcmp = 0;

    if (tree != 0) {
        da_paramcmp((tree)->paramName, (tree)->paramSource, (tree)->paramReport, paramName, paramSource, reportPart, &paramcmp);
    }

        if( tree == 0 )
        {
            /* if the Root is null, assign it. */
            /* assign values */
            leaf = (parameter_DN*) malloc( sizeof (parameter_DN));
            (leaf)->paramName = (char *) malloc ( STRSIZE * sizeof(char) );
            (leaf)->paramSource = (char *) malloc ( STRSIZE * sizeof(char) );
            (leaf)->paramReport = (char *) malloc ( STRSIZE * sizeof(char) );
            strcpy((leaf)->paramName,paramName);
            strcpy((leaf)->paramSource,paramSource);
            (leaf)->paramType = dataType;
            strcpy((leaf)->paramReport,reportPart);

            /*set defaults*/
            (leaf)->paramSize = size;
            (leaf)->parami = 0;
            (leaf)->paramf = 0;
            (leaf)->paramStr = 0;
            (leaf)->paramfArray = 0;
            (leaf)->paramiArray = 0;
            (leaf)->paramStrArray = 0;

            /* use case statement to set actuals */
            /* we are assigning values from passed-in pointers,
             * which means we don't need to allocate new memory*/
            switch ( dataType ) {
                case DAINT:
                    (leaf)->paramiArray = valuei;
                    break;
                case DAFLOAT:
                    (leaf)->paramfArray = valuef;
                    break;
                case DACHAR:
                    (leaf)->paramStrArray = valuec;
                    break;
            }

            /* set leafs to NULL */
            (leaf)->leftPDN = 0;
            (leaf)->rightPDN = 0;
            *root = leaf;
        }
        else if(paramcmp < 0)
        {
            da_insertParamArray(paramName, paramSource, typeVal, reportPart, size, valuef, valuei, valuec, (void**) &(((parameter_DN*)*root)->leftPDN));
        }
        else if(paramcmp > 0)
        {
            da_insertParamArray(paramName, paramSource, typeVal, reportPart, size, valuef, valuei, valuec, (void**) &(((parameter_DN*)*root)->rightPDN));
        }
        else
        {
            //parameter strings are equal, replace the type and value
            //deallocate any arrays being pointed to beforehand.
            //replace with pointers to newly allocated arrays from outside.
            (tree)->paramType = dataType;
            switch ( dataType ) {
                case DAINT:
                    free((tree)->paramiArray);
                    (tree)->paramiArray = valuei;
                    break;
                case DAFLOAT:
                    free((tree)->paramfArray);
                    (tree)->paramfArray = valuef;
                    break;
                case DACHAR:
                    free((tree)->paramStrArray);
                    (tree)->paramStrArray = valuec;
                    break;
            }
        }
}

/************************************************************************
 * da_paramExistsTimestamp                                              *
 *                                                                      *
 * This function determines if any parameters in the list are not       *
 * present in the timestamp's parameter name tree.                      *
 * Returns true if all the params in the list exist in timestamp, false *
 * if not.  It does not return the parameters in question               *
 *                                                                      *
 * da_paramExistsTimestamp(timestampRoot, params, parmSize, parmSource, *
 *                         paramReport, iret)                           *
 *                                                                      *
 * Input parameters:                                                    *
 *     *timestampRoot   void   pointer to a timestamp record            *
 *     **params         char   array of parameters                      *
 *     parmSize         int    size of the parameter array              *
 *     *parmSource      char   database source of the parameters        *
 *     *paramReport     char   string of the parameter's report part    *
 *                                                                      *
 * Output parameters:                                                   *
 *    *iret     int     return value                                    *
 ************************************************************************/
void da_paramExistsTimestamp(void* timestampRoot, char** params, int parmSize, char* parmSource, char* paramReport, int* iret){

    int index;
    timestamp_DN* timeRoot = (timestamp_DN*) timestampRoot;
    int returnval = 1; //assumes all exist unless one is found otherwise
    int parmExists = 0;

    for (index = 0; index < parmSize; index++) {
        da_paramExistsTree((void*)((timeRoot)->paramNames), params[index], parmSource, paramReport, &parmExists);
        if(parmExists == 0) {
            returnval = 0;
        }
    }

    *iret = returnval;
}

/************************************************************************
 * da_paramExistsTree                                                   *
 *                                                                      *
 * This function determines if an existing parameter is present inside  *
 * the named parameter tree.                                            *
 *                                                                      *
 *                                                                      *
 * da_paramExistsTree(root,param,parmSource,iret )                      *
 *                                                                      *
 * Input parameters:                                                    *
 *     *root    void   pointer to the root of a parameter name tree     *
 *     *param   char   string of the parameter name                     *
 *     *parmSource char  string of the parameter's database source      *
 *     *paramReport char string of the parameter's report part          *
 *                                                                      *
 * Output parameters:                                                   *
 *    *iret   int    return value                                       *
 ************************************************************************/
void da_paramExistsTree(void* root, char* param, char* parmSource, char* paramReport, int* iret){
    parameter_NN *leaf = (parameter_NN*)root;
    int paramcmp = 0;

    if( leaf != 0 )
    {
        da_paramcmp((leaf)->paramName, (leaf)->paramSource, (leaf)->paramReport,
                     param, parmSource, paramReport, &paramcmp);

        if(paramcmp == 0)
        {
            *iret = 1;
        }
        else if(paramcmp < 0)
        {
            da_paramExistsTree((void*) ((leaf)->leftPNN), param, parmSource, paramReport, iret);
        }
        else
        {
            da_paramExistsTree((void*) ((leaf)->rightPNN), param, parmSource, paramReport, iret);
        }
    }
    else {
        *iret = 0;
    }
}

/************************************************************************
 * da_paramcmp                                                          *
 *                                                                      *
 * This function compares two parameter names, sources, and reports,    *
 * determining whether it is equal, greater-than, or less-than.  It     *
 * returns 0, 1, or -1 - it does not report the index where the two are *
 * different. Names have priority over sources. Sources have priority   *
 * over reports.                                                        *
 *                                                                      *
 * da_paramcmp(param1,source1,report1,param2,source2,report2,iret)      *
 *                                                                      *
 * Input parameters:                                                    *
 *     *param1   char   parameter 1 name                                *
 *     *source1  char   parameter 1 source                              *
 *     *report1  char   parameter 1 report                              *
 *     *param2   char   parameter 2 name                                *
 *     *source2  char   parameter 2 source                              *
 *     *report2  char   parameter 2 report                              *
 *                                                                      *
 * Output parameters:                                                   *
 *    *iret  int   return value                                         *
 ************************************************************************/
void da_paramcmp(char* param1, char* source1, char* report1, char* param2, char* source2, char* report2, int* iret)
{
    if(strcmp(param1, param2)==0)
    {
        if(strcmp(source1, source2)==0)
        {
            if(strcmp(report1, report2)==0) {
                *iret = 0;
            }
            else if (strcmp(report1, report2) < 0)
            {
                *iret = -1;
            }
            else
            {
                *iret = 1;
            }
        }
        else if (strcmp(source1, source2) < 0)
        {
            *iret = -1;
        }
        else
        {
            *iret = 1;
        }
    }
    else if (strcmp(param1, param2) < 0)
    {
        *iret = -1;
    }
    else
    {
        *iret = 1;
    }
}
