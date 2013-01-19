/**************************************************************************
 Header file for parseAWC_XML.c library module

 Purpose:  Define structures and prototype declarations for parseAWC_XML.c
 
 Log:  L. Hinson/AWC     02/10    Created
**************************************************************************/
#ifndef _PARSEAWCXML_H
#define _PARSEAWCXML_H

#include <libxml/xmlreader.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

int get_number_of_nodes(xmlDocPtr doc,const xmlChar* xpathExpr);
int getInfo(xmlDocPtr doc, const xmlChar* xpathExpr,char* output);
void getInfoStr(xmlDocPtr doc, const char* xpathExpr, const char* element, char*
str);

void getInfoStrbySub(xmlDocPtr doc, const char* xpathExpr, const char*
element_cstring, int sub, char* str);

void getInfoInt(xmlDocPtr doc, const char* xpathExpr,const char* element,int*
number);

void getInfoIntbySub(xmlDocPtr doc, const char* xpathExpr,const char*
element_cstring,int sub,int *number);

void getInfoFloat(xmlDocPtr doc, const char* xpathExpr,const char*
element,float* real);

void getInfoFloatbySub(xmlDocPtr doc, const char* xpathExpr,const char*
element_cstring, int sub, float *real);
int elementExists(xmlDocPtr doc, const char* xpathExpr, const char* element);
void loadXmlfile(char **xmlString, char *filename, int *ier);
#endif
