#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct {
  int TheDim;
  int TheModulo;
  int nbGen;
  int ***TheGroupGenerators;
} TheInput;


typedef struct {
  int *Vect1;
  int *Vect2;
} TheWork;


typedef struct {
  int MaxElement;
  int *ListElementInSubset;
  int *ListNext;
  int *ListPrev;
} IntegerSubsetStorage;


void OrbSpl_Assignments(TheInput *TheINP, TheWork *TheWORK, FILE *f)
{
  int TheDim, TheModulo, nbGen, iGen, i, j, TheVal;
  fscanf(f, "%d", &TheDim);
  TheINP->TheDim=TheDim;
  fprintf(stderr, "TheDim=%d\n", TheDim);
  
  fscanf(f, "%d", &TheModulo);
  TheINP->TheModulo=TheModulo;
  fprintf(stderr, "TheModulo=%d\n", TheModulo);

  fscanf(f, "%d", &nbGen);
  TheINP->nbGen=nbGen;
  fprintf(stderr, "nbGen=%d\n", nbGen);

  if ((TheINP->TheGroupGenerators = (int***)malloc(nbGen*sizeof(int**))) == 0)
    exit (EXIT_FAILURE);
  for (iGen=1; iGen<=nbGen; iGen++)
    if ((TheINP->TheGroupGenerators[iGen-1] = (int**)malloc(TheDim*sizeof(int*))) == 0)
      exit (EXIT_FAILURE);
  for (iGen=1; iGen<=nbGen; iGen++)
    for (i=1; i<=TheDim; i++)
      if ((TheINP->TheGroupGenerators[iGen-1][i-1] = (int*)malloc(TheDim*sizeof(int))) == 0)
	exit (EXIT_FAILURE);
  for (iGen=1; iGen<=nbGen; iGen++)
    for (i=1; i<=TheDim; i++)
      for (j=1; j<=TheDim; j++)
	{
	  fscanf(f, "%d", &TheVal);
	  TheVal=TheVal % TheModulo;
	  if (TheVal < 0)
	    {
	      TheVal=TheModulo+TheVal;
	    }
	  TheINP->TheGroupGenerators[iGen-1][i-1][j-1]=TheVal;
	}
  for (iGen=1; iGen<=nbGen; iGen++)
    {
      fprintf(stderr, "Generators Nr=%d\n", iGen);
      for (i=1; i<=TheDim; i++)
	{
	  for (j=1; j<=TheDim; j++)
	    {
	      fprintf(stderr, " %d", TheINP->TheGroupGenerators[iGen-1][i-1][j-1]);
	    }
	  fprintf(stderr, "\n");
	}
    }
  if ((TheWORK->Vect1 = (int*)malloc(TheDim*sizeof(int))) == 0)
    exit (EXIT_FAILURE);
  if ((TheWORK->Vect2 = (int*)malloc(TheDim*sizeof(int))) == 0)
    exit (EXIT_FAILURE);
}


void ConvertLongLong2Vector(TheInput *TheINP, long long *Uin, int **Uout)
{
  long long ThePow, Pow2, TheLong, res;
  long long iPos;
  int delta;
  ThePow=1;
  TheLong=*Uin;
  for (iPos=1; iPos<=TheINP->TheDim; iPos++)
    {
      Pow2=ThePow*TheINP->TheModulo;
      res=TheLong % Pow2;
      delta=res/ThePow;
      TheLong=TheLong-res;
      (*Uout)[iPos-1]=delta;
      ThePow=ThePow*TheINP->TheModulo;
    }
}

void ConvertVector2LongLong(TheInput *TheINP, int **Uin, long long *Uout)
{
  long ThePow, TheLong;
  long iPos;
  ThePow=1;
  TheLong=0;
  for (iPos=1; iPos<=TheINP->TheDim; iPos++)
    {
      TheLong=TheLong+(*Uin)[iPos-1]*ThePow;
      ThePow=ThePow*TheINP->TheModulo;
    }
  *Uout=TheLong;
}




void TheMultiplication(long long *TheIn, long long *TheOutput, 
		       TheWork *TheWORK,
		       TheInput *TheINP, int iGen)
{
  int TheSum, i, j;
  long long h;
  h=*TheIn-1;
  ConvertLongLong2Vector(TheINP, &h, &(TheWORK->Vect1));
  for (i=1; i<=TheINP->TheDim; i++)
    {
      TheSum=0;
      for (j=1; j<=TheINP->TheDim; j++)
	TheSum=TheSum+TheWORK->Vect1[j-1]*TheINP->TheGroupGenerators[iGen-1][j-1][i-1];
      TheWORK->Vect2[i-1]=TheSum % TheINP->TheModulo;
    }
  ConvertVector2LongLong(TheINP, &(TheWORK->Vect2), TheOutput);
  *TheOutput=*TheOutput+1;
}


void VSLT_ZeroAssignment(IntegerSubsetStorage *VSLT)
{
  long long Maxp2, MaxElement, iVert;
  MaxElement=VSLT->MaxElement;
  Maxp2=MaxElement+2;
  for (iVert=1; iVert<=MaxElement; iVert++)
    VSLT->ListElementInSubset[iVert-1]=0;
  for (iVert=1; iVert<=Maxp2; iVert++)
    {
      VSLT->ListNext[iVert-1]=0;
      VSLT->ListNext[iVert-1]=0;
    }
  VSLT->ListNext[MaxElement+1-1]=MaxElement+2;
  VSLT->ListPrev[MaxElement+2-1]=MaxElement+1;
}
void VSLT_AssignAsCompletelyFull(IntegerSubsetStorage *VSLT)
{
  long long MaxElement, iVert;
  MaxElement=VSLT->MaxElement;
  for (iVert=1; iVert<=MaxElement; iVert++)
    VSLT->ListElementInSubset[iVert-1]=1;
  for (iVert=1; iVert<=MaxElement; iVert++)
    if (iVert == MaxElement)
      VSLT->ListNext[iVert-1]=MaxElement+2;
    else
      VSLT->ListNext[iVert-1]=iVert+1;
  VSLT->ListNext[MaxElement+1-1]=1;
  for (iVert=1; iVert<=MaxElement; iVert++)
    if (iVert == 1)
      VSLT->ListPrev[iVert-1]=MaxElement+1;
    else
      VSLT->ListPrev[iVert-1]=iVert-1;
  VSLT->ListPrev[MaxElement+2-1]=MaxElement;
}



void VSLT_InitializeStorage(IntegerSubsetStorage *VSLT, long long MaxElement)
{
  long long Maxp2;
  if ((VSLT->ListElementInSubset = (int*)malloc(MaxElement*sizeof(int))) == 0)
    exit (EXIT_FAILURE);
  Maxp2=MaxElement+2;
  if ((VSLT->ListNext = (int*)malloc(Maxp2*sizeof(int))) == 0)
    exit (EXIT_FAILURE);
  if ((VSLT->ListPrev = (int*)malloc(Maxp2*sizeof(int))) == 0)
    exit (EXIT_FAILURE);
  VSLT->MaxElement=MaxElement;
  VSLT_ZeroAssignment(VSLT);
}
void VSLT_FreeStorage(IntegerSubsetStorage *VSLT)
{
  free(VSLT->ListElementInSubset);
  free(VSLT->ListNext);
  free(VSLT->ListPrev);
}
int VSLT_TheFirstPosition(IntegerSubsetStorage *VSLT)
{
  return VSLT->ListNext[VSLT->MaxElement+1-1];
}
int VSLT_IsItInSubset(IntegerSubsetStorage *VSLT, long long pos)
{
  return VSLT->ListElementInSubset[pos-1];
}
void VSLT_PutToEmpty(IntegerSubsetStorage *VSLT)
{
  long long ThePos;
  ThePos=VSLT->MaxElement+1;
  while(1)
    {
      ThePos=VSLT->ListNext[ThePos-1];
      if (ThePos == VSLT->MaxElement+2)
	break;
      VSLT->ListElementInSubset[ThePos-1]=0;
    }
  VSLT->ListNext[VSLT->MaxElement+1-1]=VSLT->MaxElement+2;
  VSLT->ListPrev[VSLT->MaxElement+2-1]=VSLT->MaxElement+1;
}


void VSLT_StoreValue(IntegerSubsetStorage *VSLT, long long pos)
{
  long long posAfter;
  posAfter=VSLT->ListNext[VSLT->MaxElement+1-1];
  if (VSLT->ListElementInSubset[pos-1] != 0)
    {
      fprintf(stderr, "Inserting something already there\n");
      exit(EXIT_FAILURE);
    }
  VSLT->ListNext[VSLT->MaxElement+1-1]=pos;
  VSLT->ListNext[pos-1]=posAfter;
  VSLT->ListPrev[posAfter-1]=pos;
  VSLT->ListPrev[pos-1]=VSLT->MaxElement+1;
  VSLT->ListElementInSubset[pos-1]=1;
}
void VSLT_RemoveValue(IntegerSubsetStorage *VSLT, long long pos)
{
  long long posNext, posPrev;
  posNext=VSLT->ListNext[pos-1];
  posPrev=VSLT->ListPrev[pos-1];
  if (posNext == 0)
    {
      fprintf(stderr, "We have an inconsistency 1\n");
      exit(EXIT_FAILURE);
    }
  if (posPrev == 0)
    {
      fprintf(stderr, "We have an inconsistency 2\n");
      exit(EXIT_FAILURE);
    }
  VSLT->ListNext[posPrev-1]=posNext;
  VSLT->ListPrev[posNext-1]=posPrev;
  VSLT->ListNext[pos-1]=0;
  VSLT->ListPrev[pos-1]=0;
  VSLT->ListElementInSubset[pos-1]=0;
}
void VSLT_RemoveValueIfPresent(IntegerSubsetStorage *VSLT, int pos)
{
  long long posNext, posPrev;
  posNext=VSLT->ListNext[pos-1];
  posPrev=VSLT->ListPrev[pos-1];
  if (posNext != 0)
    {
      VSLT->ListNext[posPrev-1]=posNext;
      VSLT->ListPrev[posNext-1]=posPrev;
      VSLT->ListNext[pos-1]=0;
      VSLT->ListPrev[pos-1]=0;
      VSLT->ListElementInSubset[pos-1]=0;
    }
}
int VSLT_TheNextElement(IntegerSubsetStorage *VSLT, long long pos)
{
  return VSLT->ListNext[pos-1];
}
void VLST_FullPrinting(IntegerSubsetStorage *VSLT)
{
  long long idx;
  fprintf(stderr, "L=");
  idx=VSLT->MaxElement+1;
  while(1)
    {
      if (idx == VSLT->ListNext[idx-1])
	{
	  fprintf(stderr, "Database inconsistency !\n");
	  exit(EXIT_FAILURE);
	}
      idx=VSLT->ListNext[idx-1];
      if (idx == VSLT->MaxElement+2)
	break;
      fprintf(stderr, "%lld ", idx);
    }
  fprintf(stderr, "\n");
}
int VSLT_ThePrevElement(IntegerSubsetStorage *VSLT, long long pos)
{
  return VSLT->ListPrev[pos-1];
}
int VSLT_IsEmpty(IntegerSubsetStorage *VSLT)
{
  if (VSLT->ListNext[VSLT->MaxElement+1-1] == VSLT->MaxElement+2)
    return 1;
  else
    return 0;
}




void TotalEnumeration(FILE *TheOut, TheWork *TheWORK, TheInput *TheINP)
{
  long long h, TheVal, fVal, nbVect, H;
  int IsFirstPrint, i, iGen;
  IntegerSubsetStorage *TotalList;
  IntegerSubsetStorage *HotList;
  IntegerSubsetStorage *CompleteList;
  h=1;
  for (i=1; i<=TheINP->TheDim; i++)
    h=h*TheINP->TheModulo;
  TotalList=(IntegerSubsetStorage*)malloc(sizeof(IntegerSubsetStorage));
  HotList=(IntegerSubsetStorage*)malloc(sizeof(IntegerSubsetStorage));
  CompleteList=(IntegerSubsetStorage*)malloc(sizeof(IntegerSubsetStorage));
  VSLT_InitializeStorage(TotalList, h);
  VSLT_InitializeStorage(HotList, h);
  VSLT_InitializeStorage(CompleteList, h);


  VSLT_AssignAsCompletelyFull(TotalList);
  VSLT_ZeroAssignment(HotList);
  VSLT_ZeroAssignment(CompleteList);

  /*  
  TheMultiplication(&h, &fVal, 
		    TheWORK, TheINP, 
		    1);
  fprintf(stderr, "h=%lld  fVal=%lld \n", h, fVal);
  exit(EXIT_FAILURE);
  */

  fprintf(TheOut, "return [");
  IsFirstPrint=1;
  while(1)
    {
      if (VSLT_IsEmpty(TotalList) == 1)
	break;
      TheVal=VSLT_TheFirstPosition(TotalList);
      VSLT_StoreValue(HotList, TheVal);
      VSLT_StoreValue(CompleteList, TheVal);
      nbVect=1;
      if (IsFirstPrint == 1)
	IsFirstPrint=0;
      else
	fprintf(TheOut, ",");
      H=TheVal-1;
      ConvertLongLong2Vector(TheINP, &H, &(TheWORK->Vect1));
      fprintf(TheOut, "rec(eVect:=[");
      for (i=1; i<=TheINP->TheDim; i++)
	{
	  if (i > 1)
	    fprintf(TheOut, ",");
	  fprintf(TheOut, "%d", TheWORK->Vect1[i-1]);
	}
      fprintf(TheOut, "], OrbitSize:=");
      while(1)
	{
	  if (VSLT_IsEmpty(HotList) == 1)
	    break;
	  TheVal=VSLT_TheFirstPosition(HotList);
	  VSLT_RemoveValue(HotList, TheVal);
	  VSLT_RemoveValue(TotalList, TheVal);
	  for (iGen=1; iGen<=TheINP->nbGen; iGen++)
	    {
	      TheMultiplication(&TheVal, &fVal, 
				TheWORK, TheINP, 
				iGen);
	      if (VSLT_IsItInSubset(CompleteList, fVal) == 0)
		{
		  VSLT_StoreValue(HotList, fVal);
		  VSLT_StoreValue(CompleteList, fVal);
		  nbVect++;
		}
	    }
	}
      fprintf(stderr, "Find an orbit of size %lld \n", nbVect);
      fprintf(TheOut, "%lld)", nbVect);
    }
  fprintf(TheOut, "];\n");
}



int main(int argc, char *argv[])
{
  FILE *FileInput=NULL;
  FILE *FileOutput=NULL;
  TheInput TheINP;
  TheWork TheWORK;
  if (argc !=3)
    {
      fprintf(stderr, "Number of argument is = %d\n", argc);
      fprintf(stderr, "This program is used as\n");
      fprintf(stderr, "GetVect [Description] [OUTPUT]\n");
      return -1;
    }
  fprintf(stderr, "Reading description data\n");
  FileInput=fopen(argv[1], "r");
  if (FileInput == NULL)
    {
      fprintf(stderr,"description: The file %s was not found\n",argv[1]);
      return -1;
    }
  fprintf(stderr, "  description file:%s\n", argv[1]);
  OrbSpl_Assignments(&TheINP, &TheWORK, FileInput);
  fclose(FileInput);

  fprintf(stderr, "Creating output file\n");
  FileOutput=fopen(argv[2], "w");
  if (FileOutput == NULL)
    {
      fprintf(stderr,"Output: The file %s could not be created\n",argv[3]);
      return -1;
    }
  TotalEnumeration(FileOutput, &TheWORK, &TheINP);
  fclose(FileOutput);
  return 1;
}
