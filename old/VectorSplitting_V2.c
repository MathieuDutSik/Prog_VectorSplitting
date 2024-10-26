#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct {
  int TheDim;
  int TheModulo;
  long long OrbitSize;
  int nbGen;
  int ***TheGroupGenerators;
  int *Vect1;
  int *Vect2;
} TheInput;


typedef struct {
  long long NBtotal;
  long long NBneeded;
  long long h;
  long long *TheVector;
  int *TheSingVect;
} TwoVector;

void OrbSpl_Assignments(TheInput *TheINP, FILE *f)
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
  if ((TheINP->Vect1 = (int*)malloc(TheDim*sizeof(int))) == 0)
    exit (EXIT_FAILURE);
  if ((TheINP->Vect2 = (int*)malloc(TheDim*sizeof(int))) == 0)
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
      ThePow=Pow2;
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
		       TheInput *TheINP, int iGen)
{
  int TheSum, i, j;
  long long h;
  h=*TheIn;
  ConvertLongLong2Vector(TheINP, &h, &(TheINP->Vect1));
  for (i=0; i<TheINP->TheDim; i++)
    {
      TheSum=0;
      for (j=0; j<TheINP->TheDim; j++)
	TheSum=TheSum+TheINP->Vect1[j]*TheINP->TheGroupGenerators[iGen][j][i];
      TheINP->Vect2[i]=TheSum % TheINP->TheModulo;
    }
  ConvertVector2LongLong(TheINP, &(TheINP->Vect2), TheOutput);
}


void EFCR_Vector2Creation(TheInput *TheINP, TwoVector *EFRC)
{
  long long TheNBtotal;
  long long NBneeded;
  int i, TheMod, h;
  TheNBtotal=1;
  for (i=0; i<TheINP->TheDim; i++)
    TheNBtotal=TheNBtotal*TheINP->TheModulo;
  h=31;
  TheMod=TheNBtotal % h;
  NBneeded=1 + (TheNBtotal - TheMod)/h;
  EFRC->NBtotal=TheNBtotal;
  EFRC->NBneeded=NBneeded;
  EFRC->h=h;
  if ((EFRC->TheVector = (long long*)malloc(NBneeded*sizeof(long long))) == 0)
    exit (EXIT_FAILURE);
  for (i=0; i<NBneeded; i++)
    EFRC->TheVector[i]=0;
  if ((EFRC->TheSingVect = (int*)malloc(h*sizeof(int))) == 0)
    exit (EXIT_FAILURE);
}

int EFRC_GetValue(TwoVector *EFRC, long long idx)
{
  int h;
  long long ThePos, TheVal;
  long long ThePow, ThePow2, res;
  int TheMod;
  int delta;
  int iPos;
  h=EFRC->h;
  TheMod=idx%h;
  ThePos=(idx-TheMod)/h;
  TheVal=EFRC->TheVector[ThePos];
  ThePow=1;
  for (iPos=0; iPos<h; iPos++)
    {
      ThePow2=ThePow*4;
      res=TheVal % ThePow2;
      delta=res/ThePow;
      TheVal=TheVal-res;
      EFRC->TheSingVect[iPos]=delta;
      ThePow=ThePow2;
    }
  return EFRC->TheSingVect[TheMod];
}
void EFRC_SetValue(TwoVector *EFRC, long long idx, int eVal)
{
  int h;
  long long ThePos, TheVal, res;
  long long ThePow, ThePow2, delta;
  int TheMod, iPos;
  h=EFRC->h;
  TheMod=idx%h;
  ThePos=(idx-TheMod)/h;
  TheVal=EFRC->TheVector[ThePos];
  ThePow=1;
  for (iPos=0; iPos<h; iPos++)
    {
      ThePow2=ThePow*4;
      res=TheVal % ThePow2;
      delta=res/ThePow;
      TheVal=TheVal-res;
      EFRC->TheSingVect[iPos]=delta;
      ThePow=ThePow2;
    }
  EFRC->TheSingVect[TheMod]=eVal;
  ThePow=1;
  TheVal=0;
  for (iPos=0; iPos<h; iPos++)
    {
      TheVal=TheVal+EFRC->TheSingVect[iPos]*ThePow;
      ThePow=ThePow*4;
    }
  EFRC->TheVector[ThePos]=TheVal;
}



void RandomCompletion(TheInput *TheINP, TwoVector *EFRC, long long idxstart)
{
  long long fValReturn;
  long long runningPos, idxFound;
  int eVal, iGen;
  runningPos=idxstart;
  while(1)
    {
      idxFound=-1;
      for (iGen=0; iGen<TheINP->nbGen; iGen++)
	{
	  TheMultiplication(&runningPos, &fValReturn, 
			    TheINP, iGen);
	  eVal=EFRC_GetValue(EFRC, fValReturn);
	  if (eVal == 0)
	    {
	      idxFound=fValReturn;
	      EFRC_SetValue(EFRC, fValReturn, 1);
	      TheINP->OrbitSize=TheINP->OrbitSize+1;
	    }
	}
      EFRC_SetValue(EFRC, runningPos, 2);
      if (idxFound == -1)
	break;
      else
	runningPos=idxFound;
    }
}

void SingleEnumeration(TheInput *TheINP, TwoVector *EFRC, long long idx)
{
  int IsFinished, eVal;
  long long i;
  EFRC_SetValue(EFRC, idx, 1);
  TheINP->OrbitSize=1;
  RandomCompletion(TheINP, EFRC, idx);
  while(1)
    {
      IsFinished=1;
      for (i=0; i<EFRC->NBtotal; i++)
	{
	  eVal=EFRC_GetValue(EFRC, i);
	  if (eVal == 1)
	    {
	      IsFinished=0;
	      RandomCompletion(TheINP, EFRC, i);
	    }
	}
      if (IsFinished == 1)
	break;
    }
}


void TotalEnumeration(FILE *TheOut, TheInput *TheINP)
{
  int IsFirstPrint, i, eVal;
  TwoVector *EFRC;
  long long iPos;
  EFRC=(TwoVector*)malloc(sizeof(TwoVector));
  EFCR_Vector2Creation(TheINP, EFRC);
  fprintf(TheOut, "return [");
  IsFirstPrint=1;
  for (iPos=0; iPos<EFRC->NBtotal; iPos++)
    {
      eVal=EFRC_GetValue(EFRC, iPos);
      if (eVal == 0)
	{
	  SingleEnumeration(TheINP, EFRC, iPos);
	  if (IsFirstPrint == 1)
	    IsFirstPrint=0;
	  else
	    fprintf(TheOut, ",");
	  ConvertLongLong2Vector(TheINP, &iPos, &(TheINP->Vect1));
	  fprintf(TheOut, "rec(eVect:=[");
	  for (i=0; i<TheINP->TheDim; i++)
	    {
	      if (i > 0)
		fprintf(TheOut, ",");
	      fprintf(TheOut, "%d", TheINP->Vect1[i]);
	    }
	  fprintf(TheOut, "], OrbitSize:=");
	  fprintf(TheOut, "%lld)", TheINP->OrbitSize);
	  fprintf(stderr, "Find an orbit of size %lld \n", TheINP->OrbitSize);
	}
    }
  fprintf(TheOut, "];\n");
}



int main(int argc, char *argv[])
{
  FILE *FileInput=NULL;
  FILE *FileOutput=NULL;
  TheInput TheINP;
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
  OrbSpl_Assignments(&TheINP, FileInput);
  fclose(FileInput);

  fprintf(stderr, "Creating output file\n");
  FileOutput=fopen(argv[2], "w");
  if (FileOutput == NULL)
    {
      fprintf(stderr,"Output: The file %s could not be created\n",argv[3]);
      return -1;
    }
  TotalEnumeration(FileOutput, &TheINP);
  fclose(FileOutput);
  return 1;
}
