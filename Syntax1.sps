GET 
  FILE='C:\Users\elm26\Documents\SERDPproject\MarjoQuestions\SPSS BothSitesAllYears Data File.sav'. 
DATASET NAME DataSet1 WINDOW=FRONT. 
SORT CASES  BY Desert. 
SPLIT FILE SEPARATE BY Desert. 
USE ALL. 
COMPUTE filter_$=(TranDir = 'N' AND (Year = 2012 OR Year = 2013)). 
VARIABLE LABELS filter_$ "TranDir = 'N' AND (Year = 2012 OR Year = 2013) (FILTER)". 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE.


DATASET ACTIVATE DataSet1.

*This is command that follows from usign menu bars to specify.  
*Linear Mixed Mode, CLICK CONTINUE ON FIRST SCREEN.  On next screen, pick fixed and random effects

MIXED EstBiomass BY ShrubID TranSide Plot MH Year FireTrt RainTrt SeedTrt TurbTrt WITH WintRain
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=TranSide MH Year FireTrt RainTrt SeedTrt TurbTrt WintRain | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM=ShrubID ShrubID*TranSide ShrubID*TranSide*Plot | COVTYPE(VC)
  /EMMEANS=TABLES(FireTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(RainTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(SeedTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(TurbTrt) COMPARE ADJ(LSD).

*Francoise says the following notation (/RANDOM intercept|subject(....) is computationally more efficient than the /RANDOM statemetn above. 
*Insufficient memory to run 4-level model.  So for now, just try Shrub, Transect(Shrub)

MIXED EstBiomass BY ShrubID TranSide Plot MH Year FireTrt RainTrt SeedTrt TurbTrt WITH WintRain
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=TranSide MH Year FireTrt RainTrt SeedTrt TurbTrt WintRain | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM intercept|subject(ShrubID)
/Random intercept|subject( ShrubID*TranSide)  
  /EMMEANS=TABLES(FireTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(RainTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(SeedTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(TurbTrt) COMPARE ADJ(LSD).

*Note from above model, in Mojave the covariance for ShrubID*Transide is very small, so we can just use plot instead of transect
*This is akin to saying there are 8 plots, in 4 MH x 2 sides crossed, rather than 4 plots nested within 2 transects/side

MIXED EstBiomass BY ShrubID TranSide Plot MH Year FireTrt RainTrt SeedTrt TurbTrt WITH WintRain
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=TranSide MH Year FireTrt RainTrt SeedTrt TurbTrt WintRain | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM intercept|subject(ShrubID)
/Random intercept |subject( Plot)
  /EMMEANS=TABLES(FireTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(RainTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(SeedTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(TurbTrt) COMPARE ADJ(LSD).


MIXED EstBiomass BY ShrubID TranSide Plot MH Year FireTrt RainTrt SeedTrt TurbTrt WITH WintRain
  /CRITERIA=CIN(95) MXITER(100) MXSTEP(10) SCORING(1) SINGULAR(0.000000000001) HCONVERGE(0, 
    ABSOLUTE) LCONVERGE(0, ABSOLUTE) PCONVERGE(0.000001, ABSOLUTE)
  /FIXED=TranSide TurbTrt FireTrt RainTrt Year MH TranSide*TurbTrt TranSide*FireTrt TranSide*RainTrt 
    TranSide*Year TranSide*MH TurbTrt*FireTrt TurbTrt*RainTrt TurbTrt*Year TurbTrt*MH FireTrt*RainTrt 
    FireTrt*Year FireTrt*MH RainTrt*Year RainTrt*MH Year*MH TranSide*TurbTrt*FireTrt 
    TranSide*TurbTrt*RainTrt TranSide*TurbTrt*Year TranSide*TurbTrt*MH TranSide*FireTrt*RainTrt 
    TranSide*FireTrt*Year TranSide*FireTrt*MH TranSide*RainTrt*Year TranSide*RainTrt*MH TranSide*Year*MH 
    TurbTrt*FireTrt*RainTrt TurbTrt*FireTrt*Year TurbTrt*FireTrt*MH TurbTrt*RainTrt*Year 
    TurbTrt*RainTrt*MH TurbTrt*Year*MH FireTrt*RainTrt*Year FireTrt*RainTrt*MH FireTrt*Year*MH 
    RainTrt*Year*MH TranSide*TurbTrt*FireTrt*RainTrt TranSide*TurbTrt*FireTrt*Year 
    TranSide*TurbTrt*FireTrt*MH TranSide*TurbTrt*RainTrt*Year TranSide*TurbTrt*RainTrt*MH 
    TranSide*TurbTrt*Year*MH TranSide*FireTrt*RainTrt*Year TranSide*FireTrt*RainTrt*MH 
    TranSide*FireTrt*Year*MH TranSide*RainTrt*Year*MH TurbTrt*FireTrt*RainTrt*Year 
    TurbTrt*FireTrt*RainTrt*MH TurbTrt*FireTrt*Year*MH TurbTrt*RainTrt*Year*MH FireTrt*RainTrt*Year*MH 
    TranSide*TurbTrt*FireTrt*RainTrt*Year TranSide*TurbTrt*FireTrt*RainTrt*MH 
    TranSide*TurbTrt*FireTrt*Year*MH TranSide*TurbTrt*RainTrt*Year*MH TranSide*FireTrt*RainTrt*Year*MH 
    TurbTrt*FireTrt*RainTrt*Year*MH TranSide*TurbTrt*FireTrt*RainTrt*Year*MH WintRain | SSTYPE(3)
  /METHOD=REML
  /PRINT=SOLUTION
  /RANDOM intercept|subject(ShrubID)
/Random intercept |subject( Plot)
  /EMMEANS=TABLES(FireTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(RainTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(SeedTrt) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(TurbTrt) COMPARE ADJ(LSD).

