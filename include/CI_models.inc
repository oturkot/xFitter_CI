C-----16/09/2017
C-----Namelist for additional CI analyses 

C------------------------------------VARIABLES
      DOUBLE PRECISION CIXQfractLO(5000,2,7)
      DOUBLE PRECISION par(8)
      DOUBLE PRECISION, DIMENSION(4,6) :: Eta


C------------------------------------COUPLINGS
      DOUBLE PRECISION CILQmass, CILQres, CILQwidth

#ifdef NTOT_C
C-----------------------------------------------------------------------
C> @brief Grid with PDFs for each datapoint (THEO).
C>
C> @note Format corresponds to `XQfrac`.
C ----------------------------------------------------------------------
		DOUBLE PRECISION :: pdfsgrid(NTOT_C, 2, 7)
		COMMON/CIPDFs/ pdfsgrid
#endif
