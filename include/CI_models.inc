C-----16/09/2017
C-----Namelist for additional CI analyses 

C------------------------------------VARIABLES
      DOUBLE PRECISION CIXQfractLO(5000,2,7)
      DOUBLE PRECISION, DIMENSION(4,6) :: Eta
      integer :: i_ci


C------------------------------------COUPLINGS
      DOUBLE PRECISION CILQmass, CILQres, CILQwidth
      COMMON / CILQ / CILQmass, CILQres, CILQwidth
