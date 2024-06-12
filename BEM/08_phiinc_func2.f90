function phiIncident(xp2,yp2)
use var
implicit none
!****************************************************************************************************************************

!variables
complex(8)::					phiIncident
real(8)::						xp2,yp2,kr
real(8), parameter::			y00=0D-6,x00=-5.66d-6,b=3.8d-6
complex(8)::					phiIncident1
!****************************************************************************************************************************
 kr=kax*xp2+kay*yp2	

 phiIncident1=E0*dexp(-((yp2-y00)/b)**12)*CDExp(IMG*kr)

 phiIncident=E0*dexp(-(((yp2-y00)*Dcos(Tetainc)+(xp2-x00)*Dsin(Tetainc))/b)**12)*CDEXp(Img*kr)

!--------------------------------------------------------------------------------------------------------------------------------------------------------
end function phiIncident

