function dPhiincident(xp2,yp2,nx2,ny2)
use var
implicit none
!****************************************************************************************************************************
 !moje takht
 !***************************************************************************************************************************
!variables
complex(8)::					dPhiincident,PHIinc
real(8)::						xp2,yp2,nx2,ny2,kr,kn,w,wx,wy

complex(8)::					PHIinc2,dPhiincident2

real(8), parameter::			y00=0D-6,x00=-5.66d-6,b=3.8d-6

!****************************************************************************************************************************

 kr=kax*xp2+kay*yp2
 
 
 !PHIinc2=E0*dexp(-((yp2-y00)/b)**10) *CDExp(IMG*kr)								! 
																											!Kvecta.r	 															   
 !PHIinc2=E0*dexp(-(((yp2-y00)*Dcos(Tetainc)+(xp2-x00)*Dsin(Tetainc))/b)**10)*CDEXp(Img*kr)  !
 !dPhiincident2=PHIinc2*((((-10D0*(((yp2-y00)*Dcos(Tetainc)+(xp2-x00)*Dsin(Tetainc))/b)**9)*(Dsin(Tetainc)/b))+(Img*kax))*nx2+((-10D0*(((yp2-y00)*Dcos(Tetainc)+(xp2-x00)*Dcos(Tetainc))/b)**9)+(IMg*kay))*ny2)
 w=	((yp2-y00)*Dcos(Tetainc)+(xp2-x00)*Dsin(Tetainc))/b
 PHIinc=E0*dexp(-(w)**12)*CDEXp(Img*kr)
 wx=(-12D0*(Dsin(Tetainc)/b)*(((yp2-y00)*Dcos(Tetainc)+(xp2-x00)*Dsin(Tetainc))/b)**11)
 wy=(-12D0*(Dcos(Tetainc)/b)*(((yp2-y00)*Dcos(Tetainc)+(xp2-x00)*Dsin(Tetainc))/b)**11)

 dPhiincident=PHIinc*((wx+Img*kax)*nx2+(wy+IMg*kay)*ny2)

!--------------------------------------------------------------------------------------------------------------------------------------------------------
end function dPhiincident

