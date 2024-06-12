subroutine pre_sub
use var
implicit none
!****************************************************************************************************************************
!variables
integer::i
!****************************************************************************************************************************
!body
!-----------------------------------------------------------------------------------------------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%
!neveshtane bordare moj
!%%%%%%%%%%%%%%%%%%%%%%%%
 omega=2d0*pi*lightvelocity/landa
 kvecta=(refractive_a*2D0*Pi)/landa
 if	(TETAinc==Pi/2d0)then
  kax=0D0
  kay=kvecta
 else if(TETAinc==-Pi/2d0)then
   kax=0D0
   kay=-kvecta 
 else if(TETAinc==0D0)then
  kay=0D0
  kax=kvecta
 else if(TETAinc==pi)then
   kay=0D0
   kax=-kvecta
 else
  kax=kvecta*DCos(TETAinc)
  kay=kvecta*DSin(TETAinc)
 end if


 do i=1,nw
  kvecti(i)=(refractive_i(i)+Img*Alfa_i(i))*2D0*Pi/Landa

  dielectric_cons_i(i)=1d0-(omega_p_i(i)**2/(omega*(omega+IMG*gama_i(i))))								!k=epsilon/epsilon_0

  kvecti(i)=(cdsqrt(dielectric_cons_i(i)))*2D0*Pi/Landa			                                    !kvecti= bordare moj

  !!zarib(i)=1d0/dielectric_cons_i(i)	
   zarib(i)=1d0
   kr_mad=dreal( dielectric_cons_i(i))
   ki_mad=dimag( dielectric_cons_i(i))
   Alfa_i(i)=dsqrt(0.5d0*(-kr_mad+dsqrt(kr_mad**2+ki_mad**2)))
		
 end do
!%%%%%%%%%%%%%%%%%%%%%
!omghe nofuz e tahlili

do i=1,nw
 deltai(i)=landa/(Alfa_i(i)*2d0*pi)
 puste=deltai(i)
end do
had=650d0*puste
!av_nofuz=sum(deltai)/nw

!open(unit=22,file='omghe nofuze har mile tahlili.txt')
!write(22,*)deltai
!close(22)

!%%%%%%%%%%%%%%%%%%%%%
!--------------------------------------------------------------------------------------------------------------------------------------------
end subroutine pre_sub