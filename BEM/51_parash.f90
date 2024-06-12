subroutine sub_parash
use var
implicit none
!****************************************************************************************************************************
!variable
integer::				ip,ii,j,jj
complex(8)::			PHIsFunc ,phiIncident
real(8)::				abs_sumE
real(8)::				AVE_E,AVE_E0

!****************************************************************************************************************************
!body
!meidane kharej az mile ha ra dar ruye yek khatemoayan midahad
!--------------------------------------------------------------------------------------------------------------------------------------------------
110 format( 10000(F26.16))

do jj=0,MM
 xp_parash(jj)=x0p

 yp_parash(jj)=y0p+jj*hp
 parash(jj)=PHIsFunc(xp_parash(jj),yp_parash(jj)) 
 parash_intensity(jj)=(epsilon_0*lightvelocity/2d0)*CDABS(parash(jj))**2	   !(epsilon_0*lightvelocity/2d0)

 E_intensity(jj) =(epsilon_0*lightvelocity/2d0)*CDABS(phiIncident(xp_parash(jj),yp_parash(jj)))**2
 write(*,*)jj,MM
end do
AVE_E0=0d0
AVE_E =0d0
AVE_E =sum((parash_intensity))/(MM+1)
AVE_E0=sum(( E_intensity    ))/(MM+1)
transmission=AVE_E/AVE_E0

!sum_E=sum(parash)/(Np+1)
!transmission2=CDABS(sum_E/E0)**2
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
write(26,110)landa,transmission

write(*,*)'parash has done',landa 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%


!FIELDparash=CDAbs(parash)**2

open(unit=25,file='intensity of parash_.txt')
Do j=0,MM
 Write(25,110)parash_intensity(j)
End Do
Close(25)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-------------------------------------------------------------------------------------------------------------------------------------------------------------
end subroutine sub_parash