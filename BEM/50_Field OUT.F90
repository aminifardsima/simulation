subroutine fieldout
use var
implicit none
!****************************************************************************************************************************
!variable
integer::ip,j,knw,lp,ii,jj
complex(8)::PHIiFunc,PHIsFunc
!****************************************************************************************************************************
!body
!--------------------------------------------------------------------------------------------------------------------------------------------------
110 format( 10000(F26.16))
Do ii=0,M
 Do jj=0,N
  Do knw=1,nw
   if (test(ii,jj)==knw) FIELD(ii,jj)=PHIiFunc(knw,xpm(ii),ypn(jj)) 	 !meidane dakhele mile ha ra hesab mikonad
  end do
   if  (test(ii,jj)==0)  FIELD(ii,jj)=PHIsFunc(xpm(ii),ypn(jj)) 		 !meidane kharej az mileha ra hesab mikonad
 end do
 write(*,*)ii,M
end do 

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open(unit=19,file='Real_field.txt')
Do j=0,N
 Write(19,110)Dreal(FIELD(:,j))
End Do
Close(19)
																																	 !%%%%%%%%%%%%%%%%%%%%%%%%%%%%
open(unit=21,file='IM_field.txt')
Do j=0,N
 Write(21,110)IMag(FIELD(:,j))
End Do
Close(21)

ABS2_FIELD=CDAbs(FIELD)**2

open(unit=24,file='ABS_FIELD1_.txt')
Do j=0,N
 Write(24,110)ABS2_FIELD(:,j)
End Do
Close(24)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!-------------------------------------------------------------------------------------------------------------------------------------------------------------
end subroutine fieldout