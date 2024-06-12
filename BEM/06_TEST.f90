subroutine SUB_TEST
use var
implicit none
!****************************************************************************************************************************
!variable
integer::i,j,knw
real(8)::r

!****************************************************************************************************************************
!body
!------------------------------------------------------------------------------------------------------------------------------------------------
110 format( 10000(F26.16))
do i=0,M
 xpm(i)=x0+i*h
End Do
do i=0,N
 ypn(i)=y0+i*h
End Do
test=0
120 format( 10000(I10))
Do i=0,M
 Do j=0,N
  do knw=1,nw
   r=dsqrt(((xpm(i)-xci(knw))**2)+((ypn(j)-yci(knw))**2))									!r=radios of examination that the dot is in the "I" area or not 
   if  (r.lt.ai(knw).and.ypn(j).gt.yoi(knw)) then
   test(i,j)=knw																		    !xci=markaze mileye iom	  !yci=markaze mileye jom
   end if
  end do
 end do
!write(*,*)i,N
end do
open(unit=20,file='TEST.TXT')
Do j=0,N
 Write(20,120)test(:,j)
End Do
Close(20)
 !---------------------------------------------------------------------------------------------------------------------------------------------
end subroutine SUB_TEST