subroutine start_sub
use var
implicit none
!****************************************************************************************************************************
!variables
integer::i !,start
!****************************************************************************************************************************
!body
!--------------------------------------------------------------------------------------------------------------------------------------------
  !write(*,*)'please insert your data in "input information.TXT" before run this program.'
  !write(*,*)'xo-yo-ai-hi-n-alpha'
  !write(*,*)'notice hi should be less than 2*ai.'
  !write(*,*)'if you are ready please insert "1" and then push Enter!'
  !read(*,*) start 
  open(unit=10,file='input information.txt')														 !khandan az vorudiye input information
  do i=1,nw
   read(10,*)xci0(i),yci0(i),ai(i),omega_p_i(i),gama_i(i)  !refractive_i(i),Alfa_i(i)
   hi(i)=2d0*ai(i)
   
  end do
  radius=maxval(ai)
  do i=1,nw
  xci(i)=xci0(i)*dcos(teta_r)-yci0(i)*dsin(teta_r)
  yci(i)=xci0(i)*dsin(teta_r)+yci0(i)*dcos(teta_r)
  end do
  
  close(10)
    !open(unit=27,file='omega_p,gama.txt')														 !khandan az vorudiye input information
  do i=1,nw
  !read(27,*)omega_p_i(i),gama_i(i)
   !!omega_p_i(i)=omega_p_i(i)*(electron_charge)/(h_bar)
   omega_p_i(i)=2d0*pi*omega_p_i(i)
   gama_i(i)=2d0*pi*gama_i(i)
   !!gama_i(i)=1d0/tow_i(i)
   !!gama_i(i)=gama_i(i)*(electron_charge)/(h_bar)
  end do
  !close(27)


  !%%%%%%%%%%%%%%%%%%%%%%
!zarayebe vazni
!%%%%%%%%%%%%%%%%%%%%%%%
ww( 0    )=1d0/2d0
ww(2*nd-1)=1d0/2d0
ww(1:((2*nd-1)-1))=1d0
!%%%%%%%%%%%%%%%%%%%%%%%

!--------------------------------------------------------------------------------------------------------------------------------------------
end subroutine start_sub


