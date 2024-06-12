program MainProgram
 use var
 implicit none 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
!f0r silicon carbide Gold iron
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!****************************************************************************************************************************************  
!variable
integer::j
!****************************************************************************************************************************************
!body of program
!-------------------------------------------------------------------------------------------------------------------------------------------------
call start_sub
  
call shabake


call SUB_TEST

open(unit=26,file='transmission.txt')

do  j=0,NL
allocate(X_MAT(1:2*NT))

 landa=landa0+j*dlanda

 call pre_sub

 call sumMAT

 !call fieldout
  
 call sub_parash
deallocate(X_MAT)
end do

close(26)
	 
deallocate(xp,yp,tp,tk,xk,yk,nxh,nyh,arrayphiinc)


 
 !deallocate(qil,pil)
  !*****
!---------------------------------------------------------------------------------------------------------------------------------------------
end program MainProgram
  