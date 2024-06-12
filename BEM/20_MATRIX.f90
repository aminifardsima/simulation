subroutine sumMAT
use var
use IMSL 
implicit none
!****************************************************************************************************************************
!variables
complex(8)::Ifunc,phiIncident !,PHIinc
integer::ip,lp,i,l,j,k
integer::Ieq,Ixv
complex(8)::kr
complex(8),Allocatable::B_MAT(:),CM(:,:)
integer, parameter::NB=2
real(8)::zz1,BSJ1,BSY1,BSJ0,BSY0
complex(8)::CBS1(NB),CBS2(NB),zz2,value,value1,value2,value3,value4,pfun,qfun,sfun,Rfun
real(8)::xnu,RR,dl,dRdn,xp2,yp2
complex(8)::dGadn,dGadR,Ga,dGidn,dGidR,Gi


EXTERNAL CBYS,CBJS

!Complex(8)::b
!****************************************************************************************************************************
!parralel
!body
!--------------------------------------------------------------------------------------------------------------------------------------------
allocate(CM(1:2*NT,1:2*NT))
allocate(B_MAT(1:2*NT))
write(*,*)'start boundary'

xnu=0d0
value=(0D0,0D0)
 
X_MAT=(0D0,0D0)
CM=(0D0,0D0)
B_MAT=(0D0,0D0)
Ieq=0
do ip=1,nw											 !mileye ist ke noghte moshahede dar an gharar gerefte ast entekhab mishavad (gere mitavanad ruye tamamiye mile ha beravad)
 do lp=1,Mi(ip)											  !onsori ra entekhab mikonad ke noghte moshahede(gere) dar an gharar migirad
  Ieq=Ieq+1											   !be shomare radif dar matrix 1 ezafe mikonad
  write(*,*)'C_MAT',NT,Ieq
   Ixv=0
  xp2=xp(ip,lp)
  yp2=yp(ip,lp)
											    !shomare sotun ==0
  do i=1,nw											     !ruye tamamiye mile ha antegral begir (entekhabe mileye i ke ruyash antegral gerefte mishavad)
   do l=1,Mi(i)
    if (l<=netetai(i)) then
     dl=dlarray(i,1)
    else
     dl=dlarray(i,2) 
    end if

  value1=(0D0,0D0)
  value2=(0D0,0D0)
  value3=(0D0,0D0)
  value4=(0D0,0D0)

	do k=0,(2*nd-1)

     RR=dsqrt(((xk(i,l,k)-xp2)**2)+((yk(i,l,k)-yp2)**2))
     zz1=RR*kvecta

	 Ga=(Img/4d0)*(DBSJ0(zz1)+Img*DBSY0(zz1))
	 dGadR=(-Img/4d0)*kvecta*(+DBSJ1(zz1)+Img*DBSY1(zz1))
	 								  												 !DBSJ1=besselj1 ; DBSY1=bessely1
											  
     dRdn=(((Xk(i,l,k)-xp2)*nxh(i,l,k))+((yk(i,l,k)-yp2)*nyh(i,l,k)))/RR
	 dGadn=dGadR*dRdn
     value2=value2+(-Ga)*ww(k)*dl 
     value1=value1+dGadn*ww(k)*dl 
	 
	 if(i==ip) then
      zz2=RR*kvecti(i)
      Gi=(0d0,0d0)     !!!!!!!!!
	  dGidR=(0d0,0d0)  !!!!!!!!!
	  if(ABS(RR).le.had) then   !!!!!!!!!
	   call DCBJS(xnu,zz2,NB,CBS1)
	   call DCBYS(xnu,zz2,NB,CBS2)

	   Gi=(Img/4d0)*(CBS1(1)+Img*CBS2(1))
       dGidR=(-Img/4d0)*kvecti(i)*(+CBS1(2)+Img*CBS2(2))
   	  end if !!!!!!!!!!!  
       dGidn=dGidR*dRdn
	  
   	  value3=value3+(-dGidn)*ww(k)*dl 
      value4=value4+(Gi/zarib(i))*ww(k)*dl 
 	 end if  
 
   end do
   Pfun=value1 
   Qfun=value2
   Rfun=value3
   sfun=value4

											   !ruye tamamiye onsor ha ruye har mile antegral begir
   Ixv=Ixv+1 											  !ba antegral giri ruye har onsor be shomare sotun yeki ezafe kon
   CM(Ieq,Ixv   )=Pfun 	!
   CM(Ieq,Ixv+NT)=Qfun	!in 3 ta ruye moadele ei hastand ke baraye zamine ast(BIE1)
   B_MAT(Ieq)=0d0									!

   if(i==ip) then										  !be moadeleye BIE2 boro
     CM(Ieq+NT,Ixv   )=Rfun	!in 2 ta ruye moadele ei hastand ke baraye mile ha ast(baraye tak tak e mile ha neveshte mishavad)
     CM(Ieq+NT,Ixv+NT)=Sfun	!
	
   !if(lp==l)  CM(Ieq,Ieq    )=CM(Ieq   ,Ieq)-0.5D0		
   !if(l==lp)  CM(Ieq+NT,Ieq )=CM(Ieq+NT,Ieq)-0.5D0			
   end if
   end do !l
  end do !i
   !kr=kax*Xp (ip,lp)+kay*Yp (ip,lp)																							!Kvect.r_prim
   !PHIinc=E0*CDExp(IMG*kr)										 !moje forudi dar mahale noghte moshahede
    B_MAT(Ieq+NT)=0.5d0*phiIncident(Xp (ip,lp),Yp (ip,lp))!
	arrayphiinc(ip,lp)= phiIncident(Xp (ip,lp),Yp (ip,lp))
   B_MAT(Ieq+NT)=0.5d0*phiIncident(Xp (ip,lp),Yp (ip,lp))-Ifunc(ip,xp(ip,lp),yp(ip,lp))						 !(BIE2)

 end do	!lp
end do !ip

Ieq=0
do ip=1,nw
 do lp=1,Mi(ip)
  
  Ieq=Ieq+1
  CM(Ieq   ,Ieq)=CM(Ieq   ,Ieq)-0.5D0
  CM(Ieq+NT,Ieq)=CM(Ieq+NT,Ieq)-0.5D0							 !kam kardan az ghotre asli

 End Do
End Do


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
									  
110 format( 10000(F26.16))
open(unit=33,file='CMMatrix.txt')
!open(unit=14,file='imagCMMatrix.txt')
do j=1,2*NT
 write(33,*) DIMAG(CM(j,1:2*NT))   !,DREAL(CM(j,1:2*NT))
end do
close(33)
!close(14)


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


CALL DLSACG (2*NT, CM, 2*NT, B_MAT, 1, X_MAT)	
 																														    !dastur baraye hale matrix ::AX=B(dimension of A,A,dimension of B,B,linier=1 or not,X)

!Ieq=0
!do ip=1,nw
! do lp=1,Mi(ip)
!  Ieq=Ieq+1								  !azmayesh baraye meidane electriky
!  kr=kax*Xp(ip,lp)+kay*Yp(ip,lp)																							!Kvect.r_prim
!  PHIinc=E0*CDExp(IMG*kr)
!  X_MAT(Ieq)=PHIinc+X_MAT(Ieq)
! end do	!lp
!end do !ip


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!open(unit=15,file='BOTH_B_MAT.txt')
!open(unit=16,file='IM_B_MATrix.txt')
open(unit=15,file='PHIinc.txt')
do ip=1,nw									
 do lp=1,Mi(ip)
 write(15,110) Real(arrayphiinc(ip,lp))
 !write(15,*) DIMAG(B_MAT(j))!!, DREAL(B_MAT(j))
 !write(16,110)DIMAG(B_MAT(j))
 end do
end do
close(15)
!close(16)



!%%%%%%%%%%%%%%%%%%%%%%%%
open(unit=17,file='Both_X_MAT.txt')                              
!open(unit=18,file='IM_X_MAT.txt')
!open(unit=23,file='BOTH_X_MAT.txt')
do j=1,2*NT
 write(17,*)DREAL(X_MAT(j))      !,DIMAG(X_MAT(j))
 !write(18,110)DIMAG(X_MAT(j))
 !write(23,*)DREAL(X_MAT(j)),DIMAG(X_MAT(j))
end do
close(17)
!close(18)
!close(23)
write(*,*)'end boundary'

deallocate(CM,B_MAT)
!-------------------------------------------------------------------------------------------------------------------------------------------
end subroutine sumMAT


