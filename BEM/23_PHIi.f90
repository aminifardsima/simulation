function PHIiFunc(ip,xp2,yp2)
use IMSL 
use var
implicit none
!****************************************************************************************************************************
!variable
 integer::i,l,ip,ieq,k
 real(8)::xp2,yp2
 complex(8)::PHIiFunc,Sfun,Rfun,IFUNC 
  integer, parameter::NB=2
complex(8)::CBS1(NB),CBS2(NB),zz2,value,value3,value4
real(8)::xnu,RR,dl,dRdn
complex(8)::dGidn,dGidR,Gi

EXTERNAL CBYS,CBJS


 !****************************************************************************************************************************
!body
!meidane dakhele mileha
!---------------------------------------------------------------------------------------------------------------------------------------------
							
ieq=0
xnu=0d0 
value=(0D0,0D0)
do i=1,nw-1						    
 do l=1,Mi(i)   
  ieq=ieq+1
 end do
end do	
i=ip
if(i==ip) then					    
do l=1,Mi(i)
 ieq=ieq+1
   if (l<=netetai(i)) then
    dl=dlarray(i,1)
   else
    dl=dlarray(i,2) 
   end if
	
  value3=(0D0,0D0)
  value4=(0D0,0D0)
  do k=0,(2*nd-1)

   RR=dsqrt(((xk(i,l,k)-xp2)**2)+((yk(i,l,k)-yp2)**2))
   zz2=RR*kvecti(i)

   Gi=(0d0,0d0) !!!!!!!!!!!
   dGidR=(0d0,0d0) !!!!!!!!!
  if(ABS(RR).le.had) then !!!!!!!!!!!!!!
    call DCBJS(xnu,zz2,NB,CBS1)
    call DCBYS(xnu,zz2,NB,CBS2)

    Gi=(Img/4d0)*(CBS1(1)+Img*CBS2(1))
    dGidR=(-Img/4d0)*kvecti(i)*(+CBS1(2)+Img*CBS2(2))
  end if    !!!!!!!!

    dRdn=(((Xk(i,l,k)-xp2)*nxh(i,l,k))+((yk(i,l,k)-yp2)*nyh(i,l,k)))/RR
      
    dGidn=dGidR*dRdn
   
   value3=value3+(-dGidn)*ww(k)*dl 
   value4=value4+(Gi/zarib(i))*ww(k)*dl 

  end do

   Rfun=value3
   sfun=value4

  value=X_MAT(ieq)*Rfun+X_MaT(Ieq+NT)*Sfun+value
  
end do

PHIiFunc=value +Ifunc(ip,xp2,yp2) 
end if
!-----------------------------------------------------------------------------------------------------------------------------------------------
end function PHIiFunc
