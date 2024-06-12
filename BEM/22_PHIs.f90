function PHIsFunc(xp2,yp2)
use IMSL 
use var
implicit none
!****************************************************************************************************************************

!variable
 integer::		i,l,ieq,k
 real(8)::		xp2,yp2
 complex(8)::	PHIsFunc  
 !complex(8)::	kr
 complex(8)::	Qfun,Pfun ,PHIinc 
 integer, parameter::NB=2
complex(8)::value,value1,value2
real(8)::xnu,RR,dl,dRdn
complex(8)::dGadn,dGadR,Ga,phiIncident
real(8)::zz1,BSJ1,BSY1,BSJ0,BSY0


EXTERNAL CBYS,CBJS
!****************************************************************************************************************************
!body			
!meidane khareje mileha ast
!---------------------------------------------------------------------------------------------------------------------------------------------														
value=(0D0,0D0);ieq=0;xnu=0d0												
 do i=1,nw						    
  do l=1,Mi(i)   
   ieq=ieq+1

    if (l<=netetai(i)) then
     dl=dlarray(i,1)
    else
     dl=dlarray(i,2) 
    end if

    value1=(0D0,0D0)
    value2=(0D0,0D0)

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

   end do
   Pfun=value1 
   Qfun=value2

    value=x_Mat(ieq)*Pfun+x_MAT(Ieq+NT)*Qfun+value
  	!value=qil(i,l)*Qfunc(i,l,xp2,yp2)+pil(i,l)*Pfunc(i,l,xp2,yp2)+value
  end do
 end do
!kr=kax*xp2+kay*yp2																						  !Kvecta.r
PHIinc=phiIncident(xp2,yp2)

																										  !moje forudi
PHIsFunc=value + PHIinc
				!zamani ke PHIinc nabashad faghat meidane escater shode ra midahad
!--------------------------------------------------------------------------------------------------------------------------------------------------------
end function PHIsFunc
