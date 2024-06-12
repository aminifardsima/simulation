function ifunc(i,xp2,yp2)
use var
use imsl
implicit none
!****************************************************************************************************************************

!variables
complex(8)::		Ifunc,dGidR,dGidn,Gi
integer::			i,ip,l,k
real(8)::			RR,dRdn,dl,xp2,yp2
integer,parameter::	NB=2
real(8),Parameter::	xnu=0D0
complex(8)::		zz
complex(8)::		CBS1(NB),CBS2(NB)
complex(8)::		value, dPhiincident,phiIncident
complex(8)::		b

EXTERNAL CBYS,CBJS
!****************************************************************************************************************************
!dar in ghesmat ruye haman mileye moshahede antegral giri sorat migirad ===>>  i=ip
!body
!-------------------------------------------------------------------------------------------------------------------------------------------

value=(0D0,0D0)
do l=1,Mi(i)
 if (l<=netetai(i)) then
  dl=dlarray(i,1)
 else
  dl=dlarray(i,2)
 end if
 b=(0D0,0D0)
  do k=0,(2*nd-1)
   RR=dsqrt(((xk(i,l,k)-xp2)**2)+((yk(i,l,k)-yp2)**2))
   zz=RR*kvecti(i)
   Gi=(0d0,0d0) !!!!!!!!!!!!!!!!
   dGidR=(0d0,0d0) !!!!!!!!!!!!
   if(ABS(RR).le.had) then !!!!!!!!!!!!!!!!!
    call DCBJS(xnu,zz,NB,CBS1)
    call DCBYS(xnu,zz,NB,CBS2)
   
    Gi=(Img/4d0)*(CBS1(1)+Img*CBS2(1))
    dGidR=(-Img/4d0)* kvecti(i)*(+CBS1(2)+Img*CBS2(2))
	b=(+CBS1(2)+Img*CBS2(2))
   end if	!!!!!!!!!!!

    dRdn=(((Xk(i,l,k)-Xp2)*nxh(i,l,k))+((yk(i,l,k)-yp2)*nyh(i,l,k)))/RR
 		
    dGidn=dGidR*dRdn																						!PHIinc=PHI0*exp(i*k.r)
   																										!(dPHIinc/dn)=(dPHIinc/dr)*(dr/dn)	  
     !kr=kax*Xk (i,l,k)+kay*Yk (i,l,k)																		!Kvecta.r
    ! kn=kax*nxh(i,l,k)+kay*nyh(i,l,k)																		!Kvecta.n_hat
   
    !PHIinc=E0*CDExp(IMG*kr)																				!moje forudi
    !dPHIincdn=IMG*kn*E0*CDExp(IMG*kr)		
    !value=value+(IMG*Gi*(kax*nxh(i,l,k)+kay*nyh(i,l,k))-dGidn)*E0*exp(IMG*(kax*xk(i,l,k)+kay*yk(i,l,k)))*ww(k)*dl
  	 
	!   value=value+(Gi*dphiinc)-dGidn*phiinc))*ww(k)*dl

     b=((Gi/zarib(i))*dPhiincident(Xk(i,l,k),Yk(i,l,k),nxh(i,l,k),nyh(i,l,k))-dGidn)	
     b=phiIncident(Xk (i,l,k),Yk (i,l,k))*ww(k)*dl
										
   value=value+((Gi/zarib(i))*dPhiincident(Xk(i,l,k),Yk(i,l,k),nxh(i,l,k),nyh(i,l,k))-dGidn*phiIncident(Xk (i,l,k),Yk (i,l,k)))*dl*ww(k)	
  
  !  value=value+(IMG*(Gi/zarib(i))*(kax*nxh(i,l,k)+kay*nyh(i,l,k))-dGidn)*E0*CDexp(IMG*(kax*xk(i,l,k)+kay*yk(i,l,k)))*ww(k)*dl

  end do
end do

ifunc=value
!--------------------------------------------------------------------------------------------------------------------------------------------------------
end function ifunc

