Subroutine	Integral_Computing
Use			Var
Implicit	None


Integer::	i,j
Real(8)::	h


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Do i=0,n_teta                                 !Make teta i

    tetai(i)=deltai*i

End Do !i    




Select case(Way)

!@@@@@@@@@@@@@@@@@@@@@@@@@@ Rectangle Method  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
Case(1)


h=2D0/n_dteta             !Make WI  
WI=1D0
WI=WI*Rad*h*(deltai/2D0)    


Do j=1,n_dteta            !Make Aj

    Aj(j)=(h*(j-0.5D0))-1D0
    
End Do !j  



!@@@@@@@@@@@@@@@@@@@@@@@@@@  Trapezius Method   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
Case(2)



h=2D0/(n_dteta-1)
WI=1D0
WI(1)=1/2D0
WI(n_dteta)=1/2D0
WI=WI*h*Rad*(deltai/2D0)    !Make WI

Do j=1,n_dteta              !Make Aj

    Aj(j)=(h*(j-1D0))-1D0
    
End Do !j  



!@@@@@@@@@@@@@@@@@@@@@@@@@@@ Gaussian Method  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
Case(3)



open (unit=1011,file=ch(nj))

Do j=1,nj
    Read(1011,*)WI(j),Aj(j)
    WI(j)=WI(j)/2D0                  !Make Weight coefficients Combined Gauss From txt file 
    WI(j+nj)=WI(j)              !All Wi(1:2n_dteta)=wi/2
    Aj(j)=(Aj(j)-1D0)/2D0            !Aj(1:n_dteat)=(Aj-1)/2
    Aj(j+nj)=Aj(j)+1D0          !Aj(n_dteta+1:2n_dteta)=(Aj+1)/2 
end do 

WI=WI*Rad*(deltai/2D0)    !Make WI

Close(1011)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@




End Select



Do i=1,n_teta             !Make Tetaij(i,j)

    Do j=1,n_dteta

        tetaij(i,j)=((1D0-Aj(j))/2D0)*tetai(i-1)+((1D0+Aj(j))/2D0)*tetai(i)
        
    End Do !j
End Do !i        




End subroutine !Integral_Computing