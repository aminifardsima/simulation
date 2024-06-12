Subroutine	MatrixTime(time)
Use			Var
Include 'link_fnl_static.h'              !Link to Imsl  7
!Use LINRG_INT                            !Use LINRG library to inverse Matrix
!Use MRRRR_INT                            !Use MRRRR library to Multiple two Matrix

Implicit	None
Integer::	j
Real(8)::	time
Real(8)::	Qrt,IW   !Function Var


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Do j=1,N+L

    Vct_Q(j)=   Qrt(x_g(j),y_g(j),time)                         !Make Vect_Q    (N+L  *  1)
    Vct_IW(j)=  IW(x_g(j),y_g(j),time)                          !Make Vect_IW   (N    *  1)
      
End Do   !j

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

!   ********************************************************************************** 
!   * DMRRRR (NRA, NCA, A, LDA, NRB, NCB, B, LDB, NRC, NCC, C, LDC)     A*B=C        *
!   * NRA=Number of Row matrix A                  NCA=Number of Column matrix A      *
!   * LDA=Leading Dimension of input Array A      N=The size of square matrix N*N    *
!   **********************************************************************************  



Call DMRRRR(N+L,N+L,Mat_UFinv,N+L,N+L,1,Vct_Tp,N+L,N+L,1,Vct_UFinvTp,N+L)        !Make U*Finv*Tp 

Call DMRRRR(N+L,N+L,Mat_UFinv,N+L,N+L,1,Vct_Q,N+L,N+L,1,Vct_UFinvQ,N+L)          !Make U*Finv*Q 
                                     
Call DMRRRR(N+L,N+L,Mat_IGFinv,N+L,N+L,1,Vct_Tp,N+L,N+L,1,Vct_IGFinvTp,N+L)      !Make IG*Finv*Tp
                                                      
Call DMRRRR(N+L,N+L,Mat_IGFinv,N+L,N+L,1,Vct_Q,N+L,N+L,1,Vct_IGFinvQ,N+L)        !Make IG*Finv*Q 


				                                                                 !Make MatB
Vct_B=Alpha*Vct_IGFinvTp+(1D0/k_cnd)*Vct_IGFinvQ+Vct_IW-Alpha*Vct_UFinvTp-(1D0/k_cnd)*Vct_UFinvQ                                                                                                                  
                                                           

End   Subroutine	!Matrix