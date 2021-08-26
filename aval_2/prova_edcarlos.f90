!Aluno: Edcarlos Vilhena Carvalho
!Professor: Cícero Régis
!Data: 06/07/2021
!Problema:

MODULE integrais
    !Módulo para calcular integrais usando trapézios
    IMPLICIT NONE
    !Variáveis globais - utilizadas nas funções, subrotinas e program principal:
    INTEGER,PARAMETER::dp = kind(1.d0) !variável de precisão das variáveis
    REAL(dp)            :: R,p,L,x !variáveis do modelo               !
    
    CONTAINS
    
    SUBROUTINE trapezios(FUN,a,b,S )
     !Subrotina para calculo da integral 
        IMPLICIT NONE
        REAL(dp),EXTERNAL       :: FUN !Função integrando (entrada)
        REAL(dp),INTENT(in)     :: a,b !Limites a e b da integral (entrada)
        REAL(dp),INTENT(out)    ::  S  !Valor final da soma (saída)
        !Variáveis locais:
        REAL(dp)                :: soma,sex,h,x 
        INTEGER                 :: i,j
        REAL(dp)                :: S1,S2 
        REAL(dp)                :: eps !Precisão do critério de convergência
        INTEGER                 :: N !Número de iterações
        
        eps = 1.d-6 !Precisão do critério de convergência
        N = 4 !Iterações inicial
        !Vamos calcular a primeira soma, a partir do N inicial:
        h = (b-a)/N
        sex = FUN(a) + FUN(b)
        soma = 0.
        x = a+h
        do i = 1, N-1
            soma = soma + FUN(x)
            x = x + h
        end do
        S1 = h * (sex + 2*soma)/2.d0
        
        !Vamos calcular as somas seguintes até atingir a precisão do 
        !critério de convergência
        j = 0 !calcular iterações
        do 
            x = a - h/2.d0
            do i = 1, N
                x = x + h
                soma = soma + FUN(x)
            end do
            h = h / 2.d0
            S2 = h * (sex + 2*soma )/2.d0
            S = (1.d0/3.d0) * (4*S2 - S1)
            if (abs(S-S1) > eps) then
                N = 2 * N
                S1 = S
            else
                exit
            end if
            j = j + 1
        end do
    END SUBROUTINE trapezios

    REAL(dp) FUNCTION integrando(x1) 
        implicit none
        REAL(dp),INTENT(in)  ::x1
        REAL(dp)             ::n,d
        n = ((p+L)**2 +(x-x1)**2)*(sqrt(R**2 - x1**2)+sqrt(p**2 + R**2 + x**2 - 2*x1*x))**2
        d = (p**2+(x-x1)**2)*(sqrt(R**2-x1**2)+sqrt((p+L)**2+R**2+ x**2-2*x1*x))**2
        integrando = log(n/d)
    
    END FUNCTION integrando

end module integrais


program prova_edcarlos
    USE integrais
    IMPLICIT NONE
    !Programa principal 
    !Aqui vamos abrir os arquivos de entrada e saída, ler os parametros
    !e chamar a subrotina para calcular a integral

    !Declaração de variáveis:
    REAL(dp) :: soma  !soma representando o valor da integral
    INTEGER  :: k  !Numero de valores de x
    INTEGER  :: i  !variável para auxiliar o laço 

    OPEN(10,file='dados_input.dat',status='old',action='read')
    OPEN(100,file='dados_output.dat',status='replace',action='write') 
    !Leitura dos parametros R,p,L
    READ(10,*)R,p,L 
    !Leitura do valor de coordenadas de x:
    READ(10,*)k     !Número de valores do vetor coordenado x

    DO i = 1, k
        !Leitura do valor de x para a posição i:
        READ(10,*)x 
        !Chamamos a subrotina, podemos notar que nesse caso o integrando vai ser
        !a função integrando, a=-R, b=R e o resultado é a soma
        CALL trapezios(integrando,-R,R,soma)  
        !Por fim, escrevemos a coordenada x e o resultado soma no arquivo de saída:
        WRITE(100,*)x,soma !Escrevemos no arquivo de saída x e S
    END DO



end program prova_edcarlos