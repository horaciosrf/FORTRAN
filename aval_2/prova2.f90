!---------------CABEÇALHO-------------------------------------------------
    !Segunda avaliação da disciplina de Fortran do CPGF
    !Professor: Cícero Roberto Teixeira Régis
    !Programador aluno: Horácio Santiago Ribeiro Ferreira
    !Data: 06/07/2021 
!---------------PROBLEMA--------------------------------------------------
    !Modelagem de um levantamento gravimétrico de um cilindro.
    !Usamos a subrotina dos trapézios para calcular a integral e uma function
    !para calcular o integrando Iy. O programa irá ler os parametros e os valores 
    !de x de um arquivo chamado "modelo_cilindro.dat" e irá escrever o resultado
    !em um arquivo chamado "dados_gravi.dat".
!---------------MODULO----------------------------------------------------
    !Módulo que contem a subrotina para calcular integrais pelo método dos
    !trapézios. Também contém as funções usadas como integrando na subrotina e
    !as variáveis globais que serão utilizadas tanto na subrotina, na function
    !e no programa principal.
module integrais
    implicit none
 !--------------VARIÁVEIS-GLOBAIS------------------------------------------
    !Declaração das variáveis globais:
    integer,parameter::pr = kind(1.d0) !variável de precisão das variáveis
    real(pr)            :: R,p,L,x !variáveis globais do modeelo
    
    contains
 !--------------SUBROTINAS-------------------------------------------------
    subroutine trapezios(FUN,a,b,S )
    !Subrotina que calcula integral a partir do método dos trapézios
        implicit none
        real(pr),external       :: FUN !Integrando, definido como uma function
        real(pr),intent(in)     :: a,b !Limites da integral
        real(pr),intent(out)    ::  S  !Valor da soma/integral
        !Variáveis locais:
        real(pr)                :: soma,sex,h,x !Variáveis utilizadas nas somas
        real(pr)                :: S1,S2 !Variáveis das somas da iteração anterior e atual
        real(pr)                :: eps !Variável de precisão do critério de convergência
        integer                 :: N !variável do número de iterações
        integer                 :: i !Variáveis inteiras para auxiliar nos laços
        
        eps = 1.d-6 !Declarar a precisão do critério de convergência
        N = 4 !Número de iterações inicial
        !Cálculo da primeira soma:
        h = (b-a)/N
        sex = FUN(a) + FUN(b)
        soma = 0.
        x = a+h
        do i = 1, N-1
            soma = soma + FUN(x)
            x = x + h
        end do
        S1 = h * (sex + 2*soma)/2.d0
        
        !Cálculo das somas seguintes, com critério de convergência, aumentando
        !o número de iterações N até o ponto em que atinge a precisão eps,
        !definindo a soma final S:
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
        end do
        
    
    end subroutine trapezios

 !---------------FUNCTIONS------------------------------------------------
    real(pr) function Iy(xl) 
        !Essa função vai calcular o valor do Iy, a partir dos parametros globais
        !R,p,L e do x definido a cada coordenada.
        implicit none
        real(pr),intent(in)  ::xl
        real(pr)             ::numerador,denominador
        numerador = ((p+L)**2 +(x-xl)**2)*(sqrt(R**2 - xl**2)+sqrt(p**2 + R**2 + x**2 - 2*xl*x))**2
        denominador = (p**2+(x-xl)**2)*(sqrt(R**2-xl**2)+sqrt((p+L)**2+R**2+ x**2-2*xl*x))**2
        Iy = log(numerador/denominador)
    end function Iy

end module integrais

!---------------PROGRAMA-PRINCIPAL---------------------------------------
    !Programa principal onde vamos definir os arquivos de entrada e saída,
    !o valor das variáveis que serão utilizadas e chamar a subrotina para 
    !calcular a integral
program gravi
    use integrais
    implicit none
 !---------------VARIÁVEIS-------------------------------------------------
    !Declaração de variáveis do programa:
    integer  :: N  !Integer com numero de valores de x
    real(pr) :: S  !Real com o valor final da integral no ponto x
    integer  :: i  !Integer para auxiliar no laço do
    real(pr) :: a,b !Limites de integração utilizado na subrotina trapezios

 !---------------ARQUIVOS--------------------------------------------------
    !Abertura de arquivos tanto de entrada (Read) quanto de saída (Write)
    open(1,file='modelo_cilindro.dat',status='old',action='read')
    open(2,file='dados_gravi.dat',status='replace',action='write') 
 !---------------PROGRAMA--------------------------------------------------
    read(1,*)R,p,L !Parametros globais de raio, densidade e comprimento, respectivamente
    read(1,*)N     !Número de valores do vetor coordenado x
    a = -R
    b = R
    !Laço para calcular cada valor de S em cada coordenada x, utilizando Iy:
    do i = 1, N
        read(1,*)x !Primeiro lemos a coordenada x do arquivo de entrada
        call trapezios(Iy,a,b,S)  !A subrotina calcula o valor da integral
        write(2,*)x,S !Escrevemos no arquivo de saída x e S
    end do

end program gravi