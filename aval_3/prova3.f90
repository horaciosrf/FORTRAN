!Aluno: Edcarlos Vilhena Carvalho
!Matricula: 202101680007
!Professor: Cícero Régis
!Data: 23/07/2021
!Problema: Sondagem Magnetotelurica - Vamos ler arquivo com valores de
!frequencia e impedancia. A partir disso vamos calcular a resistividade 
!aparente e a fase a partir do módulo e do angulo de fase da impedancia
!e por fim escreveremos em um arquivo de saída.

program MT
    implicit none
    !Declaração de variáveis:
    integer,parameter      :: dp = kind(1.d0) !Define a precisão dupla
    real(dp),parameter     :: pi = 3.14159265359 !Define o valor de pi
    real(dp),parameter     :: mi = pi*4.d-7 !Permeabilidade magnetica
    real(dp),parameter     :: rad_grau = 180.d0/pi !Converter radianos em grau
    integer                :: N !Número de frequências
    real(dp)               :: f !Frequência
    complex(dp)            :: Z !Impedâncias
    real(dp)               :: Re_Z !Parte real de Z
    real(dp)               :: Im_Z !Parte imaginária de Z
    real(dp)               :: T !Período, inverso da frequência
    real(dp)               :: rho !Resistividade aparente
    real(dp)               :: phi !Fase
    real(dp)               :: w !Frequencia angular
    real(dp)               :: mod_Z !Módulo de Z
    integer                :: i !Auxiliar de laço


    !Abertura de arquivos de entrada e saída
    open(1, file='mt1d.out', status='old', action='read')
    open(2, file='output_mt.dat', status='replace', action='write')

    
    !Começamos com a leitura do número de frequências N:
    read(1,*)N

    !Agora vamos para o laço de leitura e calculos
    do i = 1,N
        !Leitura dos dados do arquivo de entrada (f e Z)
        read(1,*)f,Z
        !Com f e Z definidos, vamos para os calculos:
        !Definimos algumas variáveis para utilizar nos cálculos:
        w = 2.d0*pi*f !Frequencia angular
        Im_Z = aimag(Z) !Parte imaginária de Z
        Re_Z = real(Z) !Parte real de Z
        T = 1.d0/f !Período

        !Resistividade aparente: rho = (1/(mi*w))*|Z|^2
        mod_Z = sqrt(Re_Z**2 + Im_Z**2) !Módulo de Z
        rho = (1.d0/(mi*w))*(mod_Z**2) !Resistividade aparente

        !Fase: phi = arctan(Im(Z)/Re(Z))
        !Nesse caso usaremos a função intriseca atan2
        phi = atan2(Im_Z,Re_Z) !Fase em radianos
        phi = phi*rad_grau !Converter a fase para graus

        !Por último, vamos escrever os dados no arquivo de saída:
        write(2,*)T,rho,phi
    end do

end program MT