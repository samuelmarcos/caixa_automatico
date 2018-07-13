module Main where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import System.Environment (getArgs, getProgName)
import Text.Printf
import System.Exit ( exitFailure )



type Associado = [(String, Double, Double)]
type Estado = [ (Int, Associado ) ]--como as variaveis nao sao atualizaveis
--representação dos dados do problema em um dado momento
--saldo da conta
main :: IO ()
main = do   hSetBuffering stdout NoBuffering
            putStrLn "Simulacao de caixa automatico"
            putStrLn ("Versao" ++ versao)
            caminho <- getArgs
            case caminho of
                [arg1,arg2] -> do entrada <- readFile arg1
                                  estado1 <- menu (listaConta (words entrada))
                                  writeFile arg2 (unwords (mudaLinha estado1))
                                  return ()
                _      -> exitFailure

            where versao :: String
                  versao = "v0.1"

menu :: Estado -> IO Estado
menu sal = do putStrLn "Simulacao de caixa automatico"
              putStrLn "============================"
              putStrLn "Banco Funcional"
              putStrLn "============================"
              putStrLn "Opcoes: "
              putStrLn "1. Abertura de Conta"
              putStrLn "2. Saldo "
              putStrLn "3. Extrato"
              putStrLn "4. Depósito"
              putStrLn "5. Saque"
              putStrLn "6. Fim"
              opcao <- prompt "Escolha uma opçao: "
              case opcao of
                1 -> do sal' <- abreConta sal
                        menu sal'
                2 -> do saldo sal
                        menu sal
                3 -> do extrato sal
                        menu sal
                4 -> do sal' <- deposito sal
                        menu sal'
                5 -> do sal' <- saque sal
                        menu sal'
                6 -> do putStrLn "Encerramento"
                        return sal
                _ -> do putStrLn "Opção inválida"
                        menu sal

listaConta :: [String] -> Estado
listaConta [] = []
listaConta (conta:operacao:valor:saldo2:xs) = [(read conta, ((operacao, read valor, read saldo2) : listaConta' xs))]

listaConta' :: [String] -> Associado
listaConta' [] = []
listaConta' (operacao:valor:saldo2:xs) = (operacao, read valor, read saldo2) : (listaConta' xs)

mudaLinha ::Estado -> [String]
mudaLinha [] = []
mudaLinha [(conta, ((operacao, valor, saldo2) :xs ))] = ((show conta ) : operacao : (show valor ) : (show saldo2 ) : mudaLinha' xs)

mudaLinha' :: Associado -> [String]
mudaLinha' [] = []
mudaLinha' ((operacao,valor,saldo2) : xs) = ((operacao) : (show valor ) : (show saldo2  ) : mudaLinha' xs )


abreConta :: Estado -> IO Estado
abreConta (x:xs) = do num <- prompt "Informe o número de sua nova conta: "
                      if lookup num (x:xs) == Nothing
                        then do putStrLn "Conta aberta"
                                let tupla = (num,[("Vazio", 0.0, 0.0)])
                                return  (tupla : x : xs)
                                else do putStrLn "Essa conta já existe"
                                        return (x:xs)

saldo :: Estado -> IO Double
saldo sal = do num <- prompt "Informe o número de sua conta: "
               if lookup num sal == Nothing
                 then do putStrLn "Conta não existe"
                         return 0.0
                         else do let ((x,y):xs) = filter (\(iden, _ ) -> iden == num) sal
                                     (a,b,c) = head y
                                 putStrLn ("Seu saldo atual é de R$: " ++ show c ++ "00")
                                 return c

deposito :: Estado -> IO Estado
deposito sal = do num <- prompt "Informe o número de sua conta: "
                  if lookup num sal == Nothing
                    then do putStrLn "Conta não existe"
                            return sal
                            else do valor <- saldo sal
                                    depos <- prompt "Informe o valor a ser depositado: "
                                    if depos < 0
                                      then do putStrLn "Valor invalido"
                                              return sal
                                              else do let tupla = ("Deposito", depos, (valor+depos))
                                                          ((x,y):xs) = filter (\(iden, _ ) -> iden == num) sal
                                                          lista = (tupla:y)
                                                          sal' = (x,lista) : sal
                                                      return sal'


saque :: Estado -> IO Estado
saque sal = do num <- prompt "Informe o número de sua conta: "
               if lookup num sal == Nothing
                 then do putStrLn "Conta não existe"
                         return sal
                         else do valor <- saldo sal
                                 saque <- prompt "Informe o valor a ser sacado: "
                                 if saque < 0 || saque > valor
                                   then do putStrLn "Valor invalido"
                                           return sal
                                           else do let tupla = ("Saque", saque, (valor-saque))
                                                       ((x,y):xs) = filter (\(iden, _ ) -> iden == num) sal
                                                       lista = (tupla:y)
                                                       sal' = (x,lista) : sal
                                                   return sal'


extrato :: Estado -> IO Estado
extrato sal = do num <- prompt "Informe o número da sua conta: "
                 case lookup num sal of
                   Nothing -> do putStrLn "Conta não existe"
                                 return sal
                   Just op -> do printf "%+14s %+14s %+14s\n""Movimentaçao""Valor""Saldo"
                                 extrato' op
                                 return sal

extrato' :: [(String,Double,Double)] -> IO ()
extrato' [] = return ()
extrato' ((x,y,z):xs) = do printf "%+14s %14.2f %14.2f\n" x y z
                           extrato' xs


prompt :: Read a => String -> IO a
prompt mensage = do putStr mensage
                    readLn







--------------------------------
