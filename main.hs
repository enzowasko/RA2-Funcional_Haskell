import Data.Map (Map, empty, insert, delete, lookup, toList, null)
import Data.Time (UTCTime, getCurrentTime)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)
import System.IO (appendFile, writeFile, readFile, putStr, getLine, putStrLn)
import Text.Read (readMaybe)
import Data.List (sortBy, filter, foldl')
import qualified Data.List as List -- Uso qualificado para evitar conflitos (e.g., List.null)
import Data.Ord (comparing)

data Item = Item
    { itemID     :: String
    , nome       :: String
    , quantidade :: Int
    , categoria  :: String
    } deriving (Show, Read)

type Inventario = Map String Item


data AcaoLog = Add String Int | Remove String Int | Update String Int | QueryFail String
    deriving (Show, Read)

data StatusLog = Sucesso | Falha String
    deriving (Show, Read)
    
data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao      :: AcaoLog
    , detalhes  :: String
    , status    :: StatusLog
    } deriving (Show, Read)

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaLogFile :: FilePath
auditoriaLogFile = "Auditoria.log"

additem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either LogEntry (Inventario, LogEntry)
additem now id_ nome_ qtd_ cat_ inv
    | qtd_ <= 0 =
        let logEntry = LogEntry now (Add id_ qtd_) "Quantidade deve ser positiva para adicionar." (Falha "Qtd inválida")
        in Left logEntry
    | otherwise =
        case Data.Map.lookup id_ inv of
            Just itemExistente ->
                let novaQtd = quantidade itemExistente + qtd_
                    itemAtualizado = itemExistente {quantidade = novaQtd}
                    novoInv = Data.Map.insert id_ itemAtualizado inv
                    detalhe = "Quantidade de item existente atualizada para " ++ show novaQtd
                    logEntry = LogEntry now (Add id_ qtd_) detalhe Sucesso
                in Right (novoInv, logEntry)
            Nothing ->
                let newItem = Item id_ nome_ qtd_ cat_
                    novoInv = Data.Map.insert id_ newItem inv
                    detalhe = "Novo item adicionado: " ++ nome_ ++ " (" ++ show qtd_ ++ ")"
                    logEntry = LogEntry now (Add id_ qtd_) detalhe Sucesso
                in Right (novoInv, logEntry)

removeltem :: UTCTime -> String -> Int -> Inventario -> Either LogEntry (Inventario, LogEntry)
removeltem now id_ qtd_ inv =
    case Data.Map.lookup id_ inv of
        Nothing ->
            let msg = "Item com ID " ++ id_ ++ " nao encontrado."
                logEntry = LogEntry now (Remove id_ qtd_) msg (Falha "Item nao encontrado")
            in Left logEntry
        Just itemExistente
            | qtd_ <= 0 ->
                let msg = "Quantidade deve ser positiva para remover."
                    logEntry = LogEntry now (Remove id_ qtd_) msg (Falha "Qtd inválida")
                in Left logEntry
            | quantidade itemExistente < qtd_ ->
                let msg = "Estoque insuficiente. Disponivel: " ++ show (quantidade itemExistente)
                    logEntry = LogEntry now (Remove id_ qtd_) msg (Falha "Estoque insuficiente")
                in Left logEntry
            | otherwise ->
                let novaQtd = quantidade itemExistente - qtd_
                    detalhe = "Quantidade de item " ++ id_ ++ " reduzida para " ++ show novaQtd
                    logEntry = LogEntry now (Remove id_ qtd_) detalhe Sucesso
                in if novaQtd == 0
                    then
                        let novoInv = Data.Map.delete id_ inv
                            finalLog = LogEntry now (Remove id_ qtd_) "Item removido totalmente (Qtd zero)." Sucesso
                        in Right (novoInv, finalLog)
                    else
                        let itemAtualizado = itemExistente {quantidade = novaQtd}
                            novoInv = Data.Map.insert id_ itemAtualizado inv
                        in Right (novoInv, logEntry)

updateQty :: UTCTime -> String -> Int -> Inventario -> Either LogEntry (Inventario, LogEntry)
updateQty now id_ novaQtd inv =
    case Data.Map.lookup id_ inv of
        Nothing ->
            let msg = "Item com ID " ++ id_ ++ " nao encontrado para atualizacao."
                logEntry = LogEntry now (Update id_ novaQtd) msg (Falha "Item nao encontrado")
            in Left logEntry
        Just itemExistente
            | novaQtd < 0 ->
                let msg = "Nova quantidade nao pode ser negativa."
                    logEntry = LogEntry now (Update id_ novaQtd) msg (Falha "Qtd negativa")
                in Left logEntry
            | otherwise ->
                let detalhe = "Quantidade do item " ++ id_ ++ " atualizada para " ++ show novaQtd
                    logEntry = LogEntry now (Update id_ novaQtd) detalhe Sucesso
                in if novaQtd == 0
                    then
                        let novoInv = Data.Map.delete id_ inv
                            finalLog = LogEntry now (Update id_ novaQtd) "Item removido totalmente (Qtd 0)." Sucesso
                        in Right (novoInv, finalLog)
                    else
                        let itemAtualizado = itemExistente {quantidade = novaQtd}
                            novoInv = Data.Map.insert id_ itemAtualizado inv
                        in Right (novoInv, logEntry)

listItems :: Inventario -> String
listItems inv
    | Data.Map.null inv = "Inventario Vazio.\n"
    | otherwise = "- Inventario -\n" ++ unlines (map formatItem (toList inv))
    where
        formatItem (_, item) = "ID: " ++ itemID item ++ " | Nome: " ++ nome item ++ " | Qtd: " ++ show (quantidade item) ++ " | Cat: " ++ categoria item


logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\(LogEntry {status = s}) -> case s of {Falha _ -> True; _ -> False})

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId logs = filter (\(LogEntry {acao = a}) -> itemActionId a == itemId) logs
    where
        itemActionId (Add id_ _) = id_
        itemActionId (Remove id_ _) = id_
        itemActionId (Update id_ _) = id_
        itemActionId (QueryFail id_) = id_

report :: [LogEntry] -> String
report logs =
    " RELATORIO DE AUDITORIA DO LOG \n" ++
    "\n- Logs de Erro -\n" ++
    (if List.null errors then "Nenhuma falha registrada.\n" else unlines (map formatLogEntry errors)) ++
    "\n- Item Mais Movimentado (Top 3) -\n" ++
    (if List.null allCounts then "Nenhuma movimentacao registrada.\n" else unlines top3Report)
    where
        errors = logsDeErro logs
        isMovement (LogEntry {acao = Add _ _}) = True
        isMovement (LogEntry {acao = Remove _ _}) = True
        isMovement (LogEntry {acao = Update _ _}) = True
        isMovement _ = False
        itemActionId (LogEntry {acao = Add id_ _}) = id_
        itemActionId (LogEntry {acao = Remove id_ _}) = id_
        itemActionId (LogEntry {acao = Update id_ _}) = id_
        itemActionId _ = ""

        allCounts = List.sortBy (comparing (negate . snd)) . List.map (\l -> (List.head l, List.length l)) . List.groupBy (==) . List.sortBy (comparing id) . List.map itemActionId . List.filter isMovement $ logs
        
        top3Report = List.zipWith (\i (id', count) -> show i ++ "º: ID " ++ id' ++ " com " ++ show count ++ " operacoes.") [1..] (List.take 3 allCounts)


formatLogEntry :: LogEntry -> String
formatLogEntry logEntry =
    "[" ++ show (timestamp logEntry) ++ "] " ++ show (acao logEntry) ++ " | Status: " ++ show (status logEntry) ++ " | Detalhes: " ++ detalhes logEntry


carregarInventario :: IO Inventario
carregarInventario = do
    conteudo <- catch (readFile inventarioFile) handleMissingFile
    case readMaybe conteudo of
        Just inv -> return inv
        Nothing -> putStrLn "Aviso: Falha na desserialização do Inventario.dat. Iniciando com inventário vazio." >> return empty
  where
    handleMissingFile e
      | isDoesNotExistError e = putStrLn "Aviso: Inventario.dat nao encontrado. Iniciando com inventário vazio." >> return (show (empty :: Inventario))
      | otherwise = ioError e

carregarLogs :: IO [LogEntry]
carregarLogs = do
    conteudo <- catch (readFile auditoriaLogFile) handleMissingFile
    let logs_lines = lines conteudo
    let parsedLogs = List.foldl' (\acc line -> case readMaybe line :: Maybe LogEntry of
                                                    Just l -> acc ++ [l]
                                                    Nothing -> acc) [] logs_lines
    return parsedLogs
  where
    handleMissingFile e
      | isDoesNotExistError e = putStrLn "Aviso: Auditoria.log nao encontrado. Iniciando com log vazio." >> return ""
      | otherwise = ioError e

sincronizar :: Inventario -> LogEntry -> IO ()
sincronizar novoInv logEntry = do
    writeFile inventarioFile (show novoInv)
    appendFile auditoriaLogFile (show logEntry ++ "\n")  
    putStrLn "Operacao bem-sucedida. Estado salvo."

registrarFalha :: LogEntry -> IO ()
registrarFalha logEntry = do
    appendFile auditoriaLogFile (show logEntry ++ "\n")
    putStrLn $ "Operacao falhou. Log registrado: " ++ detalhes logEntry

executarEPersistir :: Inventario -> [LogEntry] -> (Inventario -> Either LogEntry (Inventario, LogEntry)) -> IO (Inventario, [LogEntry])
executarEPersistir inv logs operacaoPuraComHora =
    case operacaoPuraComHora inv of
        Right (novoInv, logEntry) -> sincronizar novoInv logEntry >> return (novoInv, logs ++ [logEntry])
        Left logEntry -> registrarFalha logEntry >> return (inv, logs ++ [logEntry])

processarComando :: String -> Inventario -> [LogEntry] -> IO (Inventario, [LogEntry])
processarComando input inv logs = do
    now <- getCurrentTime
    case words input of
        [] -> return (inv, logs)
        ["exit"] -> return (inv, logs)
        ["quit"] -> return (inv, logs)
        ["list"] -> putStrLn (listItems inv) >> return (inv, logs)
        ["report"] -> putStrLn (report logs) >> return (inv, logs)
        ["historico", id'] -> do
            let itemHistory = historicoPorItem id' logs
            putStrLn $ "\n- Historico de Log para Item ID: " ++ id' ++ " -\n"
            putStrLn $ unlines (List.map formatLogEntry itemHistory)
            return (inv, logs)

        ("add":id':name:qtdStr:cat:[]) ->
            case readMaybe qtdStr :: Maybe Int of
                Just qtd ->
                    let operacao = additem now id' name qtd cat
                    in executarEPersistir inv logs operacao
                Nothing -> putStrLn "Erro: Quantidade invalida. Use: add <id> <nome> <qtd> <cat>" >> return (inv, logs)

        ("remove":id':qtdStr:[]) ->
            case readMaybe qtdStr :: Maybe Int of
                Just qtd ->
                    let operacao = removeltem now id' qtd
                    in executarEPersistir inv logs operacao
                Nothing -> putStrLn "Erro: Quantidade invalida. Use: remove <id> <qtd>" >> return (inv, logs)

        ("update":id':qtdStr:[]) ->
            case readMaybe qtdStr :: Maybe Int of
                Just qtd ->
                    let operacao = updateQty now id' qtd
                    in executarEPersistir inv logs operacao
                Nothing -> putStrLn "Erro: Nova quantidade invalida. Use: update <id> <nova_qtd>" >> return (inv, logs)

        _ -> putStrLn "Comando invalido. Use: add <id> <nome> <qtd> <cat> | remove <id> <qtd> | update <id> <nova_qtd> | list | report | historico <id> | exit" >> return (inv, logs)

loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
    putStr "\n> "
    input <- getLine
    if input `elem` ["exit", "quit"]
        then putStrLn "Fechando o sistema de Inventario. Tchau!"
        else do
            (novoInv, novosLogs) <- processarComando input inv logs
            loop novoInv novosLogs

main :: IO ()
main = do
    putStrLn "Iniciando Sistema de Inventário Haskell (RA2)..."
    initialInv <- carregarInventario
    initialLogs <- carregarLogs
    putStrLn $"Inventário carregado com " ++ show (List.length $ toList initialInv) ++ " itens."
    putStrLn $ "Log de auditoria carregado com " ++ show (List.length initialLogs) ++ " entradas."
    putStrLn "Sistema pronto. Digite comandos. 'exit' para fechar."
    loop initialInv initialLogs