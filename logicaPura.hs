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
    | otherwise = "--- Inventario ---\n" ++ unlines (map formatItem (toList inv))
    where
        formatItem (_, item) = "ID: " ++ itemID item ++ " | Nome: " ++ nome item ++ " | Qtd: " ++ show (quantidade item) ++ " | Cat: " ++ categoria item
