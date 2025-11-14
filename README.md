# **Sistema de Inventário – RA2 (Haskell)**  

 **Instituição:** Pontifícia Universidade Católica do Paraná – PUCPR  
 **Disciplina:** Programação Lógica e Funcional  
 **Professor:** Frank Coelho Alcantara  

---

##  **Integrantes do Grupo**  

- Emmanuel Antonietti Ribeiro dos Santos - [@emmanuelantonietti](https://github.com/emmanuelantonietti).
- Enzo Wasko Amorim - [@enzowasko](https://github.com/enzowasko).
- Juliano Cesar Enns Miranda Marcos - [@juliano2508](https://github.com/juliano2508).
- Vinicius Paraiso Dias – [@vinni-dias](https://github.com/vinni-dias).

---

##  **Descrição do Projeto**

Este projeto implementa um Sistema de Inventário completo em Haskell, dividido em quatro módulos funcionais:

- **Dados Puras** → Estruturas `Item`, `Inventario`, `LogEntry`, ADTs e tipos auxiliares  
- **Lógica de Negócio** → Funções puras `additem`, `removeItem`, `updateQty`, `listItems`  
- **Persistência (I/O)** → Leitura e escrita de `Inventario.dat` e `Auditoria.log` com tratamento de exceção  
- **Relatórios e Auditoria** → Funções `report`, `historicoPorItem`, `logsDeErro`, `itemMaisMovimentado`  

O programa funciona em linha de comando, salvando automaticamente o estado do inventário e registrando logs detalhados com timestamp.

---

## 💻 **Comandos disponíveis**

| Comando | Descrição |
|--------|-----------|
| `add <id> <nome> <qtd> <categoria>` | Adiciona um novo item ao inventário ou aumenta a quantidade de um item existente. |
| `remove <id> <qtd>` | Remove a quantidade informada do item. Se a quantidade chegar a zero, o item é removido do inventário. |
| `update <id> <nova_qtd>` | Atualiza diretamente a quantidade do item para o valor informado. Se a nova quantidade for zero, o item é removido. |
| `list` | Lista todos os itens presentes no inventário com seus detalhes. |
| `report` | Gera um relatório completo contendo: logs de erro, top 3 itens mais movimentados e contagem de operações. |
| `historico <id>` | Exibe todas as operações registradas no log relacionadas ao item especificado. |
| `exit` / `quit` | Encerra o programa e finaliza a execução. |

---

##  **Exemplo de uso**


# Cenário 1 – Persistência de Estado (Sucesso)

**1. Iniciar o programa sem arquivos existentes**

O sistema imprime:

Aviso: Inventario.dat não encontrado. Iniciando com inventário vazio.
Aviso: Auditoria.log não encontrado. Iniciando com log vazio.

**2. Adicionar 3 itens**

add 01 PAPEL_A4 50 papelaria
Operação bem-sucedida. Estado salvo.

add 02 CANETA 20 escolar
Operação bem-sucedida. Estado salvo.

add 03 LAPIS 30 escolar
Operação bem-sucedida. Estado salvo.

**3. Fechar o programa**

exit
Fechando o sistema de inventário. Tchau!

**4. Verificar existência dos arquivos**

- `Inventario.dat` → criado  
- `Auditoria.log` → criado  

**5. Reiniciar o programa**

Ao iniciar novamente:

Inventário carregado com 3 itens.
Log de auditoria carregado com 3 entradas.

**6. Executar `list`**

list
- Inventário -
ID: 01 | Nome: PAPEL_A4 | Qtd: 50 | Cat: papelaria
ID: 02 | Nome: CANETA | Qtd: 20 | Cat: escolar
ID: 03 | Nome: LAPIS | Qtd: 30 | Cat: escolar

---

# Cenário 2 – Erro de Lógica (Estoque Insuficiente)

**1. Adicionar item com 10 unidades**

add 10 TECLADO 10 informatica
Operação bem-sucedida. Estado salvo.

**2. Tentar remover 15 unidades**

remove 10 15
Operação falhou. Log registrado : Estoque insuficiente. Disponível: 10

**3. Conferir Inventário**

list
ID: 10 | Nome: TECLADO | Qtd: 10 | Cat: informatica


**A quantidade permanece inalterada.**

**4. Verificar no Auditoria.log**

O log gerado contém :

LogEntry {timestamp = 2025-11-11 22:53:08.568472523 UTC, acao = Remove "10" 15, detalhes = "Estoque insuficiente. Disponivel: 10", status = Falha "Estoque insuficiente"}

---

# Cenário 3 – Geração de Relatório de Erros

**1. Executar comando `report`**

report

RELATÓRIO DE AUDITORIA DO LOG 

- Logs de Erro -
LogEntry {timestamp = 2025-11-14 22:53:08.568472523 UTC, acao = Remove "10" 15, detalhes = "Estoque insuficiente. Disponivel: 10", status = Falha "Estoque insuficiente"}

- Item Mais Movimentado (Top 3) -
1º: ID 10 com 2 operações.
2º: ID 01 com 1 operações.
3º: ID 02 com 1 operações.

==================================

---

## **Arquivos do Projeto**

| Arquivo | Função |
|---------|--------|
| `main.hs` | Código completo do sistema em Haskell |
| `arquitetodados.hs` | Código de dados puros |
| `logicaPura.hs` | Lógica de negócio pura |
| `Analiselogs.hs` | Análise de logs puras |
| `ModuloIO.hs` | Módulo I/O e persistência |
| `Inventario.dat` | Persistência do inventário |
| `Auditoria.log` | Logs com timestamp de cada operação |

---

##  **Ambiente de Execução**

OnlineGDB ou Replit:  
- https://onlinegdb.com/askmi5jMl

---
