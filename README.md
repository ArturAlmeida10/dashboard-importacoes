# Dashboard das Importações do Brasil (2016-2025)
Este projeto é uma ferramenta interativa desenvolvida em R e Shiny para a visualização e análise detalhada das importações brasileiras. O dashboard permite explorar fluxos comerciais com base nos dados oficiais do Ministério do Desenvolvimento, Indústria e Comércio Exterior (MDIC).

🔗 Acesse o Dashboard: https://artur-almeida.shinyapps.io/Dashboard-Importacoes-NCM-Paises/

## 📊 Funcionalidades
- **Série Histórica (2016 - 2025)**: Acompanhamento da evolução temporal das importações.

- **Filtro por NCM**: Busca detalhada pela Nomenclatura Comum do Mercosul.

- **Análise por Origem**: Visualização de dados segmentados por país de origem.

- **Indicadores Econômicos**:

  - Valor FOB (US$): Valor das mercadorias no porto de embarque.

  - Quilograma Líquido (KG): Volume físico das importações.

  - Preço Médio: Cálculo para análise de termos de troca (FOB / KG).

- **Interface Interativa**: Gráficos dinâmicos e tabelas exportáveis.

## 📁 Estrutura do Projeto
- **`app.R`**: Script principal que gerencia a interface (UI) e a lógica do servidor (Server) do Shiny.

- **`processa_dados.R`**: Script de processamento dos dados. Responsável por baixar os dados direto do MDIC, realizar agregações e salvar o arquivo otimizado .rds.

- **`ncm_lookup.csv`**: Tabela de referência com as descrições oficiais dos códigos NCM.

- **`pais_lookup.xlsx`**: Tabela de referência para conversão de códigos de países em nomes amigáveis.

- **`base_agregada_ncm_paises.rds`**: Base de dados comprimida e processada para leitura rápida pelo dashboard.

## 🛠️ Tecnologias Utilizadas
- **Linguagem**: R

- **Framework**: Shiny

- **Visualização**: plotly


## 📚 Fonte dos Dados
Os dados brutos são provenientes do Comex Stat (MDIC), o sistema oficial de estatísticas de comércio exterior do Brasil.
