# GeochemAnalytical 💎

App Shiny interativo para leitura, integração e processamento avançado de boletins geoquímicos dos laboratórios ACME e GEOSOL.

## 🚀 Novidades da Versão 2.0
- **Processamento via Upload**: Interface baseada em arquivos ZIP, eliminando a necessidade de caminhos locais.
- **Integração com OS**: Extração automática de metadados de Ordens de Serviço (arquivos `.xlsx`).
- **Módulo Geochem**: Geração de tabelas estruturadas prontas para SIG (amostras, estações e duplicatas).
- **Estrutura de Saída Organizada**: Exportação em ZIP contendo subpastas lógicas (`boletins/`, `os/`, `geochem/`).

## 🛠️ Características

- **Suporte Multi-Laboratório**: Parsers específicos para ACME e GEOSOL.
- **Classes de Amostras**: Suporte para Concentrado de Bateia, Sedimento de Corrente, Rocha, Solo e Água.
- **Visualização em Tempo Real**: Abas interativas para conferência de dados (DT) antes do download.
- **Estatística Automática**: Resumo de contagem de amostras únicas por boletim.
- **Padrão de Saída**: CSVs formatados para Excel (separador `;`, decimal `,`) e encoding `latin1`.

## 📦 Requisitos

- **R 4.0+**
- **Pacotes R**: `shiny`, `shinydashboard`, `DT`, `readxl`, `dplyr`, `zip`.

### Instalação rápida:
```r
install.packages(c("shiny", "shinydashboard", "DT", "readxl", "dplyr", "zip"))
🖥️ Como Usar
Execute o arquivo app.R no RStudio.

Na aba Upload e Parâmetros:

Envie o arquivo ZIP com os Boletins.

Envie o arquivo ZIP com as Ordens de Serviço.

Preencha o Nome do Projeto, Centro de Custo e o Método Analítico.

Clique em "Executar Processamento".

Navegue pelas abas Visualização e Estatística para validar os dados.

Clique em "Baixar todos (.zip)" para obter os resultados estruturados.

📂 Estrutura do Projeto
Plaintext

GeochemAnalytical/
├── app.R                       # Código principal (UI e Server)
├── extrai_dados_os.R           # Script de processamento das OS
├── prepara_dados_geochem.R     # Script de integração e tabelas SIG
├── R/
│   ├── le_boletim_acme.R       # Lógica do laboratório ACME
│   └── le_boletim_geosol.R     # Lógica do laboratório GEOSOL
├── inputs/
│   └── ucc/ucc.csv             # Referência de valores UCC
│   └── nomes_info.csv          # Biblioteca dos nomes dos analitos
└── README.md                   # Documentação do projeto

📊 Estrutura do Arquivo de Saída
O aplicativo gera um ZIP organizado com a seguinte hierarquia:

Plaintext

processamento_YYYYMMDD.zip/
├── 📂 boletins/
│   ├── dados_analíticos_brutos.csv
│   ├── dados_analíticos_transformados.csv
│   └── informação_boletim.csv
├── 📂 os/
│   └── dados_extraidos_os.csv
└── 📂 geochem/
    ├── amostras e resultados analíticos.csv
    ├── estações das amostras analisadas.csv
    ├── duplicatas de campo.csv
    └── condições analíticas.csv

📝 Notas Técnicas
Encoding: Utiliza latin1 na exportação para compatibilidade total com Excel (caracteres especiais e acentos).

Limpeza: Arquivos temporários de upload são deletados automaticamente ao fechar a sessão.

Robustez: Busca arquivos Excel ignorando diferenciação entre maiúsculas e minúsculas no padrão .xlsx.

👩‍💻 Autora
Viviane Ferrari

📄 Licença
Este projeto está sob a licença MIT.


