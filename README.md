GeochemAnalytical
App Shiny interativo para leitura, integração e processamento avançado de boletins geoquímicos dos laboratórios ACME e GEOSOL.

🚀 Novidades da Versão 2.0
Processamento baseado em Upload: Não depende mais de caminhos locais; basta enviar os arquivos ZIP.

Integração com OS: Processa e cruza dados de Ordens de Serviço (Excel) com os resultados laboratoriais.

Preparação Geochem: Módulo automático para geração de tabelas prontas para SIG (estatística, estações e duplicatas).

Estrutura de Saída Organizada: O ZIP de exportação agora separa os arquivos em subpastas (boletins/, os/, geochem/).

Características
Suporte multi-laboratório: ACME e GEOSOL.

Classes de amostras: Concentrado de bateia, Sedimento de corrente, Rocha, Solo, Água.

Visualização Dinâmica: Tabelas interativas para conferência imediata dos dados processados.

Download Estruturado: Exporta múltiplos CSVs organizados por categoria.

Compatibilidade: Saídas em CSV (separador ;, decimal ,) com encoding latin1 para abertura direta no Excel.

Requisitos
R 4.0+

Pacotes R:

shiny, shinydashboard

DT (tabelas interativas)

readxl (leitura de OS)

dplyr (processamento de dados)

zip

Instalação de dependências
R

install.packages(c("shiny", "shinydashboard", "DT", "readxl", "dplyr", "zip"))
Uso
Abra o R ou RStudio.

Navegue até o diretório do projeto.

Execute shiny::runApp().

No painel Processamento:

Faça o upload do ZIP contendo os Boletins.

Faça o upload do ZIP contendo as Ordens de Serviço (OS).

Insira o nome do Projeto, Centro de Custo e o Método Analítico alvo.

Clique em "Executar Processamento".

Verifique os resultados nas abas Visualização e Estatística.

Clique em "Baixar todos (.zip)" para obter os resultados.

Estrutura do Projeto
GeochemAnalytical/
├── app.R                       # Aplicação Shiny (UI/Server)
├── extrai_dados_os.R           # Processamento de planilhas de OS
├── prepara_dados_geochem.R     # Lógica de cruzamento e exportação SIG
├── R/
│   ├── le_boletim_acme.R       # Parser para laboratório ACME
│   └── le_boletim_geosol.R     # Parser para laboratório GEOSOL
├── inputs/
│   └── ucc/ucc.csv             # Dados de referência UCC
└── README.md
Estrutura do ZIP de Saída
O ZIP gerado segue a convenção processamento_<classe>_<lab>_YYYYMMDD.zip e contém:

📂 boletins/: CSVs de dados brutos, transformados e QA/QC.

📂 os/: Dados extraídos das Ordens de Serviço.

📂 geochem/:

amostras e resultados analíticos.csv (Tabela mestre para SIG)

estações das amostras analisadas.csv

duplicatas de campo.csv

condições analíticas.csv

Notas Técnicas
Limpeza Automática: Todos os arquivos temporários de upload são deletados ao encerrar a sessão do Shiny.

Segurança: O app valida a existência de colunas obrigatórias como NUM_LAB e Boletim antes de processar as estatísticas.

Autor
Viviane Ferrari

Licença
MIT