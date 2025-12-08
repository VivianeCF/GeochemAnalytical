# GeochemAnalytical

App Shiny interativo para leitura e processamento de boletins geoquímicos dos laboratórios ACME e GEOSOL.

## Características

- **Seleção de diretório**: Interface GUI para selecionar pasta contendo boletins
- **Suporte multi-laboratório**: ACME e GEOSOL
- **Classes de amostras**: Concentrado de bateia, Sedimento de corrente, Rocha, Solo, Água
- **Download em ZIP**: Exporta todos os CSVs processados em um arquivo comprimido
- **Formato português**: Saídas em CSV com separador `;` e decimal `,` (write.csv2)
- **Encoding**: Suporte a caracteres especiais com encoding latin1

## Requisitos

- R 4.0+
- Pacotes R:
  - `shiny`
  - `shinyFiles`

### Instalação de dependências

```r
install.packages(c("shiny", "shinyFiles"))
```

## Uso

1. Abra o R ou RStudio
2. Navegue até o diretório do projeto
3. Execute:

```r
shiny::runApp("app.R")
```

4. No navegador que abrir:
   - Clique em "Escolher diretório..." e selecione a pasta com os boletins
   - Escolha o laboratório (ACME ou GEOSOL)
   - Selecione a classe da amostra
   - Clique em "Ler boletins e preparar downloads"
   - Baixe o arquivo ZIP com os CSVs processados

## Estrutura do Projeto

```
GeochemAnalytical/
├── app.R                          # Aplicação Shiny principal
├── R/
│   ├── le_boletim_acme.R         # Função para ler boletins ACME
│   └── le_boletim_geosol.R       # Função para ler boletins GEOSOL
├── inputs/
│   ├── nomes_info.csv            # Informações de nomes
│   ├── ucc/ucc.csv               # Dados de referência UCC
│   └── boletins_raw/             # Boletins originais (não versionado)
├── outputs/                       # Saídas processadas (não versionado)
├── .gitignore
└── README.md
```

## Saídas

O aplicativo gera um ZIP com os seguintes arquivos:

- `dados_analíticos_brutos.csv` - Dados brutos sem processamento
- `dados_analíticos_transformados.csv` - Dados após transformações
- `dados_qaqc_transformados.csv` - Dados de QA/QC processados
- `informação_boletim.csv` - Metadados dos boletins

**Convenção de nomes do ZIP**: `boletins_<classe>_<lab>_YYYYMMDD.zip`

Exemplo: `boletins_sedimento_de_corrente_geosol_20251207.zip`

## Notas

- Os arquivos `inputs/nomes_info.csv` e `inputs/ucc/ucc.csv` são necessários para o funcionamento
- A pasta `outputs/` é gerada localmente mas não é versionada no Git
- Os CSVs são salvos com encoding latin1 para compatibilidade

## Autor

Viviane Ferrari

## Licença

MIT
