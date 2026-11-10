# Painel de Monitoramento da Tuberculose — Recife

Painel em **R/Shiny** para monitorar a tuberculose em Recife (PE), com comparação
nos níveis Brasil, UF e município. Mostra indicadores (incidência, casos novos,
mortalidade, interrupção de tratamento, HIV positivo, cura), mapa coroplético por
bairro, séries históricas com previsão, pirâmides etárias e composição dos casos
por variável do SINAN.

- Fontes: **SINAN** (notificações de TB), **SIM** (óbitos), **população SVS/DATASUS**
  e a malha de bairros da Prefeitura do Recife.
- Período configurado: **2010–2023** (`shiny/core/apps/TUBERCULOSE/app.R`).
- Última carga de dados processada: dados até **31/12/2023**, processados em 04/08/2026
  (`data/parquet/meta/doenca=TUBERCULOSE/freshness.parquet`).

---

## ⚠️ Dados sensíveis: leia antes de tudo

A pasta `data/` **não vai para o git** e **não pode ser publicada**. Ela contém
microdados do SINAN e do SIM com **dados pessoais identificados**: nome do paciente,
nome da mãe, CNS, data de nascimento, telefone, endereço completo e
latitude/longitude da residência. São dados pessoais sensíveis de saúde (LGPD,
arts. 5º, II, e 11).

O `.gitignore` deste repositório:

- ignora **tudo** em `data/`, exceto `data/Imagens/` (logos do cabeçalho);
- ignora formatos de dado em qualquer pasta (`*.parquet`, `*.csv`, `*.rds`, `*.dbf`,
  shapefiles, `*.zip` etc.), como rede de segurança;
- ignora `.Renviron`, `shiny/logs/`, `app_cache/` e `.Rhistory`.

Antes do primeiro push, confira o que vai subir:

```bash
git status --ignored
```

```bash
git ls-files | grep -Ei "parquet|csv|rds|dbf|shp|zip|logs/" | grep -v "^etl/ref/"
```

O segundo comando **não pode retornar nada**. Os CSVs de `etl/ref/` são a única
exceção permitida: são agregados públicos e o dicionário de códigos, sem dado
individual (ver seção ETL). Se algum dado for commitado por engano,
apagar o arquivo num commit novo **não basta**: ele continua no histórico. Nesse caso
é preciso reescrever o histórico (`git filter-repo`) e tratar o caso como incidente.

---

## Estado da replicação: o que este repositório tem e o que falta

| Etapa | Situação |
|---|---|
| Código do painel (Shiny) | ✅ Completo. Todos os arquivos `.R` fazem parse e o app sobe com os dados atuais (testado em R 4.5.2). |
| Configuração de deploy (Shiny Server) | ✅ `shiny/deploy/shiny-server.conf` + `.Renviron.example` |
| Pré-processamento da malha de bairros | ✅ `shiny/scripts/preparar_geometria.R` |
| Pipeline ETL que gera `data/parquet/` | ✅ `etl/`, reescrito em 11/2026 (o original foi perdido). Reproduz 2010–2023 exatamente. |

### Sobre o ETL

O ETL original (de um projeto irmão, "painel RecifeTB") foi perdido. O `etl/` deste
repositório foi **reescrito por engenharia reversa**: as regras foram deduzidas
comparando os insumos brutos com os parquets em uso, e o resultado é conferido
contra eles por `etl/validar.R`. Em 10/11/2026, para 2010–2023, ele reproduz
**exatamente** (mesmas linhas, mesmos valores) todos os datasets que o painel lê:

| Dataset | Resultado |
|---|---|
| `sinan/` (27.580 casos) | idêntico |
| `incidence/` MUN, UF, BR | idêntico |
| `incidence/` BAIRRO: casos, curas, abandono, HIV, população, incidência | idêntico |
| `incidence/` BAIRRO: óbitos, mortalidade e letalidade | idêntico, mas **congelado** (ver abaixo) |
| `_cache_ts/`, `obitos/`, `piramides/`, `sinan_landing/`, `sinan_dict/`, `dim_geo/`, `meta/` | idêntico |

**Óbitos por bairro (congelados).** Não foi possível descobrir de onde o ETL
antigo tirava os óbitos por bairro. Os números não batem com nenhuma fonte
disponível e são **maiores que o total de óbitos do município**: em 2023 somam 377,
contra 141 óbitos por TB no SIM e 241 declarações de óbito que mencionam TB.

Decisão do projeto (11/2026): **manter os números antigos** para 2010–2023. Eles
ficam em `etl/ref/obitos_bairro.csv`, uma contagem por bairro e ano, o mesmo
número que o painel já publica. A mortalidade e a letalidade por bairro são
calculadas a partir deles.

**Para anos novos** não há como seguir a regra antiga. O ETL usa então óbitos do
SIM com causa básica A15–A19, pelo bairro de residência (`BAIRES`) e pelo ano do
óbito, e avisa no log. Esses anos **não são comparáveis** com 2010–2023: a regra
do SIM dá números bem menores (134 em 2023, contra 377 na série congelada). Quem
for publicar um ano novo precisa decidir como apresentar essa quebra de série.

A pasta `data/parquet/` atual continua sendo o gabarito. Mantenha um backup
criptografado dela, com acesso restrito, fora do git.

---

## Estrutura do repositório

```
.
├── .gitignore
├── README.md
├── data/                         # FORA DO GIT (exceto Imagens/) - ver "Dados"
│   ├── Imagens/                  # logos do cabecalho (versionado)
│   ├── parquet/                  # dados processados que o painel le
│   ├── csv/                      # insumos brutos do ETL (SINAN, SIM, pop. por bairro)
│   ├── fwmalharecife/            # shapefile dos bairros de Recife
│   └── geo/recife_bairros.rds    # malha pre-processada (gerada por script)
├── etl/                          # gera data/parquet/ a partir dos insumos brutos
│   ├── run_etl.R                 # ponto de entrada
│   ├── validar.R                 # compara uma saida nova com data/parquet
│   ├── R/                        # etapas 00_config ... 50_apoio
│   ├── ref/                      # tabelas de referencia versionadas (agregados, sem dado pessoal)
│   └── ferramentas/extrair_referencias.R   # como ref/ foi criado
└── shiny/
    ├── run.ps1                   # sobe o painel localmente no Windows
    ├── diagnose_server.R         # checa caminhos, dados e pacotes (nao sobe o app)
    ├── renv.lock                 # versoes exatas dos pacotes R
    ├── deploy/shiny-server.conf  # configuracao do Shiny Server (Linux)
    ├── scripts/                  # preparacao de dados e medicoes de performance
    ├── _backup/                  # versoes antigas dos modulos (usadas pelos scripts de paridade)
    └── core/
        ├── apps/TUBERCULOSE/     # app.R (ponto de entrada) + .Renviron.example
        ├── apps/_utils/          # inicializacao comum dos apps
        ├── diseases/TUBERCULOSE/ # disease_pack.R: titulo, cores, KPIs, rotulos
        └── R/                    # nucleo: config, paths, utils, app_shell, modules/
```

### Como o app é montado

`app.R` → `apps/_utils/app_utils.R::run_core_app()`, que faz, em ordem:

1. `source()` de `core/R/utils.R`, `config.R`, `app_shell.R` (num environment persistente);
2. `source()` dos módulos `mod_state`, `mod_data`, `mod_kpis`, `mod_map`, `mod_charts` e
   `performance_utils.R`;
3. carrega `core/diseases/TUBERCULOSE/disease_pack.R`;
4. `core_get_config()` localiza a raiz do projeto (a pasta que contém `data/parquet`);
5. `core_build_ui()` + `core_build_server()` → `shinyApp()`.

A raiz é descoberta assim: se `SINAN_PARQ_ROOT` ou `SINAN_ROOT` estiverem definidas,
usa essas variáveis. Senão, sobe de `shiny/core/` até achar uma pasta com
`data/parquet`. Por isso, **`data/` precisa ficar ao lado de `shiny/`**, ou as
variáveis de ambiente precisam estar definidas.

---

## Passo a passo para replicar o sistema

Siga nesta ordem.

### 1. Instalar o R e as dependências de sistema

- **R 4.5.2** (versão do `renv.lock`).
- **Windows:** só o R. Os pacotes vêm em binário do CRAN.
- **Linux (Ubuntu/Debian),** para `sf` e `arrow`:

```bash
sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev libudunits2-dev libssl-dev libcurl4-openssl-dev libxml2-dev
```

No Linux, use binários do Posit Package Manager (P3M) para instalar o `arrow` sem
compilar. O painel precisa do `arrow` **com suporte a `dataset`**:

```bash
Rscript -e 'cat(names(which(unlist(arrow::arrow_info()$capabilities))), sep=", ")'
```

A saída precisa conter `dataset`.

### 2. Clonar o repositório

```bash
git clone <url-do-repositorio> monitoramento-tuberculose
```

### 3. Instalar os pacotes R

**Opção A (recomendada, igual ao ambiente atual).** O `run.ps1` roda com o R
"limpo" (`--vanilla`) e usa os pacotes da biblioteca do sistema:

```bash
Rscript -e 'install.packages(c("shiny","bslib","bsicons","htmltools","htmlwidgets","later","arrow","dplyr","rlang","magrittr","stringr","digest","sf","leaflet","echarts4r","jsonlite"))'
```

**Opção B (versões exatas do `renv.lock`):**

```bash
Rscript -e 'install.packages("renv"); renv::restore(project = "shiny", library = .libPaths()[1], prompt = FALSE)'
```

Opcional: `keras` (com TensorFlow) ativa a previsão LSTM nas séries históricas. Sem
ele, o painel funciona normalmente e só pula essa previsão.

### 4. Colocar os dados em `data/` (fora do git)

Há dois caminhos. Os dois exigem receber arquivos sensíveis por um canal seguro
(nunca pelo git nem por link público).

**4a. Gerar com o ETL (recomendado).** Coloque só os insumos brutos (veja a
[seção ETL](#etl-gerar-os-dados)) e rode:

```bash
Rscript etl/run_etl.R 2023
```

```bash
Rscript etl/validar.R data/parquet_novo data/parquet 2023
```

O segundo comando só faz sentido se você tiver a pasta `data/parquet/` antiga
para comparar. Depois, renomeie `data/parquet_novo` para `data/parquet`.

**4b. Copiar `data/` pronta** do ambiente atual para a raiz do clone, ao lado de
`shiny/`. O mínimo para o painel funcionar:

```
data/
├── Imagens/                  # ja vem no git
├── parquet/
│   ├── sinan/TUBERCULOSE/          (SENSIVEL)
│   ├── sinan_landing/
│   ├── sinan_dict/
│   ├── incidence/
│   ├── piramides/
│   ├── _cache_ts/
│   ├── obitos/
│   ├── dim_geo/
│   └── meta/
└── fwmalharecife/            # shapefile dos bairros (recife_bairros.*)
```

As demais pastas que existem hoje em `data/` **não são lidas pelo painel**. São
insumos ou intermediários do ETL e não precisam ir para o servidor (veja a tabela
na seção [Contrato de dados](#contrato-de-dados)).

### 5. Gerar a malha pré-processada dos bairros

```bash
Rscript shiny/scripts/preparar_geometria.R
```

Lê `data/fwmalharecife/` e grava `data/geo/recife_bairros.rds`, com a malha
simplificada (tolerância de 2 m). Para usar outra tolerância, passe-a em metros:
`Rscript shiny/scripts/preparar_geometria.R 5`. Sem esse arquivo o mapa funciona,
mas processa o shapefile a cada processo R (~525 ms a mais). Rode de novo sempre
que o shapefile mudar.

### 6. Verificar o ambiente

```bash
Rscript shiny/diagnose_server.R shiny/core/apps/TUBERCULOSE
```

Mostra a raiz detectada, as variáveis de ambiente, quais datasets foram encontrados
e quais pacotes faltam. Não sobe o painel.

### 7a. Rodar localmente (Windows)

```powershell
.\shiny\run.ps1 TUBERCULOSE
```

Opções: `-Port 8002` (porta fixa), `-Force` (mata o processo que estiver na porta),
`-Bind 0.0.0.0` (expõe na rede). O script acha o `Rscript.exe`, sobe o app, grava os
logs em `shiny/logs/` e abre o navegador.

### 7b. Rodar localmente (qualquer sistema)

```bash
Rscript -e "shiny::runApp('shiny/core/apps/TUBERCULOSE', port = 8002, launch.browser = TRUE)"
```

### 8. Deploy no Shiny Server (Linux)

1. Copie o projeto para o servidor, mantendo a estrutura (`shiny/` e `data/` lado a lado):

   ```bash
   sudo rsync -a ./ /srv/shiny-server/monitoramento-tuberculose/
   ```

2. Crie o `.Renviron` a partir do exemplo e ajuste os caminhos:

   ```bash
   cd /srv/shiny-server/monitoramento-tuberculose/shiny/core/apps/TUBERCULOSE && sudo cp .Renviron.example .Renviron
   ```

3. Instale a configuração do servidor. Se o servidor já tiver outras aplicações,
   copie **só** o bloco `location /monitoramento-tuberculose`:

   ```bash
   sudo cp shiny/deploy/shiny-server.conf /etc/shiny-server/shiny-server.conf
   ```

4. Garanta que o usuário `shiny` lê os arquivos e reinicie:

   ```bash
   sudo chown -R shiny:shiny /srv/shiny-server/monitoramento-tuberculose && sudo systemctl restart shiny-server
   ```

5. Rode o diagnóstico como o usuário `shiny` e faça as checagens descritas no fim do
   `shiny-server.conf` (PID estável, 2ª visita em ~3 s, `arrow` com `dataset`, `.rds`
   presente):

   ```bash
   sudo -u shiny Rscript /srv/shiny-server/monitoramento-tuberculose/shiny/diagnose_server.R /srv/shiny-server/monitoramento-tuberculose/shiny/core/apps/TUBERCULOSE
   ```

O painel fica em `http://<servidor>:3838/monitoramento-tuberculose/`.

Dois pontos da configuração importam:

- `app_idle_timeout 0` mantém o processo R vivo entre visitas. Todos os caches
  ficam no processo; sem isso a abertura cai de ~2,5 s para ~11 s.
- `app_dir` aponta para `shiny/core/apps/TUBERCULOSE`, porque o `app.R` usa o
  diretório de trabalho para achar `../_utils` e o `.Renviron`.

> **Recomendação de segurança:** no exemplo acima os microdados ficam dentro de
> `/srv/shiny-server`, que é o `site_dir` do servidor. É mais seguro guardar
> `data/` fora dele (ex.: `/srv/dados/tuberculose/data`), com permissão só para o
> usuário `shiny`, e apontar `SINAN_ROOT`, `SINAN_PARQ_ROOT`, `SINAN_SUPPORT_ROOT` e
> `SINAN_IMG_ROOT` no `.Renviron` para lá.

### Variáveis de ambiente

| Variável | Para quê | Padrão |
|---|---|---|
| `SINAN_ROOT` | Raiz do projeto (pasta que contém `data/parquet`) | descoberta subindo diretórios |
| `SINAN_PARQ_ROOT` | Pasta dos parquets | `<ROOT>/data/parquet` |
| `SINAN_SUPPORT_ROOT` | Pasta do shapefile de bairros | procura `fwmalharecife/` subindo diretórios |
| `SINAN_IMG_ROOT` | Pasta das imagens do cabeçalho | `<ROOT>/data/Imagens` |
| `SINAN_DEBUG` | `1` mostra erros detalhados e faz contagens de diagnóstico no startup | desligado |
| `LANG` / `LC_ALL` | `pt_BR.UTF-8`, evita erro no cruzamento de nomes de bairro | — |

---

## ETL: gerar os dados

O `etl/` lê os insumos brutos e grava, numa pasta nova, todos os datasets do
[Contrato de dados](#contrato-de-dados). Por segurança, ele **nunca grava em
`data/parquet/`** e se recusa a sobrescrever uma pasta que já tenha arquivos.

### Insumos (todos fora do git)

| Arquivo | O que é | De onde vem | Sensível? |
|---|---|---|---|
| `data/csv/SINAN_RECIFE_TB.csv` | notificações de TB de residentes em Recife, com geocodificação (`address`, `LATITUDE`, `LONGITUDE`, `DSC_LOCALIDADE`…) | export do SINAN Net pela vigilância do município | 🔴 Sim |
| `data/csv/SIM_RECIFE_TB.csv` | declarações de óbito de residentes em Recife que mencionam TB | export do SIM pela vigilância | 🔴 Sim |
| `data/csv/pop_bairro_estimado.csv` | população por bairro e ano (Censo 2010/2022 + interpolação geométrica; já vai até 2026) | estimativa própria | não |
| `data/parquet/popsvs/POPSBR<aa>.zip` | população SVS/DATASUS por município, sexo e idade (já vai até 2025) | DATASUS, "Estimativas populacionais SVS" | não |
| `etl/ref/*.csv` (no git) | dicionário de rótulos do SINAN, os 94 bairros, óbitos por bairro 2010–2023, números do Brasil e centróides dos municípios | congelados dos parquets antigos por `etl/ferramentas/extrair_referencias.R` | não |

Os CSVs do SINAN e do SIM precisam vir em **UTF-8, separados por vírgula**, com
os nomes de coluna padrão do DATASUS, como os atuais.

### Rodar

```bash
Rscript etl/run_etl.R 2023
```

Argumentos: o ano final (padrão 2023) e, opcionalmente, a pasta de saída (padrão
`data/parquet_novo`). Leva cerca de 30 s. Depois, valide e troque as pastas:

```bash
Rscript etl/validar.R data/parquet_novo data/parquet 2023
```

O validador compara dataset por dataset e imprime só contagens e diferenças de
agregados, nunca dado individual. Termina com código 1 se algo divergir.

### Regras de negócio (deduzidas do ETL antigo)

| Item | Regra |
|---|---|
| Caso de TB | linha do SINAN com `TRATAMENTO` em 1, 2, 3, 4 ou 6 (exclui 5 = transferência) e `SITUA_ENCE` ≠ 6 (exclui mudança de diagnóstico). Ano = `NU_ANO`. |
| Cura | `SITUA_ENCE` = 1 |
| Abandono (interrupção) | `SITUA_ENCE` = 2. Interrupção % = abandono ÷ casos com `SITUA_ENCE` preenchido. |
| Óbito, **KPI anual e mortalidade** | SIM, causa básica A15–A19, ano do óbito |
| Óbito, **por bairro** | 2010–2023: congelado em `etl/ref/obitos_bairro.csv`. Anos novos: SIM por bairro de residência (quebra de série). |
| Óbito, **série mensal e pirâmide** | SINAN, `SITUA_ENCE` = 3, ano/mês da notificação. É uma inconsistência herdada do ETL antigo e foi mantida para o painel não mudar. |
| HIV positivo | `HIV` = 1; no bairro, "testados" = todos os casos (como no antigo) |
| Mês da série | mês de `DT_NOTIFIC` |
| Bairro | `NM_BAIRRO` normalizado (sem acento, maiúsculas), com os apelidos de `BAIRRO_APELIDOS` em `etl/R/00_config.R` |
| Faixas etárias | 0–4, 5–9, 10–14, 15–19, depois decenais até 80+. `ratio` = casos ÷ população da faixa × 100. |
| Níveis UF e MUN | repetem os números de Recife (o painel é travado em Recife) |
| Nível Brasil | lido de `etl/ref/nacional_*.csv` |

### Limitação: números do Brasil

Os números do Brasil (KPIs de comparação e série mensal nacional) vinham do SINAN e
do SIM **nacionais** do DATASUS, que não estão nesta pasta. Para 2010–2023 eles
estão congelados em `etl/ref/nacional_anual.csv`, `nacional_mensal.csv` e
`nacional_obitos.csv`. Para um ano novo, sem uma linha nesses arquivos, o painel
fica sem o comparativo Brasil daquele ano (o ETL avisa). Para preencher, acrescente
a linha com os totais nacionais do ano: casos novos, curas e óbitos, com as mesmas
regras acima, tirados do TABNET/DATASUS.

---

## Contrato de dados

O painel lê datasets Parquet com particionamento Hive em `data/parquet/`, gerados
pelo `etl/`. Esta é a especificação de cada um.

### Lidos pelo painel

| Dataset | Layout | Colunas principais | Usado em | Sensível? |
|---|---|---|---|---|
| `sinan/TUBERCULOSE/` | `<ano>.parquet` (um arquivo por ano, 2010–2023) | microdado SINAN completo (148 col.) + geocodificação: `nu_ano`, `nm_bairro`, `address`, `latitude`, `longitude`, `dsc_localidade` … | `mod_map`, `mod_charts` (Recife por bairro) | 🔴 **Sim**: nome, CNS, mãe, telefone, endereço, coordenadas |
| `sinan_landing/` | `doenca=/nivel={BR,UF,MUN}/ano=/part-*.parquet` | `doenca, nivel, geo_id, uf, geo_nome, sexo, variavel, valor, n, valor_lbl, ano` | composição por variável | agregado (atenção a células pequenas) |
| `sinan_dict/` | `doenca=/part-*.parquet` | `variavel, valor, valor_lbl, anos, n_total, n_years` | rótulos das variáveis | não |
| `incidence/` | `doenca=/nivel={BR,UF,MUN,BAIRRO}/ano=/part-*.parquet` | `ano, uf, bairro_key, bairro_nome, casos_total, casos_M, casos_F, casos_obitos, casos_cura, abandono, interrupcao_trat_pct, hiv_tested, hiv_pos, hiv_pos_pct, pop_total, incid_100k_total, mortalidade_100k_total, letalidade_pct` | KPIs, mapa, gráficos | agregado |
| `piramides/` | `nivel=/tipo={CASOS,CURA,OBITOS}/doenca=/ano=/part-*.parquet` | `nivel, tipo, ano, geo_id, uf, geo_nome, sexo, faixa_ord, faixa_etaria, valor, pop, ratio` | pirâmides etárias | agregado |
| `_cache_ts/` | `nivel=/doenca=/ano=/part-*.parquet` | `nivel, ano, mes, mes_nome, uf, geo_id, geo_nome, casos, casos_obitos, casos_cura, pop_total, incid_100k` | séries mensais | agregado |
| `obitos/doenca=TUBERCULOSE/` | `nivel=/ano=/part-0.parquet` | `cod_uf, cod_mun, curas_sinan, obitos_sinan, casos_sinan, obitos_sim, casos_obitos, obitos_M, obitos_F` | KPI de mortalidade | agregado |
| `dim_geo/municipios_centroids/` | `uf=/part-0.parquet` | `cod_mun7, nome_mun, lon, lat` | mapa BR/UF | não |
| `meta/` | `doenca=/freshness.parquet` | `doenca, dados_ate, processado_em` | selo "dados até" | não |

Arquivos de apoio fora de `parquet/`:

| Arquivo | Usado em | Origem |
|---|---|---|
| `fwmalharecife/recife_bairros.*` (shapefile) | mapa de bairros | malha oficial da Prefeitura do Recife (dado público) |
| `geo/recife_bairros.rds` | mapa de bairros (caminho rápido) | gerado por `preparar_geometria.R` |
| `Imagens/*.jpeg, favicon.jpg` | cabeçalho | versionado no git |

### Presentes hoje em `data/`, mas não lidos pelo painel

São insumos do ETL ou sobras do ETL antigo. **Não precisam ir para o servidor de
produção**, e os marcados em vermelho não deveriam estar lá:

| Caminho | Conteúdo | Sensível? |
|---|---|---|
| `csv/SINAN_RECIFE_TB.csv` | insumo do ETL: export do SINAN (~30 mil linhas) | 🔴 Sim |
| `csv/SIM_RECIFE_TB.csv` | insumo do ETL: export do SIM, com nome, pais e endereço | 🔴 Sim |
| `Estrutura Recife/dados_geolink_recife_TB.csv` | linkage SINAN × SIM do ETL antigo; o ETL novo não usa | 🔴 Sim |
| `parquet/_dump_stage_sinan/` | staging do microdado SINAN (o código só guarda o caminho) | 🔴 Sim |
| `parquet/sinan_audit/` | auditoria do dicionário; `example_values` traz endereços reais | 🔴 Sim |
| `parquet/obitos/_cache_sim_do_base/` | óbitos do SIM por registro (data, idade, sexo, município) | 🟠 quase-identificável |
| `csv/pop_bairro_estimado.csv` | insumo do ETL: população por bairro | não |
| `csv/pop_bairro_rc.csv` | população por bairro, só Censo 2010; não usado | não |
| `parquet/popsvs/` | insumo do ETL (`POPSBR*.zip`); o ETL também grava aqui um parquet por ano | não |
| `parquet/canonical/`, `cases_new/`, `incidence_0_14/`, `obitos_sim_faixa/`, `cache_ts_sim_obitos/`, `_state_cases_new/` | intermediários do ETL | não |
| `Estrutura Recife/*.shp` (distritos, logradouros), `municipios.csv` | camadas de apoio | não |

---

## Atualizar os dados para um novo ano

Exemplo para 2024:

1. Substitua `data/csv/SINAN_RECIFE_TB.csv` e `data/csv/SIM_RECIFE_TB.csv` pelos
   exports novos, com todos os anos desde 2010. Confira se `POPSBR24.zip` existe em
   `data/parquet/popsvs/` e se `pop_bairro_estimado.csv` tem 2024. Hoje os dois têm.
2. Acrescente 2024 em `etl/ref/nacional_*.csv` (veja
   [Limitação: números do Brasil](#limitação-números-do-brasil)), ou aceite o
   painel sem o comparativo Brasil nesse ano.
3. Gere e confira:

   ```bash
   Rscript etl/run_etl.R 2024
   ```

   ```bash
   Rscript etl/validar.R data/parquet_novo data/parquet 2023
   ```

   A validação até 2023 mostra se a carga nova mudou o histórico. Isso é normal
   quando o SINAN corrige encerramentos antigos, mas vale olhar.
4. Troque as pastas, guardando a anterior como backup: `data/parquet` →
   `data/parquet_AAAAMMDD`, e `data/parquet_novo` → `data/parquet`.
5. Ajuste `year_max` em `shiny/core/apps/TUBERCULOSE/app.R` (hoje `2023L`).
6. Reinicie o app (`sudo systemctl restart shiny-server` ou
   `touch shiny/core/apps/TUBERCULOSE/restart.txt`). Os caches vivem no processo R,
   então sem reiniciar o painel continua mostrando os dados antigos.

---

## Scripts auxiliares (`shiny/scripts/`)

Nenhum deles é necessário para o painel funcionar, exceto `preparar_geometria.R`.

| Script | Para quê | Uso |
|---|---|---|
| `preparar_geometria.R` | Gera `data/geo/recife_bairros.rds` a partir do shapefile | `Rscript shiny/scripts/preparar_geometria.R [tol_m]` |
| `medir_performance.R` | Linha de base da camada de dados (mediana de 5 execuções) | `Rscript shiny/scripts/medir_performance.R depois\|antes` |
| `medir_clique.R` | Mede a abertura e o clique num bairro (mapa + gráficos) | `Rscript shiny/scripts/medir_clique.R` |
| `perfil_abertura.R` | Rprof da abertura, com processo frio e quente | `Rscript shiny/scripts/perfil_abertura.R` |
| `perfil_ui.R` | Custo de montar a UI e tamanho do payload | `Rscript shiny/scripts/perfil_ui.R` |
| `perfil_app_real.R` | Rprof do app real; grava `shiny/logs/perfil_app_real.txt` | `Rscript shiny/scripts/perfil_app_real.R [porta] [janela_seg]` |
| `verificar_paridade.R` | Compara `mod_charts` atual × `_backup` (resultado idêntico?) | `Rscript shiny/scripts/verificar_paridade.R original\|atual saida.rds` |
| `verificar_paridade_dados.R` | Idem para `mod_data` | `Rscript shiny/scripts/verificar_paridade_dados.R original\|atual saida.rds` |

As variantes `antes` e `original` dependem dos arquivos em `shiny/_backup/`. Por isso
essa pasta continua versionada.

---

## Pendências conhecidas

1. **Óbitos por bairro congelados.** Os números de 2010–2023 vêm de
   `etl/ref/obitos_bairro.csv`, porque a regra original não foi encontrada, e são
   implausíveis (veja [Sobre o ETL](#sobre-o-etl)). A partir de 2024 o ETL usa o
   SIM, com quebra de série. Também vale revisar as inconsistências herdadas: óbito
   do SIM no KPI e do SINAN na série mensal e na pirâmide, e os níveis UF/BR do
   `sinan_landing` repetindo Recife.
2. **Números do Brasil congelados** em `etl/ref/` até 2023. Anos novos precisam de
   uma linha acrescentada à mão.
3. **Texto livre nos agregados.** `sinan_landing/` e `sinan_dict/` contam
   `AGRAVOUTDE` e `EXTRAPUL_O`, que são texto digitado na notificação. Herdado do
   ETL antigo; ficam em `data/`, fora do git, mas o ideal é tirar essas duas
   variáveis de `COMPOSICAO_VARS`.
4. **Minimização dos dados em produção.** O painel lê `sinan/TUBERCULOSE/` inteiro
   (148 colunas, com nome, CNS, telefone etc.), mas para mapa e gráficos usa poucas
   colunas. O recomendado é o ETL gravar uma versão reduzida e pseudonimizada (sem
   identificadores diretos) e só ela ir para o servidor.
5. **Pontos no mapa estão desligados.** `.draw_tb_recife_points()` em `mod_map.R` só
   limpa a camada e não desenha nada. Mantenha assim: mostrar o endereço do paciente
   num painel seria exposição de dado pessoal.
6. **`year_max` fixo** em `app.R` (2023). Precisa ser ajustado a cada nova carga.
7. **`run.ps1`**: o parâmetro `-NoRenv` é aceito, mas não faz nada (o renv está
   sempre desligado). Como roda com `--vanilla`, um `.Renviron` local é ignorado no
   Windows.
8. **renv incompleto.** Há `renv.lock` e `renv/settings.json`, mas não há
   `renv/activate.R` nem `.Rprofile`, então o projeto não ativa o renv sozinho. Use o
   `renv::restore()` do passo 3.
9. **`shiny-server.conf`**: o `app_dir` foi corrigido para
   `.../shiny/core/apps/TUBERCULOSE`. Antes apontava para a raiz do projeto, onde não
   existe `app.R`.
