# 🌿 PlantDiary

Diário digital de plantas com CRUD completo, agenda de rega inteligente e
acompanhamento de crescimento — feito em **Haskell**.

> 📈 **Modelo de negócio, estratégia e roadmap:** veja
> [MODELO_DE_NEGOCIO.md](MODELO_DE_NEGOCIO.md).

## Rodar localmente

Pré-requisito: [Stack](https://docs.haskellstack.org/) instalado.

```bash
stack build
stack exec plantdiary
```

Acesse: http://localhost:3000

> Variáveis de ambiente opcionais:
> - `PORT` — porta do servidor (padrão `3000`)
> - `DATABASE_PATH` — caminho do banco SQLite (padrão `plantdiary.db`)

## Estrutura

```
plantdiary/
├── app/Main.hs        # Ponto de entrada (lê PORT e DATABASE_PATH)
├── src/
│   ├── Models.hs      # Modelos (Persistent + SQLite)
│   ├── Routes.hs      # Rotas HTTP (Scotty)
│   └── Views.hs       # Interface (Lucid + Tailwind + Tabler Icons)
├── Dockerfile         # Build multi-stage para deploy
├── render.yaml        # Blueprint do Render
└── plantdiary.cabal   # Dependências
```

## Funcionalidades

### CRUD
- **Plantas** — criar, listar, ver, editar e excluir
- **Registros de cuidado** — rega, adubação, poda, repotagem, observação,
  exposição solar e tratamento

### Diferenciais
- **🌱 Medidor de vitalidade** — o ícone da planta cresce em 5 estágios
  conforme as regas registradas (broto → exuberante), com anel de progresso.
- **💧 Agenda de rega inteligente** — defina de quantos em quantos dias regar;
  o app calcula o status (em dia / regar hoje / atrasada) e destaca no painel
  as plantas que precisam de atenção.
- **🛡️ Limite biológico de rega** — no máximo uma rega por dia por planta,
  evitando o apodrecimento de raízes por excesso de água.
- **🔍 Busca instantânea** — filtra plantas por nome, espécie ou local.
- **📊 Estatísticas do jardim** — total de cuidados, distribuição por tipo e
  ranking das plantas mais cuidadas.
- **📈 Acompanhamento de altura** — registra o porte e classifica o estágio.

## Deploy no Render

O projeto já está pronto para deploy via Docker.

1. Suba o código para um repositório no GitHub.
2. No [Render](https://render.com): **New → Blueprint** e aponte para o repo
   (ele lê o `render.yaml` automaticamente).
   - Ou **New → Web Service**, escolha **Docker** como runtime.
3. Aguarde o build (a primeira compilação Haskell leva ~15–20 min).
4. O Render fornece a URL pública ao final.

> **Persistência dos dados:** no plano gratuito o disco é efêmero — os dados
> reiniciam a cada deploy/restart (ok para demonstração). Para manter os dados,
> use um plano com disco persistente: descomente o bloco `disk` no
> `render.yaml` e troque `DATABASE_PATH` para `/data/plantdiary.db`.

## Stack tecnológica

| Tecnologia | Uso |
|---|---|
| **Scotty** | Framework web / rotas |
| **Lucid** | HTML type-safe |
| **Persistent + SQLite** | ORM e banco de dados |
| **Tailwind CSS** | Estilização (via CDN) |
| **Tabler Icons** | Ícones (via CDN) |
| **Warp** | Servidor HTTP |
| **Docker** | Empacotamento para deploy |
