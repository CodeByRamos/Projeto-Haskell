# ─── Etapa 1: build ──────────────────────────────────────────────────────────
FROM haskell:9.6.6 AS build

WORKDIR /app

# Copia apenas os arquivos de configuração primeiro (cache de dependências)
COPY stack.yaml plantdiary.cabal ./
RUN stack setup --system-ghc && \
    stack build --system-ghc --no-install-ghc --only-dependencies

# Copia o código e compila o executável
COPY . .
RUN stack install --system-ghc --no-install-ghc --local-bin-path /app/bin

# ─── Etapa 2: runtime (imagem enxuta) ─────────────────────────────────────────
FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends libgmp10 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /app/bin/plantdiary /app/plantdiary

# Render injeta a porta via $PORT; o app já lê dela.
ENV PORT=10000
EXPOSE 10000

CMD ["/app/plantdiary"]
