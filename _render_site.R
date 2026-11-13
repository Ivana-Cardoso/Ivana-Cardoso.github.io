# Renderiza o biRd blog nos dois idiomas, na ordem certa,
# e atualiza as versões do Quarto e do R no rodapé.
#
# Como usar: abra o projeto no RStudio, abra este arquivo e clique em "Source".
# (O nome começa com "_" para o Quarto não tentar renderizá-lo.)

# Usa o pacote R "quarto" se ele carregar; se não, chama o Quarto direto
# pela linha de comando. Este script NÃO instala nada.
usa_pacote <- requireNamespace("quarto", quietly = TRUE)
if (!usa_pacote) {
  message("Pacote R 'quarto' indisponível; usando o Quarto direto pela linha de comando.")
}

# Renderiza um projeto e espera terminar ("." = raiz do projeto)
renderiza <- function(pasta = ".") {
  if (usa_pacote) {
    if (pasta == ".") quarto::quarto_render(as_job = FALSE)
    else quarto::quarto_render(pasta, as_job = FALSE)
  } else {
    status <- system2("quarto", c("render", pasta))
    if (status != 0) stop("O Quarto terminou com erro ao renderizar '", pasta, "'.")
  }
}

# Confere se estamos na pasta certa (raiz do projeto)
stopifnot(file.exists("_quarto.yml"), file.exists("en/_quarto.yml"))

# 1. Versões atuais do R e do Quarto -------------------------------------
r_ver <- paste(R.version$major, R.version$minor, sep = ".")
quarto_ver <- NA_character_
if (usa_pacote) {
  quarto_ver <- tryCatch(as.character(quarto::quarto_version()),
                         error = function(e) NA_character_)
}
if (is.na(quarto_ver)) quarto_ver <- trimws(system2("quarto", "--version", stdout = TRUE))
message("R: ", r_ver, " | Quarto: ", quarto_ver)

# 2. Atualiza os números no rodapé (só reescreve o arquivo se algo mudou) --
atualiza_rodape <- function(arquivo) {
  linhas <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
  novas <- sub("\\[Quarto v\\.[0-9.]+\\]", paste0("[Quarto v.", quarto_ver, "]"), linhas)
  novas <- sub("\\[R v\\.[0-9.]+\\]", paste0("[R v.", r_ver, "]"), novas)
  if (!identical(linhas, novas)) {
    writeLines(novas, arquivo, useBytes = TRUE)
    message("Rodapé atualizado em ", arquivo)
  }
}
atualiza_rodape("_quarto.yml")
atualiza_rodape("en/_quarto.yml")

# 3. Português primeiro (isso limpa a pasta docs/) ------------------------
renderiza(".")

# Aviso de diagnóstico: depois do render do português, docs/en não deveria existir
if (dir.exists(file.path("docs", "en"))) {
  message("ATENÇÃO: o render do português já criou docs/en. ",
          "Isso indica que o Quarto renderizou os arquivos de en/ no projeto principal.")
}

# 4. Inglês depois: renderiza em en/_site e copia para docs/en -----------
renderiza("en")

destino <- file.path("docs", "en")
if (dir.exists(destino)) unlink(destino, recursive = TRUE, force = TRUE)
if (dir.exists(destino)) {
  stop("Não consegui apagar docs/en. Feche o navegador e pause o OneDrive, e tente de novo.")
}
dir.create(destino, recursive = TRUE)

copiado <- file.copy(
  list.files("en/_site", full.names = TRUE, all.files = TRUE, no.. = TRUE),
  destino,
  recursive = TRUE,
  overwrite = TRUE
)
if (!all(copiado)) stop("A cópia de en/_site para docs/en falhou em algum arquivo.")

message("Pronto! Site em português em docs/ e em inglês em docs/en/.")