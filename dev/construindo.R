## 1. Criar o pacote
usethis::create_package("decisionLab")

usethis::use_github()


# Mover suas funções para: decisionLab/R/

## 2. Instalar dependências principais

install.packages(c(
"devtools",
"roxygen2",
"testthat",
"usethis"
))


##3. Ativar documentação automática

usethis::use_roxygen_md()


## 4. Ativar testes
usethis::use_testthat()


usethis::use_test("topsis")
usethis::use_test("spotis")
usethis::use_test("saw")
usethis::use_test("merec_weights")
usethis::use_test("entropy_weights")

## Executar TODOS os testes

devtools::test()

install.packages("rlang") # se necessário


## 1. Adicionar roxygen2

install.packages("roxygen2")

usethis::use_roxygen_md()



#  Começando pelo topsis().

# Colar no início:
#' TOPSIS Method
#'
#' Technique for Order Preference by Similarity to Ideal Solution.
#'
#' @param data Decision matrix.
#' @param weights Numeric vector of criterion weights.
#' @param benefit Logical vector indicating benefit criteria.
#'
#' @return A list containing TOPSIS results.
#'
#' @export

##
devtools::document()

# Fazer para todos

## Testar
?topsis

## Autoria e licença

usethis::use_mit_license()


devtools::document()

## 3. Executar o primeiro check formal do pacote
devtools::check()

#' @keywords internal

devtools::document()

devtools::check()

dir("R", pattern = "\\.R$")

## criar e salvar imediatamente como
# internal_validation.R

# dentro colar:
# .validate_matrix()
# .validate_weights()
# .validate_benefit()

devtools::document()
devtools::check()

## Criar README

usethis::use_readme_rmd()



# No github ---------------------------------------------------------------
## 1. Verificar Git
#     No RStudio console:
usethis::git_sitrep()


# configurar um novo token GitHub;
# inicializar o repositório Git local;
# conectar ao GitHub.


## 1. Criar novo token GitHub
usethis::create_github_token()

gitcreds::gitcreds_set()


usethis::git_sitrep()
# Ok

usethis::use_git()


# devtools::install_github("alessand22/decisionLab")
