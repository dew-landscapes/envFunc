
#' Add, commit and push all current changes to github
#'
#' @param comment Character commit comment.
#'
#' @return Side effect of adding, committing and pushing any changes in project.
#' @export
#'
#' @examples
git_commit_env <- function(comment) {

  files <- gert::git_status()$file

  gert::git_add(files)

  gert::git_commit(comment)

  gert::git_push()

}
