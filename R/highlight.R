#' Prism Syntax Highlighter
#'
#' The `prism_highlight_text` function takes a string with a single code snippet
#' and returns an html fragment with syntax classes. This html gets colorized by
#' the [prism stylesheet](https://cdnjs.com/libraries/prism) when both are inserted
#' in an HTML document.
#'
#' The function `prism_highlight_document` processes an entire HTML document,
#' similar to how PrismJS works in a browser. It automatically finds all
#' `<code class="language-xyz">` elements in the document and substitutes these
#' with highlighted html elements. Again, CSS is needed to actually colorize the
#' html, you can use `include_css` to automatically inject the CSS in the html
#' header if your input document does not have this yet.
#'
#' @name prismjs
#' @rdname prismjs
#' @export
#' @param txt string with code that you want to highlight
#' @param language the language that `txt` is in, one of `prism_languages()`.
#' @return html with classes that can be colorized using a prims stylesheet
#' @examples html <- prism_highlight_text('p { color: red }', language = 'css')
#' cat(html)
prism_highlight_text <- function(txt, language = 'r'){
  txt <- paste(txt, collapse = '\n')
  ctx$assign('input', txt)
  ctx$eval(sprintf("prism_highlight(input, '%s')", language))
}

#' @rdname prismjs
#' @export
#' @param input literal html string, connection, or file path, passed to [xml2::read_html]
#' @param output path to file or connection to write to, passed to [xml2::write_html]. Set
#' `NULL` to return the entire output document as a character string.
#' @param include_css insert the Prism css style (with the default theme) into the html header.
#' @param preview opens the generated output html in a browser
prism_highlight_document <- function(input, output = NULL, include_css = FALSE, preview = interactive()){
  doc <- xml2::read_html(input, options = c("RECOVER", "NOERROR"))
  prism_process_xmldoc(doc)
  if(include_css){
    head <- xml2::xml_find_first(doc, '//head')
    css <- xml2::read_xml('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/prism/9000.0.1/themes/prism.min.css" />')
    xml2::xml_add_child(head, xml2::xml_root(css))
  }
  if(!length(output)){
    return(as.character(doc))
  }
  xml2::write_html(doc, output)
  if(isTRUE(preview)){
    utils::browseURL(output)
  }
  output
}

#' @rdname prismjs
#' @export
#' @param doc an [xml2 document][xml2::read_html] that will be modified in place
#' such that all `<code class="language-xyz">` elements are replaced with highlighted html.
prism_process_xmldoc <- function(doc){
  supported <- prism_languages()
  lapply(xml2::xml_find_all(doc, "//code[starts-with(@class,'language-')]"), function(x){
    langclass <- tolower(xml2::xml_attr(x, 'class'))
    lang <- sub("^language-", "", langclass)
    if(!lang %in% supported){
      warning("Skipping unsupported language by PrismJS: ", langclass, call. = FALSE)
      return()
    }
    input <- trimws(xml2::xml_text(x))
    output <- prism_highlight_text(input, language = lang)
    newnode <- xml2::read_xml(sprintf('<code class="%s">%s</code>', langclass, output), options = c("RECOVER", "NOERROR", "NOBLANKS"))
    parent <- xml2::xml_parent(x)
    if(xml2::xml_name(parent) == 'pre'){
      if(xml2::xml_has_attr(parent, 'class')){
        xml2::xml_set_attr(parent, 'class', paste(xml2::xml_attr(parent, 'class'), langclass))
      } else {
        xml2::xml_set_attr(parent, 'class', langclass)
      }
    }
    xml2::xml_replace(x, xml2::xml_root(newnode))
  })
  return(doc)
}

#' @rdname prismjs
#' @export
#' @examples prism_languages()
prism_languages <- function(){
  ctx$call('prism_languages')
}

#' @importFrom V8 v8 JS
.onLoad <- function(lib, pkg){
  assign("ctx", V8::v8("window"), environment(.onLoad))
  ctx$source(system.file("js/prism.js", package = pkg))
  ctx$source(system.file("js/bindings.js", package = pkg))
}
