#' Prism Syntax Highlighter
#'
#' The `prism_highlight_text` function takes a string with a single code snippet
#' and returns an html block with color classes. This html gets colorized by the
#' [prism stylesheet](https://cdnjs.com/libraries/prism) when both are inserted
#' in an HTML document.
#'
#' The functions `prism_highlight_doc` and `prism_highlight_html` work an entire
#' HTML documents, similar to what the client side JavaScript library does. They
#' automatically replace all `<code class="language-xyz">` blocks in the document
#' with highlighted versions.
#'
#' @rdname prismjs
#' @export
#' @param txt string with code that you want to highlight
#' @param language the language that `txt` is in, one of `prism_languages()`.
#' @examples html <- prism_highlight_text('p { color: red }', language = 'css')
#' cat(html)
prism_highlight_text <- function(txt, language = 'r'){
  txt <- paste(txt, collapse = '\n')
  ctx$assign('input', txt)
  ctx$eval(sprintf("prism_highlight(input, '%s')", language))
}

#' @rdname prismjs
#' @export
#' @param doc an [xml2 document][xml2::read_html] that will be modified in place
#' such that all `<code class="language-xyz">` elements are replaced with highlighted html.
prism_highlight_doc <- function(doc){
  lapply(xml2::xml_find_all(doc, "//code[starts-with(@class,'language-')]"), function(x){
    langclass <- tolower(xml2::xml_attr(x, 'class'))
    lang <- sub("^language-", "", langclass)
    input <- trimws(xml2::xml_text(x))
    output <- prism_highlight_text(input, language = lang)
    newnode <- xml2::read_xml(sprintf('<code class="%s">%s</code>', langclass, output))
    parent <- xml2::xml_parent(x)
    print(parent)
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
#' @param input literal html string, connection, or file path, passed to [xml2::read_html]
#' @param output path to file or connection to write to, passed to [xml2::write_html]
#' @param include_css injects the required CSS style file into the html header
#' @param preview opens the generated output html in a browser
prism_highlight_html <- function(input, output = NULL, include_css = FALSE, preview = interactive()){
  doc <- xml2::read_html(input, options = c("RECOVER", "NOERROR"))
  prism_highlight_doc(doc)
  if(include_css){
    head <- xml2::xml_find_first(doc, '//head')
    css <- xml2::read_xml('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/prism/9000.0.1/themes/prism.min.css" />')
    xml2::xml_add_child(head, xml2::xml_root(css))
  }
  xml2::write_html(doc, output)
  if(isTRUE(preview)){
    utils::browseURL(output)
  }
  output
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
