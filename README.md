# prismjs

Server-side syntax highlighting in R using [prism.js](https://prismjs.com)

```r
# Download and install prismjs in R
install.packages('prismjs', 
  repos = c('https://ropensci.r-universe.dev','https://cloud.r-project.org'))
```

## Example

Highlight a single code snippet:

```r
library(prismjs)
input <- "p { color: red }"
html <- prism_highlight_text(input, language = 'css')
cat(html)
## <span class="token selector">p</span> <span class="token punctuation">{</span> <span class="token property">color</span><span class="token punctuation">:</span> <span class="token color">red</span> <span class="token punctuation">}</span>
```

Or process an entire document:

```r
prism_highlight_document("input.html", output = "output.html")
