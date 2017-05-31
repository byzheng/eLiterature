# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   04:35 PM Monday, 22 May 2017
# * Copyright: AS IS

#' Convert a html/htm file into ebook formats which are supported by ebook-convert in calibre
#'
#' @param file A html/html file
#' @param output A output file with file extension supported by ebook-convert
#' @param publisher A publisher. The supported publisher is elsevier, oup and springer.
#' @export
convert_ebook <- function(file, output, publisher) {
    library(xml2)
    x <- read_html(file)
    n <- eval(parse(text = paste0('parse_', publisher, '(x)')))
    tmp_file <- tempfile(fileext = '.html')
    write_html(n, tmp_file)
    cmd <- paste0('ebook-convert ', tmp_file, ' ', output)
    system(cmd)
    file.remove(tmp_file)
}


parse_oup <- function(x) {
    title <- xml_find_first(x, '//h1[@class="wi-article-title article-title-main"]')
    title <- stringr::str_trim(xml_text(title))
    authors <- xml_find_all(x, '//div[@class="al-author-name"]/a[@class="linked-name"]')
    authors <- xml_text(authors)
    pub_info <- xml_find_first(x, '//div[@class="ww-citation-primary"]')
    article <- xml_find_first(x, '//div[@class="article-body"]')
    xml_find_all(article, '//div[@class="fig fig-modal reveal-modal"]') %>% xml_remove()
    xml_find_all(article, '//a[@class="fig-view-orig"]') %>% xml_remove()
    xml_find_all(article, '//a[@class="download-slide"]') %>% xml_remove()
    xml_find_all(article, '//h2[@class="backreferences-title"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="ref-list"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="copyright copyright-statement"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="downloadImagesppt"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="comments"]') %>% xml_remove()


    n <- xml_new_root('html')
    xml_add_child(n, 'head')
    head <- xml_find_first(n, 'head')
    xml_add_child(head, 'title', title)
    for (i in seq(along = authors)) {
        xml_add_child(head, 'meta',
                      name = 'author',
                      content = authors[i])
    }

    xml_add_child(n, 'body')
    body <- xml_find_first(n, '//body')
    xml_add_child(body, 'h1', title)
    n_authors <- xml_add_child(body, 'div')
    for (i in seq(along = authors)) {
        xml_add_child(n_authors, 'strong', authors[i])
    }
    xml_add_child(body, pub_info)
    xml_add_child(body, article)
    n

}




parse_springer <- function(x) {
    library(dplyr)
    title <- xml_find_first(x, '//meta[@name="citation_title"]') %>% xml_attr('content')

    authors <- xml_find_all(x, '//meta[@name="citation_author"]') %>% xml_attr('content')

    journal <- xml_find_first(x, '//meta[@name="citation_journal_title"]') %>% xml_attr('content')

    pub_info <- journal
    article <- xml_find_first(x, '//div[@class="main-container uptodate-recommendations-on"]')

    xml_find_all(article, '//aside[@class="main-sidebar-left"]') %>% xml_remove()
    xml_find_all(article, '//aside[@class="main-sidebar-right u-interface"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="sticky-banner u-interface"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="cta-button-container u-hide-two-col"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="enumeration"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="MainTitleSection"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="authors u-clearfix authors--enhanced"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="ArticleHeader main-context"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="HeaderArticleNotes"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="article-actions--inline"]') %>% xml_remove()
    xml_find_all(article, '//span[@class="u-screenreader-only"]') %>% xml_remove()
    xml_find_all(article, '//h2[@id="copyrightInformation"]') %>% xml_remove()
    xml_find_all(article, '//aside[@class="content-type-about"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="collapsible-section uptodate-recommendations gtm-recommendations"]') %>% xml_remove()
    xml_find_all(article, '//span[@class="Occurrences"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="ArticleCopyright"]') %>% xml_remove()

    n <- xml_new_root('html')
    xml_add_child(n, 'head')
    head <- xml_find_first(n, 'head')
    xml_add_child(head, 'title', title)
    for (i in seq(along = authors)) {
        xml_add_child(head, 'meta',
                      name = 'author',
                      content = authors[i])
    }

    xml_add_child(n, 'body')
    body <- xml_find_first(n, '//body')
    xml_add_child(body, 'h1', title)
    n_authors <- xml_add_child(body, 'div')
    for (i in seq(along = authors)) {
        xml_add_child(n_authors, 'strong', authors[i])
    }
    xml_add_child(body, 'p', pub_info)
    xml_add_child(body, article)
    n
}



parse_elsevier <- function(x) {
    library(dplyr)
    # x <- read_html(file)

    title <- xml_find_first(x, '//title') %>% xml_text() %>% stringr::str_trim()

    authors <- xml_find_all(x, '//a[@class="authorName svAuthor"]') %>% xml_text()

    pub_info <- xml_find_first(x, '//li[@class="fullSrcTitle"]') %>% xml_text()

    article <- xml_find_first(x, '//div[@class="centerInner svBigBox"]')

    xml_find_all(article, '//style[@media="screen"]') %>% xml_remove()
    xml_find_all(article, '//div[@style="display:none"]') %>% xml_remove()
    xml_find_all(article, '//noscript') %>% xml_remove()
    xml_find_all(article, '//div[@class="publicationHead"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="page_fragment auth_frag"]') %>% xml_remove()
    xml_find_all(article, '//dt[@class="label"]') %>% xml_remove()
    xml_find_all(article, '//dd[@class="fullsizeTable"]') %>% xml_remove()
    xml_find_all(article, '//dd[@class="menuButtonLinks"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="mathjax"]') %>% xml_remove()
    xml_find_all(article, '//div[@class="refText svRefs"]') %>% xml_remove()
    xml_find_all(article, '//h2[contains(text(), "References")]') %>% xml_remove()

    xml_find_all(article, '//div[contains(@class,"publicationCover")]') %>% xml_remove()
    n <- xml_new_root('html')
    xml_add_child(n, 'head')
    head <- xml_find_first(n, 'head')
    xml_add_child(head, 'title', title)
    for (i in seq(along = authors)) {
        xml_add_child(head, 'meta',
                      name = 'author',
                      content = authors[i])
    }

    xml_add_child(n, 'body')
    body <- xml_find_first(n, '//body')
    xml_add_child(body, 'h1', title)
    n_authors <- xml_add_child(body, 'div')
    for (i in seq(along = authors)) {
        xml_add_child(n_authors, 'strong', authors[i])
    }
    xml_add_child(body, 'p', pub_info)
    xml_add_child(body, article)
    # write_html(n, 'tmp.html')
    n
}
