# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   04:35 PM Monday, 22 May 2017
# * Copyright: AS IS

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

