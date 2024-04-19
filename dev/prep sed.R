#prepare sed script to find and replace variable names with codes
sed <- codebook %>% filter(code %in% reference$obs) %>%
  mutate(name = gsub("/", "\\\\/", name)) %>%
  purrr::pmap_chr(.,~paste0("s/", ..2, "/", ..1, "/g"))
#write(sed, "sedsearch.sed")
#run 'find . -type f -name "*.R" | xargs sed -i -f sedsearch.sed' from terminal or git bash
