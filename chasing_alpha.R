# Accompanying code for "Chasing Alpha" presentation
# Author: David Schemitsch

library(ggplot2)
library(glue)
library(httr)
library(data.table)
library(stringr)
library(humaniformat)
library(stringi)
library(xml2)
library(rvest)

# Accessing EDGAR Data general info: https://www.sec.gov/os/accessing-edgar-data

# Parse CIK lookup file ----
suffixes = 'JR$|JR\\.$|SR$|SR\\.$|II$|III$|IV$|ESQ$|ESQ\\.$|\\sMD$'

ciks = readLines('https://www.sec.gov/Archives/edgar/cik-lookup-data.txt')
ciks = data.table(ciks)
setnames(ciks, "original")
ciks[, cik := str_match(string = original, ":(\\d{10}):$")[, 2]]
ciks[, entity := str_sub(original, 1, str_locate(original, cik)[,1]-2)]
ciks[, last_name := str_extract(entity, pattern = '^\\S+')]
ciks[, first_middle_name := str_trim(str_sub(entity, nchar(last_name)+1))]
ciks[, first_name := str_extract(first_middle_name, pattern = '^\\S+')]
ciks[, middle_name := str_trim(str_sub(first_middle_name, nchar(first_name)+1))]
ciks[, suffix := str_extract(middle_name, suffixes)]
ciks[, middle_name := str_trim(str_remove_all(middle_name, suffixes))]

# Row for Jeff Bezos
ciks[cik == '0001043298', .(original, cik, first_name, middle_name, last_name, suffix)]

# API options ----
CIK = '0001661964'
type = '4'
before = ''
count = 20
page = 1


# 3 Approaches for assembling links to insiders' filings ----

# Approach 1: Use API ----

# User Agent declaration
get_sec <- function(url, ua){
    
    res <- httr::GET(url = url, 
                     httr::user_agent(ua),
                     add_headers("user-agent" = ua))
    
    if (res$status == 200){
        # Use read_xml or read_html based on the url
        if (str_detect(string = url, pattern = regex('xml$', ignore_case = TRUE))){
            doc <- xml2::read_xml(res)
        } else {
            doc <- xml2::read_html(res, base_url = url, options = "HUGE")    
        }
    } else if (res$status == "403") {
        stop(glue("403 Error: Too many requests. Try waiting a few minutes and restart the search."))
    } else if (res$status != "200" | res$headers["content-type"] != "application/atom+xml") {
        stop(paste0("Error: Could not find company: ", issuer_cik))
    }
    
    return(doc)
    
}

# User Agent string construction 
my_email_address = readline(prompt = 'enter email address: ')
my_organization = readline(prompt = 'enter organization name: ')
ua = paste0(my_email_address, " (", my_organization, ")")

print(ua)

get_docs = function(CIK_list, type, before, page, count, ua){
    
    doc_list = list()
    
    for (i in CIK_list){
        message(glue('CIK {i}'))
        Sys.sleep(5)
        
        # Source: https://github.com/mwaldstein/edgarWebR/blob/fb9a38e6a57186ffd1c93cc1aa00c4fdf1bc5514/R/browse_edgar.R#L23
        href <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany",
                       "&CIK=", URLencode(as.character(i), reserved = TRUE),
                       "&type=", URLencode(as.character(type), reserved = TRUE),
                       "&dateb=", before,
                       "&start=", (page - 1) * count,
                       "&count=", count,
                       "&output=atom")
        
        doc <- get_sec(url = href, ua = ua)
        doc <- xml_ns_strip(doc)
        
        insider = xml2::xml_text(xml2::xml_find_first(x = doc, ".//company-info//conformed-name"))
        insider = ifelse(is.na(insider), "ERROR", insider)
        
        entries = xml_find_all(doc, ".//entry")
        filing_hrefs = xml_text(xml_find_all(entries, "//content//filing-href"))
        filing_type = xml_text(xml_find_all(entries, "//content//filing-type"))
        form_name = xml_text(xml_find_all(entries, "//content//form-name"))
        date_filed = xml_text(xml_find_all(entries, "//content//filing-date"))
        
        docs = data.table(CIK = i,
                          insider = insider,
                          filing_hrefs = filing_hrefs,
                          filing_type = filing_type,
                          form_name = form_name,
                          date_filed = date_filed)
        doc_list[[length(doc_list) + 1]] = docs
    }
    
    doc_df = rbindlist(doc_list)
    
    return(doc_df)
    
}

# Specify form type in `type`
# Get Form 4 docs
doc_df_form4 = get_docs(CIK_list = c('1661964', '1183818'), 
                        type = '4', before = '', page = 1, count = 10,
                        ua = ua)
print(head(doc_df_form4))

# Get SC 13G/A docs
doc_df_sc13 = get_docs(CIK_list = c('1548760'), 
                       type = 'SC 13G/A', before = '', page = 1, count = 10,
                       ua = ua)
print(head(doc_df_sc13))

# Approach 1B: Use API ----
format_insider_filings <- function(input_cik){
    
    cik_info = jsonlite::read_json(glue('https://data.sec.gov/submissions/CIK{input_cik}.json'))
    
    filings_dt = data.table()
    for (col in names(cik_info$filings$recent)){
        set(x = filings_dt, j = str_to_lower(col), value = unlist(cik_info$filings$recent[[col]]))
    }
    
    filings_dt[, filing_detail_url := paste0('https://www.sec.gov/Archives/edgar/data/',
                                             input_cik, '/',
                                             str_remove_all(string = accessionnumber, pattern = '\\-'), '/',
                                             primarydocument)]
    
    filings_dt[, url := paste0('https://www.sec.gov/Archives/edgar/data/',
                               input_cik, '/',
                               str_remove_all(string = accessionnumber, pattern = '\\-'), '/',
                               str_remove(string = primarydocument, pattern = '^\\w+/'))]
    
    filings_dt[, txt_doc := paste0('https://www.sec.gov/Archives/edgar/data/',
                                   input_cik, '/',
                                   str_remove_all(string = accessionnumber, pattern = '\\-'), '/',
                                   accessionnumber, ".txt")]
    
    return(filings_dt)
}

filings = format_insider_filings(input_cik = '0001693709')

print(head(filings))

# Approach 2: Directory Crawling ----
get_direcory_listing <- function(input_cik, ua) {
    
    # Create directory listing URL for based on insiders CIK. 
    directory_listing_url <- paste0("https://www.sec.gov/Archives/edgar/data/", input_cik)
    
    # Read html of URL. Error is not valid URL.
    # read_html(directory_listing_url) returns an xml document of length two, which 
    # would make the try statement return two booleans. Use any() to avoid warning about 
    # the "condition has length > 1".
    
    read_directory_listing_url <- get_sec(url = directory_listing_url, ua = ua)
    
    # if (any(class(try(read_html(directory_listing_url), silent = TRUE)) != "try-error")){
    if (any(class(try(read_directory_listing_url, silent = TRUE)) != "try-error")){
        message("Valid directory")
    } else {
        stop("Not a valid URL")
    }
    
    # children of the table node that contain "href"
    xml_table <- xml_children(xml_find_first(read_directory_listing_url, './/table'))[xml_children(
        xml_find_first(read_directory_listing_url, './/table')) %like% "href"]
    
    # Children of the above section
    xml_table <- xml_children(xml_table)[xml_children(xml_table) %like% "href"]
    
    # create list of filing links by extracting the attributes from the href nodes
    
    filing_links <- xml_attr(xml_children(xml_table),"href")
    filing_links <- paste0("https://www.sec.gov", filing_links)
    
    return(filing_links)
    
}

input_cik = '0001661964'
directories = get_direcory_listing(input_cik, ua)

# This returns all of the valid document sub directories 
# (CIK + AccessionNumber)
directories[1:5]


# Approach 3: Bulk Download of daily filings text file ----
date_i = as.Date('2021-06-02')
day_i = lubridate::day(date_i)
month_i = lubridate::month(date_i)
quarter_i = lubridate::quarter(date_i)
year_i = lubridate::year(date_i)

# Create the URL
url_i = paste0("https://www.sec.gov/Archives/edgar/daily-index/", year_i, 
               "/QTR", quarter_i, 
               "/form.", year_i, 
               str_pad(month_i, 2, 'left', 0), 
               str_pad(day_i, 2, 'left', 0), 
               ".idx")

form_txt_i = fread(input=url_i, header=FALSE, skip=11, fill = TRUE, sep = '\n')

process_form_txt_i = function(form_txt_i){
    
    form_txt_i[, form_type := str_trim(str_sub(V1, 1, 12))]
    form_txt_i[, cik := as.integer(str_trim(str_sub(V1, 75, 86)))]
    form_txt_i[, date_filed := str_trim(str_sub(V1, 87, 98))]
    form_txt_i[, date_filed := as.Date(date_filed, '%Y%m%d')]
    form_txt_i[, file_name := str_trim(str_sub(V1, 99, nchar(V1)))]
    form_txt_i[, filing_dir := paste0('https://www.sec.gov/Archives/',file_name)]
    form_txt_i[, filing_dir := str_remove_all(filing_dir, "\\-|\\.txt")]
    
    form_txt_i[, V1 := NULL]
    
    return(form_txt_i)
    
}

daily_filings = process_form_txt_i(form_txt_i = form_txt_i)
print(head(daily_filings))


# XML parsing example ----
doc = xml2::read_xml('https://www.sec.gov/Archives/edgar/data/1018724/000112760221017247/form4.xml')

# reportingOwner node and its child nodes
xml_find_first(doc, ".//reportingOwner")

# Child nodes of reportingOwner
xml_children(xml_find_first(doc, ".//reportingOwner"))

# Navigate to a specific node
xml_find_all(doc, ".//rptOwnerName")

# Select text directly
xml_text(xml_find_all(doc, ".//rptOwnerName"))

# Select nonderivative transaction nodes
url = 'https://www.sec.gov/Archives/edgar/data/0001513142/000162643119000014/edgar.xml'
doc = xml2::read_xml(url)
nd_transactions = xml_children(xml_find_all(doc, ".//nonDerivativeTable"))
print(nd_transactions)

# Transform Form 4 ----
process_form4 = function(filing_doc_row, ua){
    
    # Basic Form 4 conversion function
    
    url <- filing_doc_row$url
    form <- filing_doc_row$form
    doc_id  <- filing_doc_row$accessionnumber
    filing_detail_url <- filing_doc_row$filing_detail_url
    text_url <- filing_doc_row$txt_doc
    
    if (!(str_to_lower(url) %like% "xml$") & (form %in% c('3','4','5'))){
        message(glue("Skipping document {i} of {nrow(filing_doc_row)} (Document {doc_id} is type {form} and not XML.)"))
        next
    }
    
    read_xml_link <- get_sec(url = url, ua = ua)
    
    results_l = list()
    
    if (form %in% c('3','4','5')){
        issuer <- str_to_upper(xml_text(xml_find_first(read_xml_link, ".//issuerName")))
        issuer_CIK <- str_to_upper(xml_text(xml_find_first(read_xml_link, ".//issuerCik")))
        schema <- str_to_upper(xml_text(xml_find_first(read_xml_link, ".//schemaVersion")))
        
        # If this variable doesn't equal the CIK input, it probably means there are more than one beneficial owner,
        # and that the insider is going to disclaim beneficial ownership. 
        # Flag these rows and exclude them initially in the sales summaries (multiple_reporting_owners)
        reporting_owner_cik_count <- length(str_to_upper(xml_text(xml_find_all(read_xml_link, ".//rptOwnerCik"))))
        transaction_price_node <- switch (schema,
                                          X0101 = ".//transactionValue",
                                          X0201 = ".//transactionPricePerShare",
                                          X0202 = ".//transactionPricePerShare",
                                          X0203 = ".//transactionPricePerShare",
                                          X0204 = ".//transactionPricePerShare",
                                          X0205 = ".//transactionPricePerShare",
                                          X0206 = ".//transactionPricePerShare",
                                          X0301 = ".//transactionPricePerShare",
                                          X0302 = ".//transactionPricePerShare",
                                          X0303 = ".//transactionPricePerShare",
                                          X0304 = ".//transactionPricePerShare",
                                          X0305 = ".//transactionPricePerShare",
                                          X0306 = ".//transactionPricePerShare",
                                          ".//transactionPricePerShare" # default value
        )
        
        issuer_ticker <- str_to_upper(xml_text(xml_find_first(read_xml_link, ".//issuerTradingSymbol")))
        doc_type <- xml_text(xml_find_first(read_xml_link, ".//documentType"))
        period_of_report <- xml_text(xml_find_first(read_xml_link, ".//periodOfReport"))
        
        # Footnotes table ----
        footnotes <- data.table(note = str_to_upper(xml_text(xml_children(xml_find_first(
            read_xml_link, ".//footnotes")))))
        footnotes[, note_id := paste0("F",seq_len(.N))]
        footnotes[, disclaims_beneficial_ownership := note %like% "DISCLAIM.*BENEFICIAL"]
        
        # Idntify which nodes are non-derivative vs. derivative
        all_non_der_nodes <- xml_children(xml_find_all(read_xml_link, ".//nonDerivativeTable"))
        
        if(length(all_non_der_nodes) == 0){
            all_non_der_nodes <- xml_find_all(read_xml_link, ".//nonDerivativeSecurity")
        }
        
        non_der_l = list()
        
        for (i in seq(length(all_non_der_nodes))){
            non_der_row = data.table(security_title = xml_text(xml_find_first(all_non_der_nodes[i], ".//securityTitle")),
                                     transaction_date = xml_text(xml_find_first(all_non_der_nodes[i], ".//transationDate")),
                                     form_type = xml_text(xml_find_first(all_non_der_nodes[i], ".//transactionFormType")),
                                     shares_count = xml_text(xml_find_first(all_non_der_nodes[i], ".//transactionShares")),
                                     share_price = xml_text(xml_find_first(all_non_der_nodes[i], ".//transactionPricePerShare")),
                                     acq_or_disp = xml_text(xml_find_first(all_non_der_nodes[i], ".//transactionAcquiredDisposedCode")),
                                     ownership_nature = xml_text(xml_find_first(all_non_der_nodes[i], ".//directOrIndirectOwnership")),
                                     shares_owned_post_transaction = xml_text(xml_find_first(all_non_der_nodes[i], ".//sharesOwnedFollowingTransaction")),
                                     security_type = 'nonDerivative'
            )
            non_der_l[[length(non_der_l) + 1]] = non_der_row
        }
        
        if (length(non_der_l) > 0){
            non_der_df = rbindlist(non_der_l)
            results_l[['non_der_df']] = non_der_df
        }
        
        der_l = list()
        
        all_der_nodes <- xml_children(xml_find_all(read_xml_link, ".//derivativeTable"))
        
        if(length(all_der_nodes) == 0){
            all_der_nodes <- xml_find_all(read_xml_link, ".//derivativeSecurity")
        }
        
        for (i in seq(length(all_der_nodes))){
            der_row = data.table(security_title = xml_text(xml_find_first(all_der_nodes[i], ".//securityTitle")),
                                 conversion_price = xml_text(xml_find_first(all_der_nodes[i], ".//conversionOrExercisePrice")),
                                 transaction_date = xml_text(xml_find_first(all_der_nodes[i], ".//transactionDate")),
                                 shares_count = xml_text(xml_find_first(all_der_nodes[i], ".//transactionShares")),
                                 share_price = xml_text(xml_find_first(all_der_nodes[i], ".//transactionPricePerShare")),
                                 acq_or_disp = xml_text(xml_find_first(all_der_nodes[i], ".//transactionAcquiredDisposedCode")),
                                 exercise_date = xml_text(xml_find_first(all_der_nodes[i], ".//exerciseDate")),
                                 expiration_date = xml_text(xml_find_first(all_der_nodes[i], ".//expirationDate")),
                                 shares_owned_post_transaction = xml_text(xml_find_first(all_der_nodes[i], ".//sharesOwnedFollowingTransaction")),
                                 security_type = 'derivative'
            )
            der_l[[length(der_l) + 1]] = der_row
        }
        
        if (length(der_l) > 0){
            der_df = rbindlist(der_l)
            results_l[['der_df']] = der_df
        }
        
        results_df = rbindlist(results_l, fill = TRUE)
        results_df[, issuer := issuer]
        results_df[, issuer_CIK := issuer_CIK]
        results_df[, issuer_ticker := issuer_ticker]
        results_df[, schema := schema]
        results_df[, doc_type := doc_type]
        results_df[, period_of_report := period_of_report]
        
        return(results_df)
    }
}

# Get the filings for CIK 0001693709
filings = format_insider_filings(input_cik = '0001693709')

# Process one Form 4
form4 = process_form4(filing_doc_row = filings[1], ua = ua)
print(form4)

# SC13 ----
sc13 = read_html('https://www.sec.gov/Archives/edgar/data/0001548760/000119312519040765/d670490dsc13ga.htm')
sc13_tables = xml_find_all(sc13, "//table")
ownership_table_raw <- sc13_tables[grepl(x = as.character(sc13_tables), 
                                         pattern = "SHARED.*POWER|SOLE.*POWER",
                                         ignore.case = TRUE)][1]
ownership_table <- ownership_table_raw[[1]] %>% rvest::html_table(header = FALSE,
                                                                  trim = FALSE,
                                                                  fill = TRUE)

ownership_table <- t(unique(t(ownership_table)))

l = list()

for (c in seq(ncol(ownership_table))){
    l[[length(l) + 1]] = data.table(ownership_table[, c])
}

shares_df = rbindlist(l)
shares_df = shares_df[V1 %like% "SHARED.*POWER|SOLE.*POWER"]
shares_df[, sc13_shares_category := str_extract(shares_df$V1, 
                                                "SHARED.*POWER|SOLE.*POWER")]
shares_df[, sc13_shares_count := str_trim(str_extract(
    shares_df$V1, "(\\s|^)\\d+(,\\d+)*(\\s|$)"))]
shares_df[, sc13_shares_count := as.numeric(
    str_remove_all(sc13_shares_count, ","))]
shares_df[, V1 := NULL]
print(shares_df)
