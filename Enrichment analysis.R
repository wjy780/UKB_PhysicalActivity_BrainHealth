######################################## enrichment #################################
library(gprofiler2)
p_value_paee<-p_value_proteinfinal$FDR_Q5
index_paee<-which(p_value_paee<0.05)
index_negative<-which(beta_value_proteinfinal$Beta.value5<0)
index_negativeprotein<-intersect(index_paee,index_negative)
protein_paee_n<-beta_value_proteinfinal$protein[index_negativeprotein]

enrich_result_short5 <- gost(query = protein_paee_n,
                             organism = 'hsapiens', ordered_query = FALSE,
                             domain_scope = "custom",custom_bg = beta_value_proteinfinal$protein,
                             multi_query = FALSE, exclude_iea = FALSE,
                             measure_underrepresentation = FALSE, evcodes = FALSE,
                             sources = c('GO','KEGG','REAC','WP'),
                             significant = TRUE, user_threshold = 0.01,correction_method = "fdr")
enrich_result_paee=do.call(cbind, enrich_result_short5$result)