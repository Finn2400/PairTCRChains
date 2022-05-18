#PairTCRChains


library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)



############################################################################
################################ place to edit #############################

# current max 10 of each chain
num_dict = list("1" = 'One', '2' = 'Two', '3' = 'Thr', '4' = 'Fou', '5' = 'Fiv',
                '6' = 'Six', '7' = 'Sev', '8' = 'Eig', '9' = 'Nin', '10'= "Ten")

peptide_dict = list("G1" = 'SLFWGEPAV', 'G3' = 'SLFWNEPAI')

hla = 'HLA00005'

TCR_group <- "TCR2"

################################ place to edit #############################
############################################################################


TCR_group = "TCR2"
# max 10  of each chain
#beta first for everything

# needed bc data is stored on two pages 
multiplesheets <- function(fname) {
# importing required packages
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

# specifying the path name
path <- "/Users/mallikagupta/Desktop/Gfg.xlsx"
x = multiplesheets('C:/Users/Finnl/OneDrive/Desktop/Research/Batch1_w_all_models/HEV_Batch1_TCR2.xlsx')


TCR_beta = x$TCR2B
TCR_alpha =x$TCR2A
TCR_beta
TCR_alpha



TCR_beta <- tibble::rowid_to_column(TCR_beta, "index")
TCR_alpha <- tibble::rowid_to_column(TCR_alpha, "index")


#cool trick but no longer needed
# TCR_beta <- TCR_beta %>% rename(cdr3_beta =`CDR3 amino acid` ) # using backward ticks to deal with white space 
# TCR_alpha <- TCR_alpha %>% rename(cdr3_alpha =`CDR3 amino acid` ) 


TCR_beta
TCR_alpha

TCR_beta_names <- c('index','tcr_group_chain', 'cdr3_beta', 'v_and_d_beta', 'd_beta', 'j_beta', "freq_%")
TCR_alpha_names <- c('index','tcr_group_chain', 'cdr3_alpha', 'v_and_d_alpha', 'd_alpha', 'j_alpha', "freq_%")

TCR_alpha_names
names(TCR_beta) <- TCR_beta_names
names(TCR_alpha) <- TCR_alpha_names


TCR_beta %>% mutate(tcr_group = "TCR2") -> TCR_beta
TCR_alpha %>% mutate(tcr_group = "TCR2") -> TCR_alpha

TCR_beta
TCR_alpha


# making into two item problem makes using expand.grid easier (possible?) to combine to make do pairwise expansion  
TCR_beta <-within(TCR_beta, combined_betas <- paste(index, cdr3_beta, v_and_d_beta, j_beta, sep=','))
TCR_alpha <-within(TCR_alpha, combined_alphas <- paste(index, cdr3_alpha, v_and_d_alpha, j_alpha, sep=','))

TCR_beta
TCR_alpha


TCR_beta_names
TCR_alpha_names


combined <-  expand.grid(genotype =c('G1','G3'),
                         combined_betas = TCR_beta$combined_betas, 
                         combined_alphas = TCR_alpha$combined_alphas) 

head(combined)



#collapse last two cols
combined %>% unite(united, genotype, combined_betas, combined_alphas,
                   sep = ',') -> combined

# csv, strange but it seems to work
out = cbind(combined, read.csv(text = combined$united, header = FALSE))


out <- out[,-1] # remove old combined thing 
head(out)



header_names<- c('genotype',
                 'index_beta', 'cdr3_beta', 'v_and_d_beta', 'j_beta',
                 'index_alpha','cdr3_alpha', 'v_and_d_alpha', 'j_alpha')

names(out) <- header_names 

head(out)




# num to char ex: 1 -> "One"
for (i in 1:10){out <- replace(out, out == names(num_dict[i]), num_dict[i])} 


finished_df <- out %>%
  mutate(TCR_group = TCR_group) %>%
  mutate(imgt_hla = hla) %>% 
  mutate(peptide = genotype) %>% 
  unite(tcr_name, c("TCR_group", "index_beta", "index_alpha", "genotype"), sep = "")


finished_df

# genotype to seq ex G1 -> SLFWGEPAV
for (i in 1:10){out <- replace(out, out == names(peptide_dict[i]), peptide_dict[i])}



head(finished_df)

col_order <- c("tcr_name", "cdr3_alpha", "cdr3_beta",
               "v_and_d_alpha", "j_alpha", "v_and_d_beta", "j_beta", "peptide","imgt_hla")
finished_df <- finished_df[, col_order]

head(finished_df)

view(finished_df)



#   \o/
library("writexl")

# df.to_excel(r'C:\Users\Finnl\OneDrive\Desktop\Research\Batch1_w_all_models\HEV_Batch1_TCR2_paired.xlsx', index = False)
write_xlsx(finished_df,"C:/Users/Finnl/OneDrive/Desktop/Research/Batch1_w_all_models\\HEV_Batch1_TCR2_paired.xlsx")


