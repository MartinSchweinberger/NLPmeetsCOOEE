# function to calculate significant co-occurrences
# requires a data frame with 
# Target: frequency of term in target
# NonTarget: frequency of term in NonTarget
# TotalTarget: frequency of all terms in Target
# TotalNonTarget: frequency of all terms in NonTarget
# NRows: Number of rows in data frame
CoocStatzFisher <- function(data){
  data <- data %>%
  dplyr::mutate(a = Target,
                b = NonTarget,
                c = TotalTarget-Target,
                d = TotalNonTarget-NonTarget) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(a, b, c, d), 
                                                        ncol = 2, byrow = T), 
                                                 simulate.p.value=F)[1]))) %>%
  dplyr::mutate(x2 = as.vector(unlist(chisq.test(matrix(c(a, b, c, d), 
                                                        ncol = 2, byrow = T), 
                                                 simulate.p.value=F)[1]))) %>%
  dplyr::mutate(phi = sqrt((x2/(a + b + c + d)))) %>%
  dplyr::mutate(RateTarget = round(Target/TotalTarget, 4),
                RateNonTarget = round(NonTarget/TotalNonTarget, 4)) %>%
  dplyr::mutate(Type = ifelse(RateTarget > RateNonTarget, "Overuse", "Underuse")) %>%
  dplyr::mutate(Significance = ifelse(p <= .001, "p<.001",
                               ifelse(p <= .01, "p<.01",
                               ifelse(p <= .05, "p<.05", "n.s.")))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(p) %>%
  dplyr::mutate(j = 1:n()) %>%
  # perform benjamini-holm correction
  dplyr::mutate(corr05 = ((j/NRows)*0.05)) %>%
  dplyr::mutate(corr01 = ((j/NRows)*0.01)) %>%
  dplyr::mutate(corr001 = ((j/NRows)*0.001)) %>%
  # calculate corrected significance status
  dplyr::mutate(CorrSignificance = ifelse(p <= corr001, "p<.001",
                                          ifelse(p <= corr01, "p<.01",
                                                 ifelse(p <= corr05, "p<.05", "n.s.")))) %>%
  dplyr::mutate(p = round(p, 6)) %>%
  dplyr::mutate(x2 = round(x2, 1)) %>%
  dplyr::mutate(phi = round(phi, 4)) %>%
  dplyr::select(-a, -b, -c, -d, - j, -NRows, -corr05, -corr01, -corr001,
                -Significance)
}