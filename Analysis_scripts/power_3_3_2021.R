library(metapower)
library(psych)

mpower(.4, k = 60, sample_size = 176, es_type = "r")


#But one option may be to extract the degrees of freedom found in the analysis for the covariate of interest 
#(which will take into account the uneven k, n, etc) and then use these as the number of studies in a traditional MA power analysis 
#(e.g., using the package you mention). For example, if you have 20 studies but the df are 5.7, you might just say the sample size is 6 studies 
#(since these df are kind of the ‘effective sample size’)

fisherz2r(.5)
fisherz2r(.4)
fisherz2r(.3)

es <- c(0.46211718, 0.379949)
es3 <- c(0.46211718, 0.379949, 0.2913126)


subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 5, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 11, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 24, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 20, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 19, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 22, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 37, es_type = "r")
subgroup_power(n_groups = 3, effect_sizes = es3, sample_size = 177, k = 12, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 32, es_type = "r")


