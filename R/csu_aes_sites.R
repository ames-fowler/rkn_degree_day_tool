############################################################
# CSU AES Site Helpers
# File: R/csu_aes_sites.R
############################################################

library(tibble)

read_csu_aes_sites <- function() {
  tibble(
    site_name = c(
      "Fort Collins AERC",
      "CSU ARDEC",
      "San Luis Valley Research Center",
      "Western Colorado Research Center - Fruita",
      "Western Colorado Research Center - Rogers Mesa",
      "Western Colorado Research Center - Orchard Mesa",
      "Arkansas Valley Research Center",
      "Plainsman Research Center",
      "CSU Spur"
    ),
    short_name = c(
      "AERC",
      "ARDEC",
      "SLVRC",
      "Fruita",
      "Rogers Mesa",
      "Orchard Mesa",
      "AVRC",
      "Plainsman",
      "Spur"
    ),
    latitude = c(
      40.5947,
      40.6523,
      37.7067,
      39.1830,
      38.7985,
      39.0441,
      38.0385,
      37.3830,
      39.7836
    ),
    longitude = c(
      -105.1370,
      -104.9962,
      -106.1444,
      -108.6970,
      -107.7882,
      -108.4673,
      -103.6950,
      -102.2940,
      -104.9742
    ),
    source_note = "CSU AES/research site from local CoAgMET station registry"
  )
}
