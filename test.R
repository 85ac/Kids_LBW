# Test Script
weight = 20

paste(ifelse(0.02*weight > 0.6, 0.6, 0.02*weight), "mg")


column_spec(1, background = c(col_GA, col_GA, col_GA, 
                              col_opioid, col_opioid, col_opioid, col_opioid,
                              col_relaxant,
                              extra_css = generate_background_stripes(col_relaxant, "white"), extra_css = generate_background_stripes(col_relaxant, "white"),
                              col_antichol, col_antichol,
                              "white", "white",
                              col_ponv, col_ponv,
                              "white", "white",
                              "orange", "white", col_GA, col_GA),
            width = "2em") %>%