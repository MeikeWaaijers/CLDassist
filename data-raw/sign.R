sign <- causal_sign(topic = "addiction",
                    direction_df = dir$direction_df,
                    direction_threshold = 50,
                    LLM_model = "gpt-4o",
                    max_tokens = 2000,
                    update_key = FALSE)

usethis::use_data(sign, overwrite = TRUE)
