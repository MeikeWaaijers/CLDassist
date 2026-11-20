literature <- search_literature(topic = "addiction",
                                edge_list = edge_lists$sign_edge_list,
                                scientific = TRUE,
                                LLM_model = "gpt-4.1",
                                max_tokens = 2000,
                                update_key = FALSE)

usethis::use_data(literature, overwrite = TRUE)
