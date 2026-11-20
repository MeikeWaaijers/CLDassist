# CLDassist 4.0.0

## New features

* Models from other providers can now be used by writing `LLM_model` as `"<provider>/<model name>"`. Supported providers: `openai`, `deepseek`, `together` and `huggingface` (many open-weight models with one Hugging Face token; add `:<company>` to fix the company that runs the model).
* New function `check_llm()` checks whether a model can be used with CLDassist (log probabilities, answer as first token) and explains why not.
* `cld()` now returns the results so far, with a message, when no causal relationship or no causal direction is found, instead of stopping with an error.

## Breaking changes

* Removed the built-in models `"mixtral"` and `"llama-3"`, which are no longer offered by their providers. Open-weight models can now be used via the supported providers.
* `causal_sign()` now only takes the `direction_df` output of `causal_direction()` (argument `direction_df`, with `direction_threshold`).
* `cld_plot()` now takes a single `sign_df` and returns `sign_edge_list` (previously `rel_sign_edge_list` and `dir_sign_edge_list`). Edge lists now include a `color` column.
* `causal_threshold` was renamed to `relation_threshold`.

## Improvements

* Revised prompts (clearer examples and guidelines for variable lists).
* Probabilities of answer variants (e.g. "C" and " C") are now summed.
* Failed LLM calls no longer break the output: they are left out of the averages, reported as a warning, and listed in the `error` column of `raw_LLM`. If all calls fail, the function stops with the provider's error message.
* Clearer error messages for API problems (invalid key, no credit, unknown model, rate limits).
* The example data sets were regenerated with the new prompts.

# CLDassist 3.0.0

* Package renamed from `theoraizer` to `CLDassist`.
