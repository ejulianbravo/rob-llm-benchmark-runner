# Rough cost estimate for a set of calls, made before anything is sent.
#
# This is an estimate, not a measurement:
#   - text tokens are approximated as characters / 4;
#   - PDF tokens are pages x model$pdf_tokens_per_page (an assumption in the
#     config, to be replaced by values measured in the cost test);
#   - output tokens are model$expected_output_tokens (also an assumption);
#   - caching is ignored, so the estimate errs on the high side for input.
# Prices are list prices per million tokens from the config, in US dollars.

approx_tokens <- function(text) ceiling(sum(nchar(text, type = "chars")) / 4)

estimate_call_cost <- function(model, system, manuals, instrument_prompt, pdf_path, target) {
  pages <- pdftools::pdf_info(pdf_path)$pages
  input <- approx_tokens(c(system, manuals, instrument_prompt, target)) +
    pages * (model$pdf_tokens_per_page %||% NA_real_)
  output <- model$expected_output_tokens %||% model$max_output_tokens
  usd <- input / 1e6 * (model$price_input_usd_per_m %||% NA_real_) +
    output / 1e6 * (model$price_output_usd_per_m %||% NA_real_)
  data.frame(pdf_pages = pages, est_input_tokens = input, est_output_tokens = output,
             est_usd = round(usd, 4))
}
