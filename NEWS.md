# taxospace 0.0.9004

* Due to changes in the GNR API to GNV, the `get_classification()` function has been updated to use the new API. The `get_classification()` function now accepts only `"gnr"` as `sel_db_name` argument.

# taxospace 0.0.9003

* `get_classification()` - add a scenario for which the `usagekey` is not returned

# taxospace 0.0.9002

* `get_classification()` - fix an issue with a scenario where one name in `resolve()` prevent the rest to run the classification

* README - change to QUARTO and add descriptions and examples
