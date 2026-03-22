#' Unwraps a nested list of R objects into a flat list of outputs.
#'
#' This function takes a potentially nested list containing `data.frame`s,
#' `flextable` objects, and `ggplot` objects, and flattens it into a single
#' list containing only these output types.
#'
#' @param input_list A list, possibly deeply nested, containing `data.frame`,
#'   `flextable`, and `ggplot` objects. Other object types (e.g., vectors,
#'   matrices, functions) encountered during flattening will be ignored and
#'   will not appear in the final output list.
#'
#' @details
#' The function processes the input list with the following rules:
#' \itemize{
#'   \item All `data.frame` and `flextable` objects are extracted to the top
#'         level of the output list.
#'   \item `ggplot` objects are also extracted to the top level, with one
#'         important exception:
#'   \item A list containing *only* `ggplot` objects (e.g., `list(plot1, plot2)`)
#'         is kept as a single list object in the flattened output. Its internal
#'         `ggplot` objects are *not* extracted further to the top level. This
#'         ensures `ggplot` objects can remain grouped at a maximum depth of one.
#'         Any nested list within this `ggplot`-only list (e.g., `list(list(p))`)
#'         will cause `p` to be extracted, as the outer list is not purely `ggplot`s.
#'   \item Object naming follows these rules:
#'     \itemize{
#'       \item If an object (or a list of `ggplot`s that is kept as a group)
#'             has an existing name in its parent list, that name is used as its
#'             base name.
#'       \item If an object (or a `ggplot`-only list) is unnamed, it is assigned
#'             a name based on its object type and a sequential index (e.g.,
#'             "df_1", "flextable_1", "ggplot_1", "ggplot_list_1"). These counters
#'             are global across the entire unwrapping process.
#'       \item When a generic list (not a `ggplot`-only group) is flattened,
#'             the names of its immediate contents are prepended with the name
#'             of that flattened parent list. For example, if `my_list = list(df1)`
#'             is flattened, `df1` will be named something like `my_list_df_1`
#'             (assuming `df1` itself was unnamed). This prepending happens
#'             recursively for deeply nested structures.
#'     }
#' }
#'
#' @return A flattened list containing `data.frame`, `flextable`, and `ggplot`
#'   objects (or lists of `ggplot` objects), with names appropriately assigned
#'   or modified.
#' @export
#' @examples
#' \dontrun{
#' # Ensure required libraries are loaded for objects
#' library(ggplot2)
#' library(flextable)
#'
#' # Create dummy objects for demonstration
#' df1 <- data.frame(a = 1:2, b = c("X","Y"))
#' df2 <- data.frame(c = 3:4, d = c("A","B"))
#' p1 <- ggplot(df1, aes(a, a)) + geom_point()
#' p2 <- ggplot(df2, aes(c, c)) + geom_point()
#' p3 <- ggplot(df1, aes(a, b)) + geom_col()
#' ft1 <- flextable(df1)
#' ft2 <- flextable(df2)
#'
#' # Example input list with various nesting and naming scenarios
#' input_example <- list(
#'   my_dataframe = df1,
#'   ft1, # Unnamed flextable
#'   plot_group_A = list(p1, my_p2 = p2), # A list containing ONLY ggplots
#'   nested_container = list(
#'     df2, # Unnamed data.frame inside a nested list
#'     "another_flextable" = ft2,
#'     deeply_nested_plots = list(
#'       final_plot = p3,
#'       # This inner list will be flattened because it's not a direct ggplot list
#'       # `p4` would be named `nested_container_deeply_nested_plots_inner_plot_p4`
#'       inner_plot_container = list(p4 = ggplot(df1, aes(b,a)) + geom_line())
#'     ),
#'     "just_a_number" = 100 # This non-output object will be ignored
#'   ),
#'   "standalone_plot" = ggplot(df1, aes(a)) + geom_histogram()
#' )
#'
#' # Unwrap the list
#' unwrapped_output <- unwrapOutputs(input_example)
#'
#' # Inspect the names of the top-level flattened list
#' print(names(unwrapped_output))
#' # Expected output (counters might vary slightly based on R version/object order):
#' # [1] "my_dataframe"                                      "flextable_1"
#' # [3] "plot_group_A"                                      "nested_container_df_1"
#' # [5] "nested_container_another_flextable"                "nested_container_deeply_nested_plots_final_plot"
#' # [7] "nested_container_deeply_nested_plots_inner_plot_container_p4"
#' # [8] "standalone_plot"
#'
#' # Inspect the contents of the 'ggplot-only' group
#' print(names(unwrapped_output$plot_group_A))
#' # Expected output:
#' # [1] "ggplot_1" "my_p2"
#'
#' # Example with an empty list
#' empty_unwrapped <- unwrapOutputs(list())
#' print(empty_unwrapped) # Expected: list()
#'
#' # Example with a list containing only non-output objects
#' non_output_list <- list(1, "hello", TRUE, matrix(1:4, 2))
#' unwrapped_non_output <- unwrapOutputs(non_output_list)
#' print(unwrapped_non_output) # Expected: list()
#' }
unwrapOutputs <- function(input_list) {

  # Initialize counters for unnamed objects. These are updated globally
  # within the recursive helper function using `<<-`.
  df_counter <- 0
  flextable_counter <- 0
  ggplot_counter <- 0
  ggplot_list_counter <- 0

  # Recursive helper function to traverse and flatten the list
  unwrap_recursive <- function(current_list, parent_name = NULL) {
    result <- list() # Accumulator for the flattened outputs from current level

    for (i in seq_along(current_list)) {
      item <- current_list[[i]]
      item_name <- names(current_list)[i]

      # Determine the effective base name for the current 'item'.
      # This is the name before any parent names are prepended.
      effective_base_name <- if (!is.null(item_name) && item_name != "") {
        # Use existing name if present
        item_name
      } else {
        # If unnamed, generate a name based on type and global counters.
        if (inherits(item, "data.frame")) {
          df_counter <<- df_counter + 1
          paste0("df_", df_counter)
        } else if (inherits(item, "flextable")) {
          flextable_counter <<- flextable_counter + 1
          paste0("flextable_", flextable_counter)
        } else if (inherits(item, "ggplot")) {
          ggplot_counter <<- ggplot_counter + 1
          paste0("ggplot_", ggplot_counter)
        } else if (is.list(item) && all(sapply(item, inherits, "ggplot"))) {
          # This is a list containing only ggplot objects. It is treated as
          # a single output unit and not flattened further.
          ggplot_list_counter <<- ggplot_list_counter + 1
          paste0("ggplot_list_", ggplot_list_counter)
        } else if (is.list(item)) {
          # This is a generic list (either nested or mixed-type) that needs
          # to be flattened. This generated name is used for prepending to
          # its children's names, not as a final output object itself.
          paste0("list_item_", i)
        } else {
          # For any other object types (e.g., numeric, character, matrix),
          # return NULL so they are ignored.
          NULL
        }
      }

      # If effective_base_name is NULL, it means the item is not an output
      # type or a list containing outputs, so we skip it.
      if (is.null(effective_base_name)) {
        next
      }

      # Form the final name for the item by prepending parent_name if applicable.
      final_item_name <- if (!is.null(parent_name) && effective_base_name != "") {
        paste0(parent_name, "_", effective_base_name)
      } else {
        effective_base_name
      }

      # Process the item based on its identified type
      if (inherits(item, "data.frame") || inherits(item, "flextable") || inherits(item, "ggplot")) {
        # Directly add data.frame, flextable, or standalone ggplot objects
        result[[final_item_name]] <- item
      } else if (is.list(item)) {
        # Check for the special case: a list containing ONLY ggplot objects
        if (all(sapply(item, inherits, "ggplot"))) {
          # This specific type of list is kept as an output unit.
          # Its internal ggplot elements also need naming if they are unnamed,
          # but they remain encapsulated within this list.
          named_ggplot_list <- list()
          for (j in seq_along(item)) {
            sub_item <- item[[j]]
            sub_item_name <- names(item)[j]
            if (is.null(sub_item_name) || sub_item_name == "") {
              ggplot_counter <<- ggplot_counter + 1 # Use global ggplot counter for inner plots
              named_ggplot_list[[paste0("ggplot_", ggplot_counter)]] <- sub_item
            } else {
              named_ggplot_list[[sub_item_name]] <- sub_item
            }
          }
          result[[final_item_name]] <- named_ggplot_list
        } else {
          # It's a generic list (possibly nested or mixed types), so recurse
          # The current `final_item_name` becomes the `parent_name` for the next recursion level.
          recursive_results <- unwrap_recursive(item, parent_name = final_item_name)
          # Combine results from the recursion with current level's results
          result <- c(result, recursive_results)
        }
      }
      # Other object types are implicitly ignored due to the `is.null(effective_base_name)` check
    }
    return(result)
  }

  # Start the recursive unwrapping process with the initial input list
  unwrap_recursive(input_list)
}
