# Interactively Edit a SEM Graph

Launch a Shiny editor for an existing `sem_graph` object. Drag nodes,
adjust node dimensions and labels, change edge connections and
curvature, edit supported plotting aesthetics, and change graph-level
plotting settings. For multigroup or multilevel graphs, switch between
panels. Optionally synchronize node positions across groups within the
same level. Changes are shown in a ggplot preview and can be undone,
redone, or reset.

## Usage

``` r
graph_editor(graph)
```

## Arguments

- graph:

  An object inheriting from class `sem_graph`, typically created by
  [`prepare_graph()`](https://cjvanlissa.github.io/tidySEM/reference/prepare_graph.md).

## Value

An edited `sem_graph` object when **Done** is clicked, or `NULL` when
**Cancel** is clicked. The supplied object is not modified in place.

## Details

This function requires the optional packages `shiny` and `htmltools`.
They are checked at runtime and do not need to be installed for other
tidySEM functionality. The editor uses
[`update_graph()`](https://cjvanlissa.github.io/tidySEM/reference/update_graph.md)
to maintain node geometry without rerunning layout or edge-routing
algorithms.

Panel switching does not change the underlying graph. When enabled,
**Apply layout to all groups** copies node positions (not aesthetics,
dimensions, or edge properties) to same-named nodes in other groups at
the same level. This setting is available only for multigroup models.

Edits are saved automatically within the app. Clicking **Done** returns
the edited graph; clicking **Cancel** returns `NULL`.

## See also

[`prepare_graph()`](https://cjvanlissa.github.io/tidySEM/reference/prepare_graph.md),
[`update_graph()`](https://cjvanlissa.github.io/tidySEM/reference/update_graph.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # graph is an existing sem_graph object
  graph <- prepare_graph(
  edges = data.frame(from = "x", to = "y"),
  layout = matrix(c("x", "y")))
  edited <- graph_editor(graph)
  if (!is.null(edited)) plot(edited)
} # }
```
