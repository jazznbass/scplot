# scplot 0.3

## new functions:

- `new_theme()`: creates a new scplot-theme.
```{.r}
new_theme <- new_theme() %>% 
  set_xaxis(color = "brown") %>%
  set_yaxis(color = "sienna3") %>%
  set_ylabel(color = "sienna3", angle = 0) %>%
  set_xlabel(size = 1, color = "brown") %>%
  add_title(color = "sienna4", face = "italic") %>%
  set_casenames(color = "steelblue4", size = 0.7) %>%
  set_phasenames(size = 0) %>% 
  set_panel(fill = c("grey80", "grey95"), color = "sienna4") %>%
  add_grid(color = "grey85", size = 0.5) %>%
  set_dataline(
    size = 0.5, linetype = "solid", 
    point = list(colour = "sienna4", size = 0.5, shape = "circle")
  ) %>%
  set_separator(size = 0.5, linetype = "solid", color = "sienna") %>%
  set_background(fill = "grey99") 

scplot(exampleABC) %>% set_theme(new_theme)

```

- `extract_theme()`: returns the theme of an scplot object (instead of creating a plot). This theme can be reused in other plots with the `add_theme()` function.

```{.r}
new_theme <- scplot() %>% 
  set_xaxis(color = "brown") %>%
  set_yaxis(color = "sienna3") %>%
  set_ylabel(color = "sienna3", angle = 0) %>%
  set_xlabel(size = 1, color = "brown") %>%
  add_title(color = "sienna4", face = "italic") %>%
  set_casenames(color = "steelblue4", size = 0.7) %>%
  set_phasenames(size = 0) %>% 
  set_panel(fill = c("grey80", "grey95"), color = "sienna4") %>%
  add_grid(color = "grey85", size = 0.5) %>%
  set_dataline(
    size = 0.5, linetype = "solid", 
    point = list(colour = "sienna4", size = 0.5, shape = "circle")
  ) %>%
  set_separator(size = 0.5, linetype = "solid", color = "sienna") %>%
  set_background(fill = "grey99") %>%
  extract_theme()

scplot(exampleABC) %>% set_theme(new_theme)

```

## fixed bugs

- solved: scdf with dulicated casenames crash

## existing functions:

- ggplot2 deprecated the size argument for lines and replaced it with linewidth. We adapted several functions to follow this change which now use the linewidth argument instead of size: `set_dataline(), add_dataline(), add_statline(), set_grid()`.

- `add_statline()`: New `type` "bar"  
- `set_xaxis()` `set_yaxis()`: New argument expand (see ggplot scale_continuous).
- `set_xaxis()`: Extended Argument `position` with `strip-right` and `strip-top`

# scplot 0.2

- replaced all dplyr functions with base R functions.
- each scplot function is now in a separate file.

# scplot 0.1.0 

## New experimental function for ploting

The new `scplot()` function is here! It allows for a more tidy coding and the use of `%>%` (or `|>`) operators. `scplot` is in an experimental state and code with
current syntax might not work in  a later version due to changes in function and argument names. Still, `scplot()` works in many cases.
We plan to add new graphical features primarily to `scplot` which is already capable of doing more than `plot.scdf()`.

Here is an example that implicitly also introduces several of the new graphical functions:

```{.r}
scplot(exampleABAB) %>% 
  add_statline(stat = "trendA", colour = "tomato2") %>%
  add_statline(stat = "maxA", colour = "lightblue") %>%
  add_marks(case = 1:2, positions = 14, colour = "red3", size = 3, shape = 4) %>%
  add_marks(case = "all", positions = 'points < quantile(points, 0.1)', colour = "blue3", size = 2) %>%
  add_marks(positions = outlier(exampleABAB), colour = "brown", size = 2) %>%
  add_labels(colour = "sienna") %>%
  set_xaxis(increment = 4, size = 0.7, colour = "brown") %>%
  set_yaxis(limits = c(0, 50), colour = "sienna3", size = 0.7) %>%
  set_ylabel("Points", colour = "sienna3", size = 0.7, orientation = 0) %>%
  set_xlabel("Weeks", size = 0.7, colour = "brown") %>%
  add_title("Points by week", colour = "sienna4", size = 1.5, font = 3) %>%
  set_phasenames("Baseline", "Intervention", "Fall-Back", "Intervention", cex = 1, colour = "darkgreen") %>%
  add_theme("tiny") %>%
  set_background(c("grey94", "grey99")) %>%
  add_grid(colour = "grey85", width = 0.5) %>%
  add_frame("sienna4") %>%
  set_dots("sienna4", size = 1, shape = 18) %>%
  set_line("black", width = 1, type = "dotted") %>%
  add_text(case = 1, x = 5, y = 35, "Wow!!", colour = "darkgreen", angle = 20) %>%
  add_text(case = 1, 1, 22, "PND", colour = "darkblue", size = 1.3) %>%
  add_text(case = 1, 4, 8, "Trend", colour = "tomato", size = 1.3) %>%
  add_arrow(case = 1, 5, 30, 5, 22, colour = "steelblue") %>%
  add_ridge("white") %>%
  set_casenames("MY", "FUNNY", "VALENTINE", colour = "steelblue4", size = 0.6) %>%
  add_box("sienna1", width = 2) %>%
  set_separator(extent = 0.9, width = 0.5, type = "solid", colour = "sienna")
```


