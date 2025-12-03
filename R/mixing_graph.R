library(igraph)
library(ggraph)
library(dplyr)

## --- 1. Contact matrix ------------------------------------------------------

age_groups <- c("0-17", "18-64", "65+")

day_contact_mat <- matrix(
  c(0.944, 0.054, 0.002,
    0.150, 0.820, 0.030,
    0.139, 0.686, 0.175),
  nrow = 3, byrow = TRUE,
  dimnames = list(from = age_groups, to = age_groups)
)

night_contact_mat <- matrix(
  c(0.482, 0.490, 0.028,
    0.262, 0.684, 0.054,
    0.153, 0.554, 0.292),
  nrow = 3, byrow = TRUE,
  dimnames = list(from = age_groups, to = age_groups)
)

contact_mat <- night_contact_mat

edges <- as.data.frame(as.table(contact_mat)) |> 
  rename(from = from, to = to, weight = Freq)


## --- 2. Node sets with age-group colors -----------------------------------

age_colors <- c(
  "0-17"  = "#8ecae6",   # light blue
  "18-64" = "#a0c4ff",   # mid blue
  "65+"   = "#bde0fe"    # very light blue
)

age_colors <- c(
  "0-17"  = "#b7d4deff",   # MetaRVM teal
  "18-64" = "#d8a1c2ff",   # MetaRVM plum
  "65+"   = "#aee7c7ff"    # MetaRVM green
)

nodes_left  <- data.frame(
  name  = paste0("L_", age_groups),
  group = "from",
  label = age_groups,
  age_group = age_groups,
  type = TRUE
)

nodes_right <- data.frame(
  name  = paste0("R_", age_groups),
  group = "to",
  label = age_groups,
  age_group = age_groups,
  type = FALSE
)

nodes <- bind_rows(nodes_left, nodes_right)

edges_bip <- edges |> 
  mutate(
    from = paste0("L_", from),
    to   = paste0("R_", to)
  )

## --- 3. Build graph ---------------------------------------------------------

g <- graph_from_data_frame(edges_bip, vertices = nodes, directed = FALSE)

## --- 4. Plot bipartite graph ------------------------------------------------

gg <- ggraph(g, layout = "bipartite") +
  
  geom_edge_link(
    aes(width = weight, alpha = weight),
    colour = "#145374"
  ) +
  
  # BIG hollow squares
  geom_node_point(
    aes(fill = age_group),
    shape = 22,         # hollow square shape
    size = 25,          # increase for larger boxes
    stroke = 1.8,
    colour = "#145374"
  ) +
  
  # labels inside squares
  geom_node_text(
    aes(label = label),
    size = 5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = age_colors) +
  scale_edge_width(range = c(0.5, 4)) +
  scale_edge_alpha(range = c(0.3, 0.9)) +
  
  coord_flip(ylim = c(-1, 2), xlim = c(0, 3)) +
  theme_void() +
  theme(legend.position = "none")

ggsave("figures/night_contact.png", gg)



## Mixing matrices

m_wd_d <- matrix(
  c(0.944, 0.054, 0.002,
    0.150, 0.820, 0.030,
    0.139, 0.686, 0.175),
  nrow = 3, byrow = TRUE,
  dimnames = list(from = age_groups, to = age_groups)
)

m_wd_n <- matrix(
  c(0.482, 0.490, 0.028,
    0.262, 0.684, 0.054,
    0.153, 0.554, 0.292),
  nrow = 3, byrow = TRUE,
  dimnames = list(from = age_groups, to = age_groups)
)

m_we_d <- matrix(
  c(0.944, 0.054, 0.002,
    0.150, 0.820, 0.030,
    0.139, 0.686, 0.175),
  nrow = 3, byrow = TRUE,
  dimnames = list(from = age_groups, to = age_groups)
)

m_we_n <- matrix(
  c(0.482, 0.490, 0.028,
    0.262, 0.684, 0.054,
    0.153, 0.554, 0.292),
  nrow = 3, byrow = TRUE,
  dimnames = list(from = age_groups, to = age_groups)
)