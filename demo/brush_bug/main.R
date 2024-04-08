library(detourr)

n_points <- 100

init_sphere <- geozoo::sphere.solid.random(p = 4, n = n_points)
s1_points <- init_sphere$points
s2_points <- (s1_points + 2) * 3
s3_points <- (s1_points - 2) * 8

data <- rbind(s1_points, s2_points, s3_points) |> as.data.frame()
data$id <- seq(1, nrow(data))

detour(data, tour_aes(projection = V1:V4, label = id)) |>
  tour_path(grand_tour(3)) |>
  show_scatter()

# can replicate the issue with the above configuration
# selecting and coloring points within the cluster
# doesn't color all the points in it.
