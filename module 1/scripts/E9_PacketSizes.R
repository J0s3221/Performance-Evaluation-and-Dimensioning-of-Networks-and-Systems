# Exercise 9: analysis of packet sizes captured with Wireshark.
# Before running this script, export each capture as CSV from Wireshark.
# Run the script from the repository root, where the CSV files are stored.

csv_files <- c(
  video = "capture_video.csv",
  download = "capture_download.csv",
  normal_state = "capture_normal_state.csv"
)

# Maximum packet size considered valid for this analysis.
# Larger values can be caused by TCP Segmentation Offload.
maximum_packet_size <- 1500

find_length_column <- function(data) {
  column_names <- tolower(names(data))
  normalized_names <- gsub("[^a-z0-9]", "", column_names)

  possible_names <- c(
    "length", "framelen", "packetlength", "packetsize",
    "capturedlength", "framecapturedlength"
  )

  match_index <- match(possible_names, normalized_names)
  match_index <- match_index[!is.na(match_index)]

  if (length(match_index) == 0) {
    stop(
      "Could not find a packet-length column. Available columns: ",
      paste(names(data), collapse = ", ")
    )
  }

  match_index[1]
}

read_packet_sizes <- function(file_name) {
  if (!file.exists(file_name)) {
    stop(
      "File not found: ", file_name,
      ". Export the corresponding capture as CSV from Wireshark first."
    )
  }

  data <- read.csv(file_name, check.names = FALSE, stringsAsFactors = FALSE)
  length_column <- find_length_column(data)
  packet_sizes <- suppressWarnings(as.numeric(data[[length_column]]))
  packet_sizes <- packet_sizes[!is.na(packet_sizes) & packet_sizes >= 0]

  total_packets <- length(packet_sizes)
  removed_packets <- sum(packet_sizes > maximum_packet_size)
  packet_sizes <- packet_sizes[packet_sizes <= maximum_packet_size]

  cat("\nFile:", file_name, "\n")
  cat("Packets read:", total_packets, "\n")
  cat("Packets removed (>", maximum_packet_size, "bytes):", removed_packets, "\n")
  cat("Packets analyzed:", length(packet_sizes), "\n")
  cat("Mean packet size:", round(mean(packet_sizes), 2), "bytes\n")
  cat("Median packet size:", median(packet_sizes), "bytes\n")

  packet_sizes
}

packet_sizes <- lapply(csv_files, read_packet_sizes)

# Save the three comparable histograms in one PDF file.
pdf("E9_packet_sizes.pdf", width = 9, height = 10)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

for (capture_name in names(packet_sizes)) {
  hist(
    packet_sizes[[capture_name]],
    breaks = seq(0, maximum_packet_size, by = 50),
    xlim = c(0, maximum_packet_size),
    main = paste("Packet sizes -", gsub("_", " ", capture_name)),
    xlab = "Packet size (bytes)",
    ylab = "Frequency",
    col = "lightblue",
    border = "white"
  )
}

dev.off()
cat("\nHistogram file created: E9_packet_sizes.pdf\n")
