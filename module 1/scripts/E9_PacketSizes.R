# Exercise 9: analysis of packet sizes captured with Wireshark.
# Export each capture as CSV from Wireshark (Export Packet Dissections) before running.

maximum_packet_size <- 1500   # packets above this are excluded (see report)

csv_files <- c(
  video        = "captures/capture_video.csv",
  download     = "captures/capture_download.csv",
  normal_state = "captures/capture_normal_state.csv"
)

read_packet_sizes <- function(file_name) {
  data <- read.csv(file_name)
  packet_sizes <- as.numeric(data$Length)
  packet_sizes <- packet_sizes[!is.na(packet_sizes) & packet_sizes >= 0]

  total    <- length(packet_sizes)
  packet_sizes <- packet_sizes[packet_sizes <= maximum_packet_size]
  excluded <- total - length(packet_sizes)

  cat("\nFile:", file_name, "\n")
  cat("Packets read:", total, "| Excluded:", excluded,
      "| Analyzed:", length(packet_sizes), "\n")
  cat("Mean:", round(mean(packet_sizes), 2),
      "bytes | Median:", median(packet_sizes), "bytes\n")

  packet_sizes
}

packet_sizes <- lapply(csv_files, read_packet_sizes)

pdf("E9_packet_sizes.pdf", width = 9, height = 10)
par(mfrow = c(length(packet_sizes), 1), mar = c(4, 4, 3, 1))
for (name in names(packet_sizes)) {
  hist(packet_sizes[[name]],
       breaks = seq(0, maximum_packet_size, by = 50),
       xlim = c(0, maximum_packet_size),
       main = paste("Packet sizes -", gsub("_", " ", name)),
       xlab = "Packet size (bytes)", ylab = "Frequency",
       col = "lightblue", border = "white")
}
dev.off()