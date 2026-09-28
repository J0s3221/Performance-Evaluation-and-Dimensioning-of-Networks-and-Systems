# Exercise 9: analysis of packet sizes captured with Wireshark.
# Before running this script, export each capture as CSV from Wireshark.
# Resolve the script path when sourced; RStudio's Run button may execute lines
# without providing it, so fall back to locating the project from the working directory.
script_path <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
if (!is.null(script_path) && nzchar(script_path)) {
  script_dir <- dirname(normalizePath(script_path, winslash = "/"))
  capture_dir <- normalizePath(
    file.path(script_dir, "..", "captures"),
    winslash = "/",
    mustWork = FALSE
  )
} else {
  working_dir <- normalizePath(getwd(), winslash = "/")
  parent_dir <- dirname(working_dir)
  nearby_dirs <- unique(c(
    working_dir,
    list.dirs(working_dir, recursive = FALSE, full.names = TRUE),
    parent_dir
  ))
  possible_capture_dirs <- c(
    file.path(nearby_dirs, "module 1", "captures"),
    file.path(nearby_dirs, "captures")
  )
  existing_capture_dirs <- possible_capture_dirs[dir.exists(possible_capture_dirs)]
  capture_dir <- if (length(existing_capture_dirs)) existing_capture_dirs[1] else NA_character_
  if (!is.na(capture_dir)) {
    capture_dir <- normalizePath(capture_dir, winslash = "/")
    script_dir <- file.path(dirname(capture_dir), "scripts")
    if (!dir.exists(script_dir)) script_dir <- working_dir
  }
}

if (is.na(capture_dir) || !dir.exists(capture_dir)) {
  stop(
    "Capture folder not found. Expected 'module 1/captures' relative to the project. ",
    "Set the RStudio working directory to the project folder and run the script again."
  )
}

csv_files <- c(
  video = file.path(capture_dir, "capture_video.csv"),
  download = file.path(capture_dir, "capture_download.csv"),
  normal_state = file.path(capture_dir, "capture_normal_state.csv")
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

is_git_lfs_pointer <- function(file_name) {
  if (!file.exists(file_name)) {
    return(FALSE)
  }

  first_lines <- readLines(file_name, n = 5, warn = FALSE)
  any(grepl("^version https://git-lfs.github.com/spec/v1", first_lines))
}

read_packet_sizes <- function(file_name) {
  if (!file.exists(file_name)) {
    warning(
      "Skipping missing capture: ", file_name,
      ". Export the corresponding capture as CSV from Wireshark first."
    )
    return(NULL)
  }

  if (is_git_lfs_pointer(file_name)) {
    warning(
      "Skipping Git LFS pointer file: ", file_name,
      ". Download the real capture with 'git lfs pull' or re-export the CSV from Wireshark before running this script."
    )
    return(NULL)
  }

  data <- read.csv(file_name, check.names = FALSE, stringsAsFactors = FALSE)
  length_column <- find_length_column(data)
  packet_sizes <- suppressWarnings(as.numeric(data[[length_column]]))
  packet_sizes <- packet_sizes[!is.na(packet_sizes) & packet_sizes >= 0]

  if (length(packet_sizes) == 0) {
    warning("No valid packet sizes found in file: ", file_name)
    return(NULL)
  }

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

packet_sizes <- list()
for (capture_name in names(csv_files)) {
  capture_data <- read_packet_sizes(csv_files[[capture_name]])
  if (!is.null(capture_data)) {
    packet_sizes[[capture_name]] <- capture_data
  }
}

if (length(packet_sizes) == 0) {
  stop(
    "No valid packet captures were found. Ensure the CSV files exist and are not Git LFS pointer files."
  )
}

# Save one comparable histogram for each available capture.
pdf(file.path(script_dir, "E9_packet_sizes.pdf"), width = 9, height = 10)
par(mfrow = c(length(packet_sizes), 1), mar = c(4, 4, 3, 1))

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
