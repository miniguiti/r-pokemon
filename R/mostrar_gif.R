#' Mostrar GIF no Viewer
#'
#' @export
mostrar_gif <- function() {
  get_extdata_dir <- function(folder_name) {
    package_dir <- system.file("extdata", folder_name, package = "pokemor")
    local_dir <- file.path(getwd(), "inst", "extdata", folder_name)

    if (nzchar(package_dir) && dir.exists(package_dir)) {
      return(package_dir)
    }

    if (dir.exists(local_dir)) {
      return(local_dir)
    }

    ""
  }

  gen4_root <- get_extdata_dir("gen4")
  backgrounds_root <- get_extdata_dir("backgrounds")

  if (!nzchar(gen4_root)) stop("Pasta 'gen4' não encontrada.")
  if (!nzchar(backgrounds_root)) stop("Pasta 'backgrounds' não encontrada.")

  gif_files <- list.files(
    gen4_root,
    pattern = "\\.gif$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(gif_files) == 0) {
    stop("Nenhum GIF encontrado em 'gen4'.")
  }

  walk_gif_files <- gif_files[
    grepl("walk", basename(gif_files), ignore.case = TRUE)
  ]

  if (length(walk_gif_files) == 0) {
    stop("Nenhum GIF com 'walk' no nome foi encontrado em 'gen4'.")
  }

  if (length(walk_gif_files) < 4) {
    stop("São necessários pelo menos 4 GIFs com 'walk' em 'gen4'.")
  }

  background_files <- list.files(
    backgrounds_root,
    pattern = "\\.(png|jpg|jpeg)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  background_files <- background_files[
    grepl("background", basename(background_files), ignore.case = TRUE)
  ]

  if (length(background_files) == 0) {
    stop("Nenhum background encontrado em 'backgrounds'.")
  }

  selected_gifs <- sample(walk_gif_files, 4, replace = FALSE)
  selected_background <- sample(background_files, 1)

  background_path_lower <- tolower(selected_background)
  theme_color <- if (grepl("/beach/", background_path_lower)) {
    "#2E8BC0"
  } else if (grepl("/castle/", background_path_lower)) {
    "#6C757D"
  } else if (grepl("/forest/", background_path_lower)) {
    "#2D6A4F"
  } else {
    "#111111"
  }

  viewer_dir <- tempfile("pokemor_viewer_")
  dir.create(viewer_dir, recursive = TRUE, showWarnings = FALSE)

  bg_ext <- tools::file_ext(selected_background)

  bg_name <- paste0("background.", bg_ext)

  bg_target <- file.path(viewer_dir, bg_name)
  html_file <- file.path(viewer_dir, "scene.html")

  file.copy(selected_background, bg_target, overwrite = TRUE)

  gif_names <- vapply(
    seq_along(selected_gifs),
    function(index) {
      ext <- tools::file_ext(selected_gifs[index])
      name <- paste0("pokemon_", index, ".", ext)
      file.copy(selected_gifs[index], file.path(viewer_dir, name), overwrite = TRUE)
      name
    },
    character(1)
  )

  lanes_bottom <- c(8, 70, 132, 194)
  lanes_duration <- c(6.0, 5.0, 6.8, 5.7)
  lanes_delay <- c(0.0, -1.1, -2.0, -0.6)

  runners_html <- paste0(
    vapply(
      seq_along(gif_names),
      function(index) {
        direction <- if (index %% 2 == 0) "reverse" else "alternate"
        paste0(
          "<div class='pokemon-runner' style='bottom:", lanes_bottom[index], "px; animation-duration:",
          lanes_duration[index], "s; animation-delay:", lanes_delay[index], "s; animation-direction:", direction, ";'>",
          "<img src='", gif_names[index], "' style='width:112px;height:112px;object-fit:contain;image-rendering:pixelated;'>",
          "</div>"
        )
      },
      character(1)
    ),
    collapse = ""
  )

  html_content <- paste0(
    "<html><head><meta charset='UTF-8'>
      <style>
        .scene { position: relative; width: 640px; height: 360px; overflow: hidden; }
        .pokemon-runner {
          position: absolute;
          left: 10px;
          width: 112px;
          height: 112px;
          display: flex;
          align-items: flex-end;
          justify-content: center;
          animation: run-lr linear infinite alternate;
        }
        @keyframes run-lr {
          from { transform: translateX(0); }
          to { transform: translateX(518px); }
        }
      </style>
    </head>
     <body style='margin:0;display:flex;justify-content:center;align-items:center;min-height:100vh;background:", theme_color, ";'>
       <div class='scene' style='background-image:url(\"", bg_name, "\");background-size:640px 360px;background-position:center;background-repeat:no-repeat;border:3px solid ", theme_color, ";box-shadow:0 0 18px ", theme_color, ";'>
         ", runners_html, "
       </div>
     </body></html>"
  )

  writeLines(html_content, html_file)
  rstudioapi::viewer(html_file)
}
