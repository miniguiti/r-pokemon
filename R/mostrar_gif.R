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
  reaction_root <- get_extdata_dir("reaction")

  if (!nzchar(gen4_root)) stop("Pasta 'gen4' não encontrada.")
  if (!nzchar(backgrounds_root)) stop("Pasta 'backgrounds' não encontrada.")
  if (!nzchar(reaction_root)) stop("Pasta 'reaction' não encontrada.")

  happy_icon <- file.path(reaction_root, "happy.png")
  heart_icon <- file.path(reaction_root, "heart.png")
  pokeball_icon <- file.path(reaction_root, "pokeball.png")
  add_icon <- file.path(reaction_root, "light-add.svg")

  if (!file.exists(happy_icon)) stop("Arquivo 'happy.png' não encontrado em 'reaction'.")
  if (!file.exists(heart_icon)) stop("Arquivo 'heart.png' não encontrado em 'reaction'.")
  if (!file.exists(pokeball_icon)) stop("Arquivo 'pokeball.png' não encontrado em 'reaction'.")
  if (!file.exists(add_icon)) stop("Arquivo 'light-add.svg' não encontrado em 'reaction'.")

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

  build_sprite_set <- function(pokemon_dir) {
    pokemon_gifs <- list.files(
      pokemon_dir,
      pattern = "\\.gif$",
      full.names = TRUE,
      ignore.case = TRUE
    )

    default_walk <- pokemon_gifs[
      grepl("default", basename(pokemon_gifs), ignore.case = TRUE) &
        grepl("walk", basename(pokemon_gifs), ignore.case = TRUE) &
        !grepl("walk_left", basename(pokemon_gifs), ignore.case = TRUE)
    ]

    if (length(default_walk) == 0) {
      return(NULL)
    }

    default_walk <- sample(default_walk, 1)

    default_idle <- file.path(
      dirname(default_walk),
      sub("walk", "idle", basename(default_walk), ignore.case = TRUE)
    )

    shiny_walk <- file.path(
      dirname(default_walk),
      sub("default", "shiny", basename(default_walk), ignore.case = TRUE)
    )

    shiny_idle <- file.path(
      dirname(default_idle),
      sub("default", "shiny", basename(default_idle), ignore.case = TRUE)
    )

    if (!file.exists(default_idle) || !file.exists(shiny_walk) || !file.exists(shiny_idle)) {
      return(NULL)
    }

    data.frame(
      pokemon = basename(pokemon_dir),
      default_walk = default_walk,
      default_idle = default_idle,
      shiny_walk = shiny_walk,
      shiny_idle = shiny_idle,
      stringsAsFactors = FALSE
    )
  }

  pokemon_dirs <- list.dirs(gen4_root, recursive = FALSE, full.names = TRUE)
  sprite_list <- lapply(pokemon_dirs, build_sprite_set)
  sprite_list <- sprite_list[!vapply(sprite_list, is.null, logical(1))]

  if (length(sprite_list) < 5) {
    stop("São necessários pelo menos 5 pokémons com GIFs default/shiny (walk+idle).")
  }

  sprite_sets <- do.call(rbind, sprite_list)

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

  viewer_dir <- tempfile("pokemor_viewer_")
  dir.create(viewer_dir, recursive = TRUE, showWarnings = FALSE)

  html_file <- file.path(viewer_dir, "scene.html")

  asset_map <- new.env(parent = emptyenv())
  asset_counter <- 0L

  copy_asset <- function(path, prefix) {
    key <- normalizePath(path, winslash = "/", mustWork = TRUE)

    if (exists(key, envir = asset_map, inherits = FALSE)) {
      return(get(key, envir = asset_map, inherits = FALSE))
    }

    asset_counter <<- asset_counter + 1L
    ext <- tools::file_ext(path)
    file_name <- paste0(prefix, "_", asset_counter, ".", ext)
    file.copy(path, file.path(viewer_dir, file_name), overwrite = TRUE)
    assign(key, file_name, envir = asset_map)
    file_name
  }

  get_theme_color <- function(background_path) {
    background_path_lower <- tolower(background_path)

    if (grepl("/beach/", background_path_lower)) {
      return("#2E8BC0")
    }
    if (grepl("/castle/", background_path_lower)) {
      return("#6C757D")
    }
    if (grepl("/forest/", background_path_lower)) {
      return("#2D6A4F")
    }

    "#111111"
  }

  happy_name <- copy_asset(happy_icon, "reaction")
  heart_name <- copy_asset(heart_icon, "reaction")
  pokeball_name <- copy_asset(pokeball_icon, "reaction")
  add_name <- copy_asset(add_icon, "reaction")

  scene_count <- max(6L, min(12L, nrow(sprite_sets)))
  scene_configs <- vector("list", scene_count)

  for (scene_index in seq_len(scene_count)) {
    selected_rows <- sample(seq_len(nrow(sprite_sets)), 5, replace = FALSE)
    selected_pairs <- sprite_sets[selected_rows, , drop = FALSE]
    selected_background <- sample(background_files, 1)

    scene_configs[[scene_index]] <- list(
      theme = get_theme_color(selected_background),
      background = copy_asset(selected_background, "background"),
      pokemons = lapply(seq_len(nrow(selected_pairs)), function(index) {
        list(
          default_walk = copy_asset(selected_pairs$default_walk[index], "pokemon_dw"),
          default_idle = copy_asset(selected_pairs$default_idle[index], "pokemon_di"),
          shiny_walk = copy_asset(selected_pairs$shiny_walk[index], "pokemon_sw"),
          shiny_idle = copy_asset(selected_pairs$shiny_idle[index], "pokemon_si")
        )
      })
    )
  }

  scene_configs_js <- paste0(
    vapply(
      scene_configs,
      function(scene) {
        pokemon_js <- vapply(
          scene$pokemons,
          function(pokemon) {
            paste0(
              "{defaultWalk:'", pokemon$default_walk,
              "',defaultIdle:'", pokemon$default_idle,
              "',shinyWalk:'", pokemon$shiny_walk,
              "',shinyIdle:'", pokemon$shiny_idle, "'}"
            )
          },
          character(1)
        )

        paste0(
          "{theme:'", scene$theme,
          "',background:'", scene$background,
          "',pokemons:[", paste(pokemon_js, collapse = ","), "]}"
        )
      },
      character(1)
    ),
    collapse = ","
  )

  html_content <- paste0(
    "<html><head><meta charset='UTF-8'>
      <style>
        .scene { position: relative; width: 640px; height: 360px; overflow: hidden; }
        .runners-layer { position: absolute; inset: 0; }
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
        .reroll-btn {
          z-index: 20;
          width: 72px;
          height: 72px;
          border: 0;
          background: transparent;
          padding: 0;
          cursor: pointer;
        }
        .reroll-btn img {
          width: 100%;
          height: 100%;
          object-fit: contain;
          image-rendering: pixelated;
        }
        .control-bar {
          position: absolute;
          top: 8px;
          right: 8px;
          z-index: 20;
          display: flex;
          gap: 8px;
          align-items: center;
        }
        .add-btn {
          width: 22px;
          height: 22px;
          border: 0;
          border-radius: 10px;
          background: rgba(0, 0, 0, 0.72);
          padding: 6px;
          cursor: pointer;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .add-btn img {
          width: 100%;
          height: 100%;
          object-fit: contain;
        }
      </style>
      <script>
        document.addEventListener('DOMContentLoaded', function () {
          var scenes = [",
    scene_configs_js,
    "];
          var lanesBottom = [8, 60, 112, 164, 216];
          var lanesDuration = [6.0, 5.0, 6.8, 5.7, 6.3];
          var lanesDelay = [0.0, -1.1, -2.0, -0.6, -1.6];

          var sceneEl = document.getElementById('scene');
          var runnersLayer = document.getElementById('runners-layer');
          var rerollBtn = document.getElementById('reroll-btn');
          var addBtn = document.getElementById('add-btn');
          var currentSceneIndex = 0;
          var currentPokemonCount = 1;

          function buildRunner(pokemon, index) {
            var runner = document.createElement('div');
            runner.className = 'pokemon-runner';
            runner.style.bottom = lanesBottom[index] + 'px';
            runner.style.animationDuration = lanesDuration[index] + 's';
            runner.style.animationDelay = lanesDelay[index] + 's';
            runner.style.animationDirection = (index % 2 === 0) ? 'alternate' : 'reverse';

            var happy = document.createElement('img');
            happy.className = 'reaction-happy';
            happy.src = '", happy_name, "';
            happy.style.display = 'none';
            happy.style.position = 'absolute';
            happy.style.bottom = '108px';
            happy.style.left = '50%';
            happy.style.transform = 'translateX(-50%)';
            happy.style.width = '56px';
            happy.style.height = '56px';
            happy.style.objectFit = 'contain';
            happy.style.imageRendering = 'pixelated';
            happy.style.pointerEvents = 'none';

            var heart = document.createElement('img');
            heart.className = 'reaction-heart';
            heart.src = '", heart_name, "';
            heart.style.display = 'none';
            heart.style.position = 'absolute';
            heart.style.bottom = '148px';
            heart.style.left = '50%';
            heart.style.transform = 'translateX(-50%)';
            heart.style.width = '36px';
            heart.style.height = '36px';
            heart.style.objectFit = 'contain';
            heart.style.imageRendering = 'pixelated';
            heart.style.pointerEvents = 'none';

            var sprite = document.createElement('img');
            sprite.className = 'pokemon-sprite';
            sprite.src = pokemon.defaultWalk;
            sprite.style.width = '112px';
            sprite.style.height = '112px';
            sprite.style.objectFit = 'contain';
            sprite.style.imageRendering = 'pixelated';
            sprite.style.cursor = 'pointer';

            var shinyMode = false;
            var heartMode = false;

            sprite.addEventListener('mouseenter', function () {
              happy.style.display = heartMode ? 'none' : 'block';
              heart.style.display = heartMode ? 'block' : 'none';
              sprite.src = shinyMode ? pokemon.shinyIdle : pokemon.defaultIdle;
            });

            sprite.addEventListener('mouseleave', function () {
              happy.style.display = 'none';
              heart.style.display = 'none';
              sprite.src = shinyMode ? pokemon.shinyWalk : pokemon.defaultWalk;
            });

            sprite.addEventListener('click', function () {
              shinyMode = true;
              heartMode = true;
              happy.style.display = 'none';
              heart.style.display = 'block';
              sprite.src = pokemon.shinyWalk;
            });

            runner.appendChild(happy);
            runner.appendChild(heart);
            runner.appendChild(sprite);

            return runner;
          }

          function renderScene(index) {
            var scene = scenes[index];
            document.body.style.background = scene.theme;
            sceneEl.style.backgroundImage = 'url(' + scene.background + ')';
            sceneEl.style.border = '3px solid ' + scene.theme;
            sceneEl.style.boxShadow = '0 0 18px ' + scene.theme;

            runnersLayer.innerHTML = '';
            scene.pokemons.slice(0, currentPokemonCount).forEach(function (pokemon, idx) {
              runnersLayer.appendChild(buildRunner(pokemon, idx));
            });

            addBtn.disabled = currentPokemonCount >= 5;
            addBtn.style.opacity = addBtn.disabled ? '0.45' : '1';
            addBtn.style.cursor = addBtn.disabled ? 'not-allowed' : 'pointer';
          }

          rerollBtn.addEventListener('click', function () {
            var next = currentSceneIndex;

            if (scenes.length > 1) {
              while (next === currentSceneIndex) {
                next = Math.floor(Math.random() * scenes.length);
              }
            }

            currentSceneIndex = next;
            renderScene(currentSceneIndex);
          });

          addBtn.addEventListener('click', function () {
            if (currentPokemonCount < 5) {
              currentPokemonCount += 1;
              renderScene(currentSceneIndex);
            }
          });

          renderScene(currentSceneIndex);
        });
      </script>
    </head>
     <body style='margin:0;display:flex;justify-content:center;align-items:center;min-height:100vh;background:#111;'>
       <div class='scene' id='scene' style='background-size:640px 360px;background-position:center;background-repeat:no-repeat;'>
         <div class='control-bar'>
           <button class='add-btn' id='add-btn' title='Adicionar pokémon'>
             <img src='", add_name, "'>
           </button>
           <button class='reroll-btn' id='reroll-btn' title='Novo cenário'>
             <img src='", pokeball_name, "'>
           </button>
         </div>
         <div class='runners-layer' id='runners-layer'></div>
       </div>
     </body></html>"
  )

  writeLines(html_content, html_file)
  rstudioapi::viewer(html_file)
}
