for (experiment in unique(metadata_linked$idoc_folder)) {
  sessions <- sort(list.dirs(experiment, recursive = F, full.names = F))
  n_sessions <- length(sessions)
  if (n_sessions == 3) {
    session_data <- list(
      pre = sessions[1],
      train_1 = sessions[2],
      post = sessions[3]
    )
    path <- file.path(experiment, "sessions.yaml")
    print(paste(n_sessions, path))
    yaml::write_yaml(file = path, x = session_data)
  } else if (n_sessions == 4) {
    session_data <- list(
      pre = sessions[1],
      train_1 = sessions[2],
      post_1 = sessions[3],
      post_2 = sessions[4]
    )
    path <- file.path(experiment, "sessions.yaml")
    print(paste(n_sessions, path))
    yaml::write_yaml(file = path, x = session_data)
  } else if (n_sessions == 8) {
    session_data <- list(
      pre = sessions[1],
      train_1 = sessions[2],
      train_2 = sessions[3],
      train_3 = sessions[4],
      train_4 = sessions[5],
      train_5 = sessions[6],
      train_6 = sessions[7],
      post = sessions[8]
    )
    path <- file.path(experiment, "sessions.yaml")
    print(paste(n_sessions, path))
    yaml::write_yaml(file = path, x = session_data)
  } else if (n_sessions == 9) {
    session_data <- list(
      pre = sessions[1],
      train_1 = sessions[2],
      train_2 = sessions[3],
      train_3 = sessions[4],
      train_4 = sessions[5],
      train_5 = sessions[6],
      train_6 = sessions[7],
      post_1 = sessions[8],
      post_2 = sessions[9]
    )
    path <- file.path(experiment, "sessions.yaml")
    print(paste(n_sessions, path))
    yaml::write_yaml(file = path, x = session_data)
  } else if (n_sessions == 10) {
    session_data <- list(
      pre = sessions[1],
      train_1 = sessions[2],
      train_2 = sessions[3],
      train_3 = sessions[4],
      train_4 = sessions[5],
      train_5 = sessions[6],
      train_6 = sessions[7],
      post_1 = sessions[8],
      post_2 = sessions[9],
      post_3 = sessions[10]
    )
    path <- file.path(experiment, "sessions.yaml")
    print(paste(n_sessions, path))
    yaml::write_yaml(file = path, x = session_data)
  } else {
    print(paste(n_sessions, experiment, "Sessions: ", paste(sessions, collapse = " ")))
  }
}
