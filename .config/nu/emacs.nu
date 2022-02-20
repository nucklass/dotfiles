def find-file [file] {
    printf '\e]51;Efind-file "%s\/%s" \e\\' (pwd) $file
}