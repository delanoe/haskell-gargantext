jq < repo.json '.state |= map_values(map_values(length)) | .history |= length'
