package main

import (
	"encoding/json"
	//"fmt"
	"net/http"
	"time"
)

var st state

type state struct {
	files map[string]file
}

type file struct {
	data     string
	modified time.Time
}

type response struct {
	updates  map[string]file
	requests []string
}

func sync(w http.ResponseWriter, r *http.Request) {
	json.NewDecoder(r.Body).Decode(files)
	updates := make(map[string]file)
	requests := make([]string, 0)

	files := make(map[string]file)
	for key, serverFile := range st.files {
		clientFile, prs := files[key]
		if !prs {
			clientFile = file{data: "", modified: time.Unix(0, 0)}
		}

		if serverFile.modified.After(clientFile.modified) {
			updates[key] = serverFile
		} else if serverFile.modified.Before(clientFile.modified) {
			requests = append(requests, key)
		}
	}

	for key, _ := range files {
		_, prs := st.files[key]
		if !prs {
			requests = append(requests, key)
		}
	}

	resp := response{updates: updates, requests: requests}
	js, err := json.Marshal(resp)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func main() {
	st = state{files: make(map[string]file)}
	//http.HandleFunc(`/save/{rest:[a-zA-Z0-9=\-\/\.]+}`, saved)
	http.HandleFunc("/sync", sync)
	http.ListenAndServe(":8000", nil)
}
