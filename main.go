package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"time"
)

var st state

type state struct {
	files map[string]file
}

type file struct {
	Data     string
	Modified time.Time
}

type response struct {
	Updates  map[string]file
	Requests []string
}

func sync(w http.ResponseWriter, r *http.Request) {
	files := make(map[string]file)
	json.NewDecoder(r.Body).Decode(files)

	updates, requests := poll(files)

	resp := response{Updates: updates, Requests: requests}
	js, err := json.Marshal(resp)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	fmt.Println(resp)

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func poll(files map[string]file) (map[string]file, []string) {
	updates := make(map[string]file)
	requests := make([]string, 0)

	for i := 0; i < 5; i++ {
		for key, serverFile := range st.files {
			clientFile, prs := files[key]
			if !prs {
				clientFile = file{Data: "", Modified: time.Unix(0, 0)}
			}

			if serverFile.Modified.After(clientFile.Modified) {
				updates[key] = serverFile
			} else if serverFile.Modified.Before(clientFile.Modified) {
				requests = append(requests, key)
			}
		}

		for key, _ := range files {
			_, prs := st.files[key]
			if !prs {
				requests = append(requests, key)
			}
		}

		if len(updates) > 0 || len(requests) > 0 {
			break
		} else {
			time.Sleep(1 * time.Second)
		}
	}

	return updates, requests
}

func main() {
	st = state{files: make(map[string]file)}
	//http.HandleFunc(`/save/{rest:[a-zA-Z0-9=\-\/\.]+}`, saved)
	http.HandleFunc("/sync", sync)
	http.ListenAndServe(":8000", nil)
}
