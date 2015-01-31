package main

import (
	"encoding/json"
	"net/http"
)

type State struct {
	Files map[string]File
}

type File struct {
	Data string
	Hash string
}

func statictest(w http.ResponseWriter, r *http.Request) {
	js, err := json.Marshal(static)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func livetest(w http.ResponseWriter, r *http.Request) {
	js, err := json.Marshal(live)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func main() {
	static = State{Files: make(map[string]File)}
	http.HandleFunc("/sync", Sync)
	http.HandleFunc("/read", Read)

	live = State{Files: make(map[string]File)}
	http.HandleFunc("/watch", Watch)
	http.HandleFunc("/push", Push)
	http.HandleFunc("/unlock", Unlock)

	http.HandleFunc("/static", statictest)
	http.HandleFunc("/live", livetest)

	http.ListenAndServe(":8000", nil)
}
