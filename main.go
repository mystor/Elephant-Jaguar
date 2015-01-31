package main

import (
	"net/http"
)

type State struct {
	Files map[string]File
}

type File struct {
	Data string
	Hash string
}

func main() {
	static = State{Files: make(map[string]File)}
	http.HandleFunc("/sync", Sync)

	live = State{Files: make(map[string]File)}

	http.ListenAndServe(":8000", nil)
}
