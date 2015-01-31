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

func test(w http.ResponseWriter, r *http.Request) {
	w.Write([]byte(`{"key": "foo"}`))
}

func main() {
	static = State{Files: make(map[string]File)}
	http.HandleFunc("/sync", Sync)

	live = State{Files: make(map[string]File)}
	http.HandleFunc("/watch", Watch)
	http.HandleFunc("/push", Push)
	http.HandleFunc("/unlock", Unlock)

	http.HandleFunc("/test", test) // TESTING FUNCTION FOR MICHAEL

	http.ListenAndServe(":8000", nil)
}
