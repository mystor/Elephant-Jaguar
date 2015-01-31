package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"
)

const (
	PollNum  = 5
	PollTime = 1000
)

var st State

type State struct {
	Files map[string]File
}

type File struct {
	Data string
	Hash string
}

type SyncRequest struct {
	Added   map[string]File
	Changed map[string]File
	Unmod   map[string]File
}

func NewSyncRequest() *SyncRequest {
	added := make(map[string]File)
	changed := make(map[string]File)
	unmod := make(map[string]File)
	return &SyncRequest{Changed: changed, Unmod: unmod, Added: added}
}

type SyncResponse struct {
	Update map[string]File
	Delete []string
}

func sync(w http.ResponseWriter, r *http.Request) {
	syncReq := NewSyncRequest()
	err := json.NewDecoder(r.Body).Decode(&syncReq)
	if err != nil {
		log.Fatal(err)
	}

	update, del := poll(syncReq)

	resp := SyncResponse{Update: update, Delete: del}
	js, err := json.Marshal(resp)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	fmt.Println(st)

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func poll(syncReq *SyncRequest) (map[string]File, []string) {
	update := make(map[string]File)
	del := make([]string, 0)

	for i := 0; i < PollNum; i++ {
		for key, clientFile := range syncReq.Added {
			serverFile, prs := st.Files[key]
			if !prs {
				st.Files[key] = clientFile
			} else {
				update[key] = serverFile
			}
		}

		for key, clientFile := range syncReq.Changed {
			st.Files[key] = clientFile
		}

		for key, clientFile := range syncReq.Unmod {
			serverFile, prs := st.Files[key]
			if prs && serverFile.Hash != clientFile.Hash {
				update[key] = serverFile
			} else {
				del = append(del, key)
			}
		}

		if len(update) > 0 || len(del) > 0 {
			break
		} else {
			time.Sleep(PollTime * time.Millisecond)
		}
	}

	return update, del
}

func main() {
	st = State{Files: make(map[string]File)}
	http.HandleFunc("/sync", sync)
	http.ListenAndServe(":8000", nil)
}
