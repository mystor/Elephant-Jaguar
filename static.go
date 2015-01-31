package main

import (
	"encoding/json"
	"log"
	"net/http"
	"time"
)

const (
	StaticPollNum  = 5
	StaticPollTime = 1000
)

var static State

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

func Sync(w http.ResponseWriter, r *http.Request) {
	syncReq := NewSyncRequest()
	err := json.NewDecoder(r.Body).Decode(&syncReq)
	if err != nil {
		log.Fatal(err)
	}

	resp := SyncPoll(syncReq)

	js, err := json.Marshal(resp)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func SyncPoll(syncReq *SyncRequest) SyncResponse {
	update := make(map[string]File)
	del := make([]string, 0)

	for key, clientFile := range syncReq.Added {
		serverFile, prs := static.Files[key]
		if !prs {
			static.Files[key] = clientFile
		} else {
			update[key] = serverFile
		}
	}

	for key, clientFile := range syncReq.Changed {
		static.Files[key] = clientFile
	}

	for i := 0; i < StaticPollNum; i++ {
		for key, clientFile := range syncReq.Unmod {
			serverFile, prs := static.Files[key]
			if prs && serverFile.Hash != clientFile.Hash {
				update[key] = serverFile
			} else {
				del = append(del, key)
			}
		}

		if len(update) > 0 || len(del) > 0 {
			break
		} else {
			time.Sleep(StaticPollTime * time.Millisecond)
		}
	}

	return SyncResponse{Update: update, Delete: del}
}
