package main

import (
	"encoding/json"
	"log"
	"net/http"
	"time"
)

const (
	LivePollNum  = 5
	LivePollTime = 1000
)

var live State

type WatchRequest struct {
	Key    string
	Target File
}

type WatchResponse struct {
	Target File
	Locked bool
}

func Watch(w http.ResponseWriter, r *http.Request) {
	var watchReq WatchRequest
	err := json.NewDecoder(r.Body).Decode(&watchReq)
	if err != nil {
		log.Fatal(err)
	}

	resp := WatchPoll(watchReq)

	js, err := json.Marshal(resp)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
}

func WatchPoll(watchReq WatchRequest) WatchResponse {
	var target File
	var locked bool

	for i := 0; i < LivePollNum; i++ {
		target, locked := live.Files[watchReq.Key]

		if locked && target.Hash != watchReq.Target.Hash {
			break
		} else {
			time.Sleep(LivePollTime * time.Millisecond)
		}
	}

	return WatchResponse{Target: target, Locked: locked}
}

type PushRequest struct {
	Updated File
	Locked  bool
}

func Push(w http.ResponseWriter, r *http.Request) {

}

type UnlockRequest struct {
	Key string
}

func Unlock(w http.ResponseWriter, r *http.Request) {
	var unlockReq UnlockRequest
	err := json.NewDecoder(r.Body).Decode(&unlockReq)
	if err != nil {
		log.Fatal(err)
	}
	_, locked := live.Files[unlockReq.Key]
	if !locked {
		log.Fatal("trying to unlock a file that is already unlocked")
	}
	delete(live.Files, unlockReq.Key)

	w.Write([]byte(""))
}
