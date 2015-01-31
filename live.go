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
	Key     string
	Updated File
	Locked  bool
}

type PushResponse struct {
	Locked bool
}

func Push(w http.ResponseWriter, r *http.Request) {
	var pushReq PushRequest
	err := json.NewDecoder(r.Body).Decode(&pushReq)
	if err != nil {
		log.Fatal(err)
	}

	var lockPrivs bool

	_, locked := live.Files[pushReq.Key]
	if locked == pushReq.Locked {
		lockPrivs = true
		live.Files[pushReq.Key] = pushReq.Updated
	} else if locked && !pushReq.Locked {
		lockPrivs = false
	} else {
		log.Fatal("error: one of you is lying about who locked the file")
	}

	resp := PushResponse{Locked: lockPrivs}

	js, err := json.Marshal(resp)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Write(js)
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
		log.Fatal("error: trying to unlock a file that is already unlocked") // SHOULD BE HANDLED BY RESPONSE
	}
	delete(live.Files, unlockReq.Key)

	w.Write([]byte(""))
}
